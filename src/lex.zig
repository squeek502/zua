const std = @import("std");

// Notes:
//
// This implementation is currently based mostly on Zig's tokenizer, not Lua's lexer.
// As such, how/when errors are caught/handled is not going to be 1:1.
//
// In Lua's lexer, all single char tokens use their own ASCII value as their ID, and
// every other multi-character token uses ID >= 257 (see FIRST_RESERVED in llex.h).
// For now, this implementation uses a 'SingleChar' token as a catch-all for
// such single char tokens
//
// Lua's lexer uses a lua_State and parses strings/numbers while lexing, allocating
// strings and adding them to the lua_State's string table. This lexer, instead,
// does no allocation or parsing of strings/numbers (that will be done later).
// TODO: is this too big of a deviation?
//
// Related to the previous paragraph, Zig's tokenizer cannot fail. Lua's lexer can fail
// at multiple points (both when parsing strings/numbers and on general syntax errors).
// TODO: decide if not failing could/should be a property of this implementation;
//       if it can fail then maybe avoiding allocating doesn't make as much sense?
//
// Lua's lexer skips over all comments (doesn't store them as tokens). This functionality is
// kept in this implementation.
//
// Lua's number lexing allows for using locale-specific decimal points
// TODO: ?

// Debug/test output
const dumpTokensDuringTests = true;
const veryVerboseLexing = false;

// In Lua 5.1 there is a bug in the lexer where check_next() accepts \0
// since \0 is technically in the string literal passed to check_next representing
// the set of characters to check for (since the string is null-terminated)
//
// This affects a few things:
//  ".\0" gets lexed as ..
//  ".\0\0" and "..\0" get lexed as ...
//  the e/E in numeral exponents can be '\0'
//  the +- sign for numeral exponents can be '\0'
//
// Note that these last two affect how numbers are ultimately parsed, since something like
// 1\0 gets lexed as the equiv of 1e but gets parsed into 1 (since the str2d func treats it as
// a null-terminated string). For this reason, "1\0\0" will succesfully parse into 1
// while "1e\0002" will fail to parse (since it will treat it as "1e").
pub const LUA_51_COMPAT_CHECK_NEXT_BUG = false;

// TODO: implement or ignore this (options for handling nesting of [[]] in multiline strings)
// for now we simply allow [[ (Lua 5.1 errors by default on [[ saying that nesting is deprecated)
const LUA_COMPAT_LSTR = 0;

var terribleTempString: [10]u8 = undefined;

pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,
    // for single-char tokens
    // TODO: figure out something better for this (it's only used for nameForDisplay)
    char: ?u8,

    pub const Keyword = struct {
        name: []const u8,
        id: Id,
        hash: u32,

        fn init(name: []const u8, id: Id) Keyword {
            @setEvalBranchQuota(2000);
            return .{
                .name = name,
                .id = id,
                .hash = std.hash_map.hashString(name),
            };
        }

        pub fn idFromName(name: []const u8) ?Id {
            var hash = std.hash_map.hashString(name);
            for (keywords) |keyword| {
                if (keyword.hash == hash and std.mem.eql(u8, keyword.name, name)) {
                    return keyword.id;
                }
            }
            return null;
        }
    };

    pub const keywords = [_]Keyword{
        Keyword.init("and", .Keyword_and),
        Keyword.init("break", .Keyword_break),
        Keyword.init("do", .Keyword_do),
        Keyword.init("else", .Keyword_else),
        Keyword.init("elseif", .Keyword_elseif),
        Keyword.init("end", .Keyword_end),
        Keyword.init("false", .Keyword_false),
        Keyword.init("for", .Keyword_for),
        Keyword.init("function", .Keyword_function),
        Keyword.init("if", .Keyword_if),
        Keyword.init("in", .Keyword_in),
        Keyword.init("local", .Keyword_local),
        Keyword.init("nil", .Keyword_nil),
        Keyword.init("not", .Keyword_not),
        Keyword.init("or", .Keyword_or),
        Keyword.init("repeat", .Keyword_repeat),
        Keyword.init("return", .Keyword_return),
        Keyword.init("then", .Keyword_then),
        Keyword.init("true", .Keyword_true),
        Keyword.init("until", .Keyword_until),
        Keyword.init("while", .Keyword_while),
    };

    pub const Id = enum {
        // terminal symbols denoted by reserved words
        Keyword_and,
        Keyword_break,
        Keyword_do,
        Keyword_else,
        Keyword_elseif,
        Keyword_end,
        Keyword_false,
        Keyword_for,
        Keyword_function,
        Keyword_if,
        Keyword_in,
        Keyword_local,
        Keyword_nil,
        Keyword_not,
        Keyword_or,
        Keyword_repeat,
        Keyword_return,
        Keyword_then,
        Keyword_true,
        Keyword_until,
        Keyword_while,
        // any normal byte
        SingleChar,
        // other terminal symbols
        Concat,
        Ellipsis,
        EQ,
        GE,
        LE,
        NE,
        Number,
        Name,
        String,
        Eof,
    };

    pub fn nameForDisplay(self: *const Token) []const u8 {
        return switch (self.id) {
            .Keyword_and,
            .Keyword_break,
            .Keyword_do,
            .Keyword_else,
            .Keyword_elseif,
            .Keyword_end,
            .Keyword_false,
            .Keyword_for,
            .Keyword_function,
            .Keyword_if,
            .Keyword_in,
            .Keyword_local,
            .Keyword_nil,
            .Keyword_not,
            .Keyword_or,
            .Keyword_repeat,
            .Keyword_return,
            .Keyword_then,
            .Keyword_true,
            .Keyword_until,
            .Keyword_while,
            // FIXME: This relies on the keywords array and Id enum to be in the exact same
            // order which isnt ideal
            => keywords[@enumToInt(self.id)].name,
            .Concat => "..",
            .Ellipsis => "...",
            .EQ => "==",
            .GE => ">=",
            .LE => "<=",
            .NE => "~=",
            .Number => "<number>",
            .Name => "<name>",
            .String => "<string>",
            .Eof => "<eof>",
            .SingleChar => blk: {
                // TODO: make all of this less horrible
                if (std.ascii.isCntrl(self.char.?)) {
                    var terribleTempSliceStream = std.io.SliceOutStream.init(terribleTempString[0..]);
                    const stream = &terribleTempSliceStream.stream;
                    stream.print("char({d})", .{self.char.?}) catch unreachable;
                    break :blk terribleTempSliceStream.getWritten();
                } else {
                    break :blk @as(*const [1]u8, &self.char.?)[0..1];
                }
            },
        };
    }
};

pub const LexError = error{
    UnfinishedString,
    UnfinishedLongComment,
    UnfinishedLongString,
    InvalidLongStringDelimiter,
    MalformedNumber,
    EscapeSequenceTooLarge,
};

pub const Lexer = struct {
    buffer: []const u8,
    index: usize,

    pub fn init(buffer: []const u8) Lexer {
        return Lexer{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn dump(self: *Lexer, token: *const Token) void {
        std.debug.warn("{} {} \"{}\"\n", .{ @tagName(token.id), token.nameForDisplay(), self.buffer[token.start..token.end] });
    }

    const State = enum {
        Start,
        Identifier,
        StringLiteral,
        StringLiteralBackslash,
        StringLiteralBackslashLineEndings,
        Dash,
        Dot,
        Concat,
        CommentStart,
        ShortComment,
        LongCommentStart,
        LongComment,
        LongCommentPossibleEnd,
        LongStringStart,
        LongString,
        LongStringPossibleEnd,
        Number,
        NumberExponentStart,
        NumberExponent,
        NumberHexStart,
        NumberHex,
        CompoundEqual,
    };

    pub fn next(self: *Lexer) LexError!Token {
        const start_index = self.index;
        if (veryVerboseLexing) {
            if (self.index < self.buffer.len) {
                std.debug.warn("{}:'{c}'", .{ self.index, self.buffer[self.index] });
            } else {
                std.debug.warn("eof", .{});
            }
        }
        var result = Token{
            .id = Token.Id.Eof,
            .start = start_index,
            .end = undefined,
            .char = null,
        };
        var state = State.Start;
        var string_delim: u8 = undefined;
        var string_level: usize = 0;
        var expected_string_level: usize = 0;
        var string_escape_n: std.math.IntFittingRange(0, 999) = 0;
        var string_escape_i: std.math.IntFittingRange(0, 3) = 0;
        var string_escape_line_ending: u8 = undefined;
        var number_is_float: bool = false;
        var number_starting_char: u8 = undefined;
        var number_exponent_signed_char: ?u8 = null;
        var number_is_null_terminated: bool = false;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            if (veryVerboseLexing) std.debug.warn(":{}", .{@tagName(state)});
            switch (state) {
                State.Start => switch (c) {
                    '\n', '\r' => {
                        result.start = self.index + 1;
                    },
                    // space, tab, vertical tab, form feed
                    ' ', '\t', '\x0b', '\x0c' => {
                        // skip whitespace
                        result.start = self.index + 1;
                    },
                    '-' => {
                        // this could be the start of a comment, a long comment, or a single -
                        state = State.Dash;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        state = State.Identifier;
                        result.id = Token.Id.Name;
                    },
                    '0'...'9' => {
                        state = State.Number;
                        number_starting_char = c;
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG) {
                            number_is_null_terminated = false;
                        }
                    },
                    '"', '\'' => {
                        state = State.StringLiteral;
                        string_delim = c;
                        result.id = Token.Id.String;
                    },
                    '.' => {
                        // this could be the start of .., ..., or a single .
                        state = State.Dot;
                    },
                    '>', '<', '~', '=' => {
                        state = State.CompoundEqual;
                    },
                    '[' => {
                        state = State.LongStringStart;
                        expected_string_level = 0;
                    },
                    else => {
                        result.id = Token.Id.SingleChar;
                        self.index += 1;
                        break;
                    },
                },
                State.Identifier => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        const name = self.buffer[result.start..self.index];
                        if (Token.Keyword.idFromName(name)) |id| {
                            result.id = id;
                        }
                        break;
                    },
                },
                State.StringLiteral => switch (c) {
                    '\\' => {
                        state = State.StringLiteralBackslash;
                        string_escape_i = 0;
                        string_escape_n = 0;
                    },
                    '"', '\'' => {
                        if (c == string_delim) {
                            self.index += 1;
                            break;
                        }
                    },
                    '\n', '\r' => return LexError.UnfinishedString,
                    else => {},
                },
                State.StringLiteralBackslash => switch (c) {
                    '0'...'9' => {
                        // Validate that any \ddd escape sequences can actually fit
                        // in a byte
                        string_escape_n = 10 * string_escape_n + (c - '0');
                        string_escape_i += 1;
                        if (string_escape_i == 3) {
                            if (string_escape_n > std.math.maxInt(u8)) {
                                return LexError.EscapeSequenceTooLarge;
                            }
                            state = State.StringLiteral;
                        }
                    },
                    '\r', '\n' => {
                        if (string_escape_i > 0) {
                            return LexError.UnfinishedString;
                        }
                        state = State.StringLiteralBackslashLineEndings;
                        string_escape_line_ending = c;
                    },
                    else => {
                        // if the escape sequence had any digits, then
                        // we need to backtrack so as not to escape the current
                        // character (since the digits are the things being escaped)
                        if (string_escape_i > 0) {
                            self.index -= 1;
                        }
                        state = State.StringLiteral;
                    },
                },
                State.StringLiteralBackslashLineEndings => switch(c) {
                    '\r', '\n' => {
                        // can only escape \r\n or \n\r pairs, not \r\r or \n\n
                        if (c == string_escape_line_ending) {
                            return LexError.UnfinishedString;
                        } else {
                            state = State.StringLiteral;
                        }
                    },
                    else => {
                        // backtrack so that we don't escape the current char
                        self.index -= 1;
                        state = State.StringLiteral;
                    },
                },
                State.Dash => switch (c) {
                    '-' => {
                        state = State.CommentStart;
                    },
                    else => {
                        result.id = Token.Id.SingleChar;
                        break;
                    },
                },
                State.CommentStart => switch (c) {
                    '[' => {
                        state = State.LongCommentStart;
                        expected_string_level = 0;
                    },
                    '\r', '\n' => {
                        // comment immediately ends
                        result.start = self.index + 1;
                        state = State.Start;
                    },
                    else => {
                        state = State.ShortComment;
                    },
                },
                State.LongStringStart,
                State.LongCommentStart,
                => switch (c) {
                    '=' => {
                        expected_string_level += 1;
                    },
                    '[' => {
                        state = if (state == State.LongCommentStart) State.LongComment else State.LongString;
                    },
                    else => {
                        if (state == State.LongCommentStart) {
                            if (c == '\n' or c == '\r') {
                                // not a long comment, but the short comment ends immediately
                                result.start = self.index + 1;
                                state = State.Start;
                            } else {
                                state = State.ShortComment;
                            }
                        } else {
                            // Lua makes the pattern [=X where X is anything but [ or = an explicit
                            // 'invalid long string delimiter' error instead of discarding
                            // its long-string-ness and parsing the tokens as normal
                            //
                            // - This is only true of long strings: long comments handle --[==X just fine
                            //   since it falls back to -- (short comment)
                            // - The end of long strings is unaffected: [=[str]=X does not give this error
                            //   (instead the string will just not be finished)
                            // - Long strings with no sep chars is unaffected: [X does not give this error
                            //   (instead it will an give unexpected symbol error while parsing)
                            if (expected_string_level > 0) {
                                return LexError.InvalidLongStringDelimiter;
                            } else {
                                result.id = Token.Id.SingleChar;
                                break;
                            }
                        }
                    },
                },
                State.LongString,
                State.LongComment,
                => switch (c) {
                    ']' => {
                        state = if (state == State.LongComment) State.LongCommentPossibleEnd else State.LongStringPossibleEnd;
                        string_level = 0;
                    },
                    else => {},
                },
                State.LongStringPossibleEnd,
                State.LongCommentPossibleEnd,
                => switch (c) {
                    ']' => {
                        if (string_level == expected_string_level) {
                            if (state == State.LongCommentPossibleEnd) {
                                result.start = self.index + 1;
                                state = State.Start;
                            } else {
                                self.index += 1;
                                result.id = Token.Id.String;
                                break;
                            }
                        } else {
                            state = if (state == State.LongCommentPossibleEnd) State.LongComment else State.LongString;
                        }
                    },
                    '=' => {
                        string_level += 1;
                    },
                    else => {
                        state = if (state == State.LongCommentPossibleEnd) State.LongComment else State.LongString;
                    },
                },
                State.ShortComment => switch (c) {
                    '\n', '\r' => {
                        result.start = self.index + 1;
                        state = State.Start;
                    },
                    else => {},
                },
                State.Dot => switch (c) {
                    '.' => {
                        state = State.Concat;
                    },
                    '0'...'9' => {
                        state = State.Number;
                        number_starting_char = '.';
                        number_is_float = true;
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG) {
                            number_is_null_terminated = false;
                        }
                    },
                    else => {
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG and c == '\x00') {
                            state = State.Concat;
                        } else {
                            result.id = Token.Id.SingleChar;
                            break;
                        }
                    },
                },
                State.Concat => switch (c) {
                    '.' => {
                        result.id = Token.Id.Ellipsis;
                        // include this .
                        self.index += 1;
                        break;
                    },
                    else => {
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG and c == '\x00') {
                            result.id = Token.Id.Ellipsis;
                            // include this .
                            self.index += 1;
                            break;
                        } else {
                            result.id = Token.Id.Concat;
                            break;
                        }
                    },
                },
                State.Number => switch (c) {
                    '0'...'9' => {},
                    '.' => {
                        // multiple decimal points not allowed
                        if (number_is_float) {
                            return LexError.MalformedNumber;
                        }
                        number_is_float = true;
                    },
                    'x', 'X' => {
                        // only 0x is allowed
                        if (number_starting_char != '0') {
                            return LexError.MalformedNumber;
                        }
                        state = State.NumberHexStart;
                    },
                    'e', 'E' => {
                        state = State.NumberExponentStart;
                        number_exponent_signed_char = null;
                    },
                    // 'a'...'z' minus e and x
                    'a'...'d', 'A'...'D', 'f'...'w', 'F'...'W', 'y'...'z', 'Y'...'Z' => {
                        return LexError.MalformedNumber;
                    },
                    '_' => return LexError.MalformedNumber,
                    else => {
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG and c == '\x00') {
                            state = State.NumberExponentStart;
                            number_exponent_signed_char = null;
                            number_is_null_terminated = true;
                        } else {
                            result.id = Token.Id.Number;
                            break;
                        }
                    },
                },
                State.NumberHexStart, State.NumberHex => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        state = State.NumberHex;
                    },
                    'g'...'z', 'G'...'Z' => {
                        return LexError.MalformedNumber;
                    },
                    '_' => return LexError.MalformedNumber,
                    else => {
                        result.id = Token.Id.Number;
                        break;
                    },
                },
                State.NumberExponentStart => switch (c) {
                    '0'...'9' => state = State.NumberExponent,
                    '-', '+' => {
                        if (number_exponent_signed_char) |_| {
                            return LexError.MalformedNumber;
                        }
                        number_exponent_signed_char = c;
                    },
                    '_' => return LexError.MalformedNumber,
                    else => {
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG) {
                            if (number_is_null_terminated) {
                                switch (c) {
                                    // since the number is null-terminated already,
                                    // absolutely any consumable char is fine
                                    '\x00', 'a'...'z', 'A'...'Z', '_' => {
                                        state = State.NumberExponent;
                                    },
                                    else => {
                                        result.id = Token.Id.Number;
                                        break;
                                    },
                                }
                            } else {
                                return LexError.MalformedNumber;
                            }
                        } else {
                            return LexError.MalformedNumber;
                        }
                    },
                },
                State.NumberExponent => switch (c) {
                    '0'...'9' => {},
                    'a'...'z', 'A'...'Z' => {
                        if (LUA_51_COMPAT_CHECK_NEXT_BUG and number_is_null_terminated) {} else {
                            return LexError.MalformedNumber;
                        }
                    },
                    '_' => return LexError.MalformedNumber,
                    else => {
                        result.id = Token.Id.Number;
                        break;
                    },
                },
                State.CompoundEqual => switch (c) {
                    '=' => {
                        switch (self.buffer[self.index - 1]) {
                            '>' => result.id = Token.Id.GE,
                            '<' => result.id = Token.Id.LE,
                            '~' => result.id = Token.Id.NE,
                            '=' => result.id = Token.Id.EQ,
                            else => unreachable,
                        }
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.id = Token.Id.SingleChar;
                        break;
                    },
                },
            }
        } else {
            // this will always be true due to the while loop condition
            // as the else block is only evaluated after a break; in the while loop
            std.debug.assert(self.index == self.buffer.len);
            switch (state) {
                State.Start => {},
                State.Identifier => {
                    const name = self.buffer[result.start..self.index];
                    if (Token.Keyword.idFromName(name)) |id| {
                        result.id = id;
                    }
                },
                State.Dot,
                State.Dash,
                State.CompoundEqual,
                => {
                    result.id = Token.Id.SingleChar;
                },
                State.Concat => {
                    result.id = Token.Id.Concat;
                },
                State.NumberExponent,
                State.NumberHex,
                State.Number,
                => {
                    result.id = Token.Id.Number;
                },
                State.CommentStart,
                State.ShortComment,
                State.LongCommentStart,
                => {
                    result.start = self.index;
                },
                State.LongStringStart => {
                    if (expected_string_level > 0) {
                        return LexError.InvalidLongStringDelimiter;
                    } else {
                        result.id = Token.Id.SingleChar;
                    }
                },
                State.LongCommentPossibleEnd,
                State.LongComment,
                => return LexError.UnfinishedLongComment,
                State.LongStringPossibleEnd,
                State.LongString,
                => return LexError.UnfinishedLongString,
                State.StringLiteral,
                State.StringLiteralBackslash,
                State.StringLiteralBackslashLineEndings,
                => return LexError.UnfinishedString,
                State.NumberHexStart,
                State.NumberExponentStart,
                => {
                    if (LUA_51_COMPAT_CHECK_NEXT_BUG and number_is_null_terminated) {
                        result.id = Token.Id.Number;
                    } else {
                        return LexError.MalformedNumber;
                    }
                },
            }
        }

        if (veryVerboseLexing) {
            if (self.index < self.buffer.len) {
                std.debug.warn(":{}:'{c}'=\"{}\"\n", .{ self.index, self.buffer[self.index], self.buffer[result.start..self.index] });
            } else {
                std.debug.warn(":eof=\"{}\"\n", .{self.buffer[result.start..self.index]});
            }
        }

        if (result.id == Token.Id.SingleChar) {
            result.char = self.buffer[result.start];
        }

        result.end = self.index;
        return result;
    }

    pub fn lookahead(self: *Lexer) Token {
        const lookaheadLexer = Lexer{
            .buffer = self.buffer,
            .index = self.index,
        };
        return lookaheadLexer.next();
    }
};

test "hello \"world\"" {
    try testLex("local hello = \"wor\\\"ld\"", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
}

test "hello 'world'" {
    try testLex("local hello = 'wor\\'ld'", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
}

test "strings" {
    // none of these escaped chars have any meaning, but Lua allows
    // any character to be escaped so this should lex just fine
    try testLex("'\\e\\s\\c\\ any char'", &[_]Token.Id{Token.Id.String});
    try testLex("'\\1'", &[_]Token.Id{Token.Id.String});
    try testLex("'\\12'", &[_]Token.Id{Token.Id.String});
    try testLex("'\\123'", &[_]Token.Id{Token.Id.String});
    try testLex("'\\1234'", &[_]Token.Id{Token.Id.String});
    // carriage returns and newlines can be escaped with \
    try testLex("'\\\n\\\r'", &[_]Token.Id{Token.Id.String});
    try testLex("\".\\\x0d\\\\\\\".\\\x0d\xa5[\\ArA\"", &[_]Token.Id{Token.Id.String});
    // a pair of CR/LF can be escaped with a single \ (either CRLF or LFCR)
    try testLex("'\\\r\n'", &[_]Token.Id{Token.Id.String});
    try testLex("'\\\n\r'", &[_]Token.Id{Token.Id.String});
}

test "long strings" {
    try testLex("[[]]", &[_]Token.Id{Token.Id.String});
    try testLex("[===[\nhello\nworld\n]===]", &[_]Token.Id{Token.Id.String});
    try testLex("[]", &[_]Token.Id{ Token.Id.SingleChar, Token.Id.SingleChar });
    // TODO: this depends on LUA_COMPAT_LSTR
    try testLex("[[ [[ ]]", &[_]Token.Id{Token.Id.String});
    // this is always allowed
    try testLex("[=[ [[ ]] ]=]", &[_]Token.Id{Token.Id.String});
}

test "comments and dashes" {
    try testLex("-", &[_]Token.Id{Token.Id.SingleChar});
    try testLex("a-b", &[_]Token.Id{ Token.Id.Name, Token.Id.SingleChar, Token.Id.Name });
    try testLex("--", &[_]Token.Id{});
    try testLex("--local hello = 'wor\\'ld'", &[_]Token.Id{});
    try testLex("--[this is a short comment\nreturn", &[_]Token.Id{Token.Id.Keyword_return});
    try testLex("--\rreturn", &[_]Token.Id{Token.Id.Keyword_return});
    try testLex("--[[local hello = 'wor\\'ld']]", &[_]Token.Id{});
    try testLex("--[==[\nlocal\nhello\n=\n'world'\n]==]", &[_]Token.Id{});
    try testLex("--[==", &[_]Token.Id{});
    try testLex("--[\n]]", &[_]Token.Id{Token.Id.SingleChar, Token.Id.SingleChar});
}

test "whitespace" {
    // form feed
    try testLex("_\x0c_W_", &[_]Token.Id{Token.Id.Name, Token.Id.Name});
    // vertical tab
    try testLex("_\x0b_W_", &[_]Token.Id{Token.Id.Name, Token.Id.Name});
}

test "dots, concat, ellipsis" {
    try testLex(".", &[_]Token.Id{Token.Id.SingleChar});
    try testLex("a.b", &[_]Token.Id{ Token.Id.Name, Token.Id.SingleChar, Token.Id.Name });
    try testLex("..", &[_]Token.Id{Token.Id.Concat});
    try testLex("a..b.c", &[_]Token.Id{
        Token.Id.Name,
        Token.Id.Concat,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.Name,
    });
    // this is valid Lua, apparently (abc will be true, test will be the first value in ...)
    try testLex("test=...abc=true", &[_]Token.Id{
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.Ellipsis,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.Keyword_true,
    });
}

test "= and compound = operators" {
    try testLex("=", &[_]Token.Id{Token.Id.SingleChar});
    try testLex("a=b", &[_]Token.Id{ Token.Id.Name, Token.Id.SingleChar, Token.Id.Name });
    try testLex("a==b", &[_]Token.Id{ Token.Id.Name, Token.Id.EQ, Token.Id.Name });
    try testLex(">=", &[_]Token.Id{Token.Id.GE});
    try testLex("if a~=b and a<=b and b<a then end", &[_]Token.Id{
        Token.Id.Keyword_if,
        Token.Id.Name,
        Token.Id.NE,
        Token.Id.Name,
        Token.Id.Keyword_and,
        Token.Id.Name,
        Token.Id.LE,
        Token.Id.Name,
        Token.Id.Keyword_and,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.Name,
        Token.Id.Keyword_then,
        Token.Id.Keyword_end,
    });
}

test "numbers" {
    // from the Lua 5.1 manual
    try testLex("3", &[_]Token.Id{Token.Id.Number});
    try testLex("3.0", &[_]Token.Id{Token.Id.Number});
    try testLex("3.1416", &[_]Token.Id{Token.Id.Number});
    try testLex("314.16e-2", &[_]Token.Id{Token.Id.Number});
    try testLex("0.31416E1", &[_]Token.Id{Token.Id.Number});
    try testLex("0xff", &[_]Token.Id{Token.Id.Number});
    try testLex("0x56", &[_]Token.Id{Token.Id.Number});

    // other cases
    try testLex(".1", &[_]Token.Id{Token.Id.Number});
    try testLex("0xFF", &[_]Token.Id{Token.Id.Number});
    try testLex("0XeF", &[_]Token.Id{Token.Id.Number});
    try testLex("1e+3", &[_]Token.Id{Token.Id.Number});
    // 3e2 and .52 should lex as separate tokens
    try testLex("3e2.52", &[_]Token.Id{ Token.Id.Number, Token.Id.Number });
}

test "LexError.MalformedNumber" {
    expectLexError(LexError.MalformedNumber, testLex("1e", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0z", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0x", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0xabcz", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("1xabc", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1.e2", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1.", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1.2", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e3a", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e-", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e-a", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e+", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e--2", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e-)2", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0.1e+-2", &[_]Token.Id{Token.Id.Number}));
    // Lua's lexer weirdly 'allows'/consumes _ when lexing numbers (see llex.c:201 in 5.1.5),
    // but as far as I can tell there are no valid ways to define a number with a _ in it.
    // Either way, we should fail with MalformedNumber in the same ways that Lua does,
    // so we need to handle _ similarly to the Lua lexer.
    expectLexError(LexError.MalformedNumber, testLex("1_2", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0x2__", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("0x__", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("1e__", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex("1e-1_", &[_]Token.Id{Token.Id.Number}));
    expectLexError(LexError.MalformedNumber, testLex(".1_", &[_]Token.Id{Token.Id.Number}));
}

test "LexError.InvalidLongStringDelimiter" {
    // see comment in Lexer.next near the return of LexError.InvalidLongStringDelimiter
    const simple = testLex("[==]", &[_]Token.Id{Token.Id.String});
    expectLexError(LexError.InvalidLongStringDelimiter, simple);

    const number = testLex("[=======4", &[_]Token.Id{Token.Id.String});
    expectLexError(LexError.InvalidLongStringDelimiter, number);

    const eof = testLex("[==", &[_]Token.Id{ Token.Id.String });
    expectLexError(LexError.InvalidLongStringDelimiter, eof);
}

test "LexError.EscapeSequenceTooLarge" {
    expectLexError(LexError.EscapeSequenceTooLarge, testLex("'\\256'", &[_]Token.Id{Token.Id.String}));
}

test "LexError.UnfinishedLongComment" {
    const simple = testLex("--[[", &[_]Token.Id{});
    expectLexError(LexError.UnfinishedLongComment, simple);

    const mismatchedSep = testLex("--[==[ ]=]", &[_]Token.Id{});
    expectLexError(LexError.UnfinishedLongComment, mismatchedSep);
}

test "LexError.UnfinishedString" {
    const missingQuoteResult = testLex("local hello = \"wor\\\"ld", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
    expectLexError(LexError.UnfinishedString, missingQuoteResult);

    const newlineResult = testLex("local hello = \"wor\\\"ld\n\"", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
    expectLexError(LexError.UnfinishedString, newlineResult);
}

test "5.1 check_next bug compat" {
    if (!LUA_51_COMPAT_CHECK_NEXT_BUG) return;

    try testLex(".\x00", &[_]Token.Id{Token.Id.Concat});
    try testLex(".\x00\x00", &[_]Token.Id{Token.Id.Ellipsis});
    try testLex("..\x00", &[_]Token.Id{Token.Id.Ellipsis});
    try testLex("1\x00", &[_]Token.Id{Token.Id.Number});
    try testLex("1\x00-5", &[_]Token.Id{Token.Id.Number});
    try testLex("1\x00\x005", &[_]Token.Id{Token.Id.Number});
    try testLex("1\x00\x00anythingcangoherenow", &[_]Token.Id{Token.Id.Number});
    try testLex(".0\x00", &[_]Token.Id{Token.Id.Number});
    try testLex(".0\x00)", &[_]Token.Id{ Token.Id.Number, Token.Id.SingleChar });
    // should lex as: 5\x00z5 ; \x00 ; 9\x00\x00 ; \x00
    try testLex("5\x00z5\x009\x00\x00\x00", &[_]Token.Id{ 
        Token.Id.Number,
        Token.Id.SingleChar,
        Token.Id.Number,
        Token.Id.SingleChar,
    });
    expectLexError(LexError.MalformedNumber, testLex("1e\x005", &[_]Token.Id{Token.Id.Number}));
}

fn expectLexError(expected: LexError, actual: var) void {
    if (veryVerboseLexing) std.debug.warn("\n", .{});
    std.testing.expectError(expected, actual);
    if (dumpTokensDuringTests) std.debug.warn("{}\n", .{actual});
}

fn testLex(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source);
    if (dumpTokensDuringTests) std.debug.warn("\n----------------------\n{}\n----------------------\n", .{source});
    for (expected_tokens) |expected_token_id| {
        const token = try lexer.next();
        if (dumpTokensDuringTests) lexer.dump(&token);
        std.testing.expectEqual(expected_token_id, token.id);
    }
    const last_token = try lexer.next();
    std.testing.expectEqual(Token.Id.Eof, last_token.id);
}
