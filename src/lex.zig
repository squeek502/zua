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

// Debug/test output
const dumpTokensDuringTests = true;
const veryVerboseLexing = false;

// TODO: implement or ignore this (options for handling nesting of [[]] in multiline strings)
// for now we simply allow [[ (Lua 5.1 errors by default on [[ saying that nesting is deprecated)
const LUA_COMPAT_LSTR = 0;

pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,

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
        Keyword.init("elseIf", .Keyword_elseIf),
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
        Keyword_elseIf,
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

    pub fn nameForDisplay(id: Id) []const u8 {
        return switch (id) {
            .Keyword_and,
            .Keyword_break,
            .Keyword_do,
            .Keyword_else,
            .Keyword_elseIf,
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
            => keywords[@enumToInt(id)].name,
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
            .SingleChar => "<char>",
        };
    }
};

pub const LexError = error{
    UnfinishedString,
    UnfinishedLongComment,
    UnfinishedLongString,
    InvalidLongStringDelimiter,
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
        std.debug.warn("{} {} \"{}\"\n", .{ @tagName(token.id), Token.nameForDisplay(token.id), self.buffer[token.start..token.end] });
    }

    const State = enum {
        Start,
        Identifier,
        StringLiteral,
        StringLiteralBackslash,
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
        };
        var state = State.Start;
        var string_literal_delim: u8 = undefined;
        var long_string_sep_count: u32 = 0;
        var expected_long_string_sep_count: u32 = 0;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            if (veryVerboseLexing) std.debug.warn(":{}", .{@tagName(state)});
            switch (state) {
                State.Start => switch (c) {
                    '\n', '\r' => {
                        result.start = self.index + 1;
                    },
                    ' ', '\t' => {
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
                    '"', '\'' => {
                        state = State.StringLiteral;
                        string_literal_delim = c;
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
                        expected_long_string_sep_count = 0;
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
                    },
                    '"', '\'' => {
                        if (c == string_literal_delim) {
                            self.index += 1;
                            break;
                        }
                    },
                    '\n', '\r' => return LexError.UnfinishedString,
                    else => {},
                },
                State.StringLiteralBackslash => switch (c) {
                    '\n', '\r' => return LexError.UnfinishedString,
                    else => {
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
                        expected_long_string_sep_count = 0;
                    },
                    else => {
                        state = State.ShortComment;
                    },
                },
                State.LongStringStart,
                State.LongCommentStart,
                => switch (c) {
                    '=' => {
                        expected_long_string_sep_count += 1;
                    },
                    '[' => {
                        state = if (state == State.LongCommentStart) State.LongComment else State.LongString;
                    },
                    ']' => {
                        // Lua makes the pattern [=], [==], etc an explicit
                        // 'invalid long string delimiter' error instead of discarding
                        // its long-string-ness and parsing the tokens as normal
                        //
                        // - This is only true of long strings: long comments handle --[==] just fine
                        //   since it falls back to -- (short comment)
                        // - The end of long strings is unaffected: [=[str]=[ does not give this error
                        //   (instead the string will just not be finished)
                        // - Long strings with no sep chars is unaffected: [] does not give this error
                        //   (instead it will an give unexpected symbol error while parsing)
                        if (state == State.LongStringStart) {
                            if (expected_long_string_sep_count > 0) {
                                return LexError.InvalidLongStringDelimiter;
                            } else {
                                result.id = Token.Id.SingleChar;
                                break;
                            }
                        } else {
                            state = State.ShortComment;
                        }
                    },
                    else => {
                        if (state == State.LongCommentStart) {
                            state = State.ShortComment;
                        } else {
                            // fall back to a single [ token since there's no
                            // way this could be a valid multiline string start anymore
                            result.id = Token.Id.SingleChar;
                            // set the length to 1 since we could be at [=== for example
                            self.index = result.start + 1;
                            break;
                        }
                    },
                },
                State.LongString,
                State.LongComment,
                => switch (c) {
                    ']' => {
                        state = if (state == State.LongComment) State.LongCommentPossibleEnd else State.LongStringPossibleEnd;
                        long_string_sep_count = 0;
                    },
                    else => {},
                },
                State.LongStringPossibleEnd,
                State.LongCommentPossibleEnd,
                => switch (c) {
                    ']' => {
                        if (long_string_sep_count == expected_long_string_sep_count) {
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
                        long_string_sep_count += 1;
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
                    },
                    else => {
                        result.id = Token.Id.SingleChar;
                        break;
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
                        result.id = Token.Id.Concat;
                        break;
                    },
                },
                State.Number => switch (c) {
                    // TODO: proper handling, this is a placeholder
                    '0'...'9' => {},
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
                State.Number => {
                    result.id = Token.Id.Number;
                },
                State.CommentStart,
                State.ShortComment,
                State.LongCommentStart,
                => {
                    result.start = self.index;
                },
                State.LongStringStart,
                => {
                    // fall back to a single [ token since there's no
                    // way this could be a valid multiline string start anymore
                    result.id = Token.Id.SingleChar;
                    // set the length to 1 since we could be at [=== for example
                    self.index = result.start + 1;
                },
                State.LongCommentPossibleEnd,
                State.LongComment,
                => return LexError.UnfinishedLongComment,
                State.LongStringPossibleEnd,
                State.LongString,
                => return LexError.UnfinishedLongString,
                State.StringLiteral,
                State.StringLiteralBackslash,
                => return LexError.UnfinishedString,
            }
        }

        if (veryVerboseLexing) {
            if (self.index < self.buffer.len) {
                std.debug.warn(":{}:'{c}'=\"{}\"\n", .{ self.index, self.buffer[self.index], self.buffer[result.start..self.index] });
            } else {
                std.debug.warn(":eof=\"{}\"\n", .{self.buffer[result.start..self.index]});
            }
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

test "long strings" {
    try testLex("[[]]", &[_]Token.Id{Token.Id.String});
    try testLex("[===[\nhello\nworld\n]===]", &[_]Token.Id{Token.Id.String});
    try testLex("[==", &[_]Token.Id{Token.Id.SingleChar, Token.Id.EQ});
    try testLex("[]", &[_]Token.Id{Token.Id.SingleChar, Token.Id.SingleChar});
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
    try testLex("--[[local hello = 'wor\\'ld']]", &[_]Token.Id{});
    try testLex("--[==[\nlocal\nhello\n=\n'world'\n]==]", &[_]Token.Id{});
    try testLex("--[==", &[_]Token.Id{});
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

test "LexError.InvalidLongStringDelimiter" {
    // see comment in Lexer.next near the return of LexError.InvalidLongStringDelimiter
    const simple = testLex("[==]", &[_]Token.Id{Token.Id.String});
    std.testing.expectError(LexError.InvalidLongStringDelimiter, simple);
}

test "LexError.UnfinishedLongComment" {
    const simple = testLex("--[[", &[_]Token.Id{});
    std.testing.expectError(LexError.UnfinishedLongComment, simple);

    const mismatchedSep = testLex("--[==[ ]=]", &[_]Token.Id{});
    std.testing.expectError(LexError.UnfinishedLongComment, mismatchedSep);
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
