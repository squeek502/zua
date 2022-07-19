const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoComptimeLookup = @import("comptime_lookup.zig").AutoComptimeLookup;
const zua = @import("zua.zig");

// Notes:
//
// In Lua's lexer, all single char tokens use their own ASCII value as their ID, and
// every other multi-character token uses ID >= 257 (see FIRST_RESERVED in llex.h).
// For now, this implementation uses a 'single_char' token as a catch-all for
// such single char tokens
//
// Lua's lexer uses a lua_State and parses strings/numbers while lexing, allocating
// strings and adding them to the lua_State's string table. This lexer, instead,
// does no allocation or parsing of strings/numbers (that will be done later). However,
// it maintains 1:1 compatibility with when Lua's lexer errors by validating
// strings/numbers at lex-time in the same way that Lua's lexer parses them.
//
// Lua's lexer skips over all comments (doesn't store them as tokens). This functionality is
// kept in this implementation.
//
// Lua's number lexing allows for using locale-specific decimal points
// TODO: ?
//
// Zua's lexer error messages are almost entirely identical to Lua's, except in a few cases:
// - malformed number in Lua prints more than where a number stops being valid, whereas Zua
//   only prints up to the first malformed char (e.g. Zua will stop at 3.. whereas Lua will
//   print 3.................)
// - string token context in Lua prints the parsed string rather than the source of the string
//   since Lua parses and lexes at the same time. Zua will print the source of the string instead.
//   (e.g. for "\v Zua will print "\v whereas Lua would print an actual vertical tab character)
// - escape sequence too large in Lua doesn't include the actual escape sequence that is too large,
//   while Zua does include it (e.g. "\444" in Lua would error with: near '"', while Zua gives:
//   near '"\444')

// Debug/test output
const dumpTokensDuringTests = false;
const veryVerboseLexing = false;

pub const Token = struct {
    id: Id,
    start: usize,
    end: usize,
    line_number: usize,
    // for single-char tokens
    // TODO: figure out something better for this (it's only used for nameForDisplay)
    char: ?u8,

    /// Helper for .single_char tokens
    pub fn isChar(self: *const Token, expected_char: u8) bool {
        return self.id == .single_char and self.char.? == expected_char;
    }

    pub const Keyword = struct {
        pub fn idFromName(name: []const u8) ?Id {
            return keywords.get(name);
        }
    };

    const keywordMapping = .{
        .{ "and", .keyword_and },
        .{ "break", .keyword_break },
        .{ "do", .keyword_do },
        .{ "else", .keyword_else },
        .{ "elseif", .keyword_elseif },
        .{ "end", .keyword_end },
        .{ "false", .keyword_false },
        .{ "for", .keyword_for },
        .{ "function", .keyword_function },
        .{ "if", .keyword_if },
        .{ "in", .keyword_in },
        .{ "local", .keyword_local },
        .{ "nil", .keyword_nil },
        .{ "not", .keyword_not },
        .{ "or", .keyword_or },
        .{ "repeat", .keyword_repeat },
        .{ "return", .keyword_return },
        .{ "then", .keyword_then },
        .{ "true", .keyword_true },
        .{ "until", .keyword_until },
        .{ "while", .keyword_while },
    };
    pub const keywords = std.ComptimeStringMap(Id, keywordMapping);

    pub const Id = enum {
        // terminal symbols denoted by reserved words
        keyword_and,
        keyword_break,
        keyword_do,
        keyword_else,
        keyword_elseif,
        keyword_end,
        keyword_false,
        keyword_for,
        keyword_function,
        keyword_if,
        keyword_in,
        keyword_local,
        keyword_nil,
        keyword_not,
        keyword_or,
        keyword_repeat,
        keyword_return,
        keyword_then,
        keyword_true,
        keyword_until,
        keyword_while,
        // any normal byte
        single_char,
        // other terminal symbols
        concat,
        ellipsis,
        eq,
        ge,
        le,
        ne,
        number,
        name,
        string,
        eof,
    };

    // A mapping of id -> name pairs as an array
    const keywordNames = blk: {
        // FIXME: This relies on the keyword enums starting at 0 and being contiguous
        var array: [keywordMapping.len][]const u8 = undefined;
        for (keywordMapping) |mapping| {
            const name = mapping[0];
            const id = @enumToInt(@as(Id, mapping[1]));
            array[id] = name;
        }
        break :blk array;
    };

    // buffer for nameForDisplay
    var token_name_buf: [10]u8 = undefined;

    /// Intended to be equivalent to Lua's luaX_token2str function
    ///
    /// NOTE: To avoid allocation, this function uses a static buffer for the
    ///       name of control characters (which display as `char(15)`, etc).
    ///       This means that this function only works correctly if you immediately
    ///       print/copy the result before calling it again, as the returned
    ///       slice can potentially be overwritten on subsequent calls.
    /// TODO: is this ok? ^
    pub fn nameForDisplay(self: *const Token) []const u8 {
        return switch (self.id) {
            .keyword_and,
            .keyword_break,
            .keyword_do,
            .keyword_else,
            .keyword_elseif,
            .keyword_end,
            .keyword_false,
            .keyword_for,
            .keyword_function,
            .keyword_if,
            .keyword_in,
            .keyword_local,
            .keyword_nil,
            .keyword_not,
            .keyword_or,
            .keyword_repeat,
            .keyword_return,
            .keyword_then,
            .keyword_true,
            .keyword_until,
            .keyword_while,
            => keywordNames[@enumToInt(self.id)],
            .concat => "..",
            .ellipsis => "...",
            .eq => "==",
            .ge => ">=",
            .le => "<=",
            .ne => "~=",
            .number => "<number>",
            .name => "<name>",
            .string => "<string>",
            .eof => "<eof>",
            .single_char => blk: {
                if (std.ascii.isCntrl(self.char.?)) {
                    break :blk std.fmt.bufPrint(&token_name_buf, "char({d})", .{self.char.?}) catch unreachable;
                } else {
                    break :blk @as(*const [1]u8, &self.char.?)[0..1];
                }
            },
        };
    }

    /// Intended to be equivalent to Lua's txtToken (llex.c) function
    ///
    /// NOTE: The slice returned should be considered temporary, and either copied
    ///       or otherwise used immediately. See also nameForDisplay, which this function
    ///       can call.
    pub fn nameForErrorDisplay(self: *const Token, source: []const u8) []const u8 {
        return switch (self.id) {
            .name, .string, .number => source[self.start..self.end],
            else => self.nameForDisplay(),
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
    ChunkHasTooManyLines,
    LexicalElementTooLong,
};

/// error -> msg lookup for lex errors
pub const lex_error_strings = AutoComptimeLookup(LexError, []const u8, .{
    .{ LexError.UnfinishedString, "unfinished string" },
    .{ LexError.UnfinishedLongComment, "unfinished long comment" },
    .{ LexError.UnfinishedLongString, "unfinished long string" },
    .{ LexError.InvalidLongStringDelimiter, "invalid long string delimiter" },
    .{ LexError.MalformedNumber, "malformed number" },
    .{ LexError.EscapeSequenceTooLarge, "escape sequence too large" },
    .{ LexError.ChunkHasTooManyLines, "chunk has too many lines" },
    .{ LexError.LexicalElementTooLong, "lexical element too long" },
});

pub const LexErrorContext = struct {
    token: Token,
    // TODO this is kinda weird, doesn't seem like it needs to be stored (maybe passed to render instead?)
    err: LexError,

    pub fn renderAlloc(self: *LexErrorContext, allocator: Allocator, lexer: *Lexer) ![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();

        const looked_up_msg = lex_error_strings.get(self.err).?;
        const error_writer = buffer.writer();
        const MAXSRC = 80; // see MAXSRC in llex.c
        var chunk_id_buf: [MAXSRC]u8 = undefined;
        const chunk_id = zua.object.getChunkId(lexer.chunk_name, &chunk_id_buf);
        try error_writer.print("{s}:{d}: {s}", .{ chunk_id, self.token.line_number, looked_up_msg });
        // special case errors that shouldn't be printed with context
        if (self.err != LexError.ChunkHasTooManyLines and self.err != LexError.LexicalElementTooLong) {
            try error_writer.print(" near '{s}'", .{self.token.nameForErrorDisplay(lexer.buffer)});
        }
        return buffer.toOwnedSlice();
    }
};

pub const Lexer = struct {
    const Self = @This();

    chunk_name: []const u8,
    buffer: []const u8,
    index: usize,
    line_number: usize = 1,
    // TODO this still feels slightly sloppy, could probably be cleaned up still.
    // Would be good to revisit when adding parse error rendering
    error_context: ?LexErrorContext = null,

    /// In Lua 5.1 there is a bug in the lexer where check_next() accepts \0
    /// since \0 is technically in the string literal passed to check_next representing
    /// the set of characters to check for (since the string is null-terminated)
    ///
    /// This affects a few things:
    ///  ".\0" gets lexed as ..
    ///  ".\0\0" and "..\0" get lexed as ...
    ///  the e/E in numeral exponents can be '\0'
    ///  the +- sign for numeral exponents can be '\0'
    ///
    /// Note that these last two affect how numbers are ultimately parsed, since something like
    /// 1\0 gets lexed as the equiv of 1e but gets parsed into 1 (since the str2d func treats it as
    /// a null-terminated string). For this reason, "1\0\0" will succesfully parse into 1
    /// while "1e\0002" will fail to parse (since it will treat it as "1e").
    check_next_bug_compat: bool = true,

    // TODO: implement or ignore this (options for handling nesting of [[]] in multiline strings)
    // for now we simply allow [[ (Lua 5.1 errors by default on [[ saying that nesting is deprecated)
    long_str_nesting_compat: bool = false,

    /// maximum number of allowable lines in a chunk
    /// Lua uses MAX_INT from llimits.h which is set to INT_MAX-2
    /// This is the equivalent of that.
    max_lines: usize = std.math.maxInt(i32) - 2,

    /// maximum size of a lexical element
    /// Lua uses MAX_SIZET/2 where MAX_SIZET is from llimits.h and is set to (~(size_t)0)-2)
    /// This is the equivalent of that.
    max_lexical_element_size: usize = (std.math.maxInt(usize) - 2) / 2,

    pub const Error = LexError;

    pub fn init(buffer: []const u8, chunk_name: []const u8) Self {
        return Self{
            .buffer = buffer,
            .index = 0,
            .chunk_name = chunk_name,
        };
    }

    pub fn dump(self: *Self, token: *const Token) void {
        std.debug.print("{s} {s}:{d}: \"{s}\"\n", .{ @tagName(token.id), token.nameForDisplay(), token.line_number, std.fmt.fmtSliceEscapeLower(self.buffer[token.start..token.end]) });
    }

    const State = enum {
        start,
        identifier,
        string_literal,
        string_literal_backslash,
        string_literal_backslash_line_endings,
        dash,
        dot,
        concat,
        comment_start,
        short_comment,
        long_comment_start,
        long_comment,
        long_comment_possible_end,
        long_string_start,
        long_string,
        long_string_possible_end,
        number,
        number_exponent_start,
        number_exponent,
        number_hex_start,
        number_hex,
        compound_equal,
    };

    pub fn next(self: *Self) Error!Token {
        const start_index = self.index;
        if (veryVerboseLexing) {
            if (self.index < self.buffer.len) {
                std.debug.print("{d}:'{c}'", .{ self.index, self.buffer[self.index] });
            } else {
                std.debug.print("eof", .{});
            }
        }
        var result = Token{
            .id = Token.Id.eof,
            .start = start_index,
            .end = undefined,
            .char = null,
            .line_number = self.line_number,
        };
        var state = State.start;
        var string_delim: u8 = undefined;
        var string_level: usize = 0;
        var expected_string_level: usize = 0;
        var string_escape_n: std.math.IntFittingRange(0, 999) = 0;
        var string_escape_i: std.math.IntFittingRange(0, 3) = 0;
        var string_escape_line_ending: u8 = undefined;
        var last_line_ending_index: ?usize = null;
        var number_is_float: bool = false;
        var number_starting_char: u8 = undefined;
        var number_exponent_signed_char: ?u8 = null;
        var number_is_null_terminated: bool = false;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            if (veryVerboseLexing) std.debug.print(":{s}", .{@tagName(state)});
            // Check for tokens that are over the size limit here as a catch-all
            if (self.index - result.start >= self.max_lexical_element_size) {
                return self.reportLexError(LexError.LexicalElementTooLong, result, Token.Id.eof);
            }
            switch (state) {
                State.start => switch (c) {
                    '\n', '\r' => {
                        result.start = self.index + 1;
                        result.line_number = try self.incrementLineNumber(&last_line_ending_index);
                    },
                    // space, tab, vertical tab, form feed
                    ' ', '\t', '\x0b', '\x0c' => {
                        // skip whitespace
                        result.start = self.index + 1;
                    },
                    '-' => {
                        // this could be the start of a comment, a long comment, or a single -
                        state = State.dash;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        state = State.identifier;
                        result.id = Token.Id.name;
                    },
                    '0'...'9' => {
                        state = State.number;
                        number_starting_char = c;
                        if (self.check_next_bug_compat) {
                            number_is_null_terminated = false;
                        }
                    },
                    '"', '\'' => {
                        state = State.string_literal;
                        string_delim = c;
                        result.id = Token.Id.string;
                    },
                    '.' => {
                        // this could be the start of .., ..., or a single .
                        state = State.dot;
                    },
                    '>', '<', '~', '=' => {
                        state = State.compound_equal;
                    },
                    '[' => {
                        state = State.long_string_start;
                        expected_string_level = 0;
                    },
                    else => {
                        result.id = Token.Id.single_char;
                        self.index += 1;
                        break;
                    },
                },
                State.identifier => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        const name = self.buffer[result.start..self.index];
                        if (Token.Keyword.idFromName(name)) |id| {
                            result.id = id;
                        }
                        break;
                    },
                },
                State.string_literal => switch (c) {
                    '\\' => {
                        state = State.string_literal_backslash;
                        string_escape_i = 0;
                        string_escape_n = 0;
                    },
                    '"', '\'' => {
                        if (c == string_delim) {
                            self.index += 1;
                            break;
                        }
                    },
                    '\n', '\r' => {
                        return self.reportLexError(LexError.UnfinishedString, result, Token.Id.string);
                    },
                    else => {},
                },
                State.string_literal_backslash => switch (c) {
                    '0'...'9' => {
                        // Validate that any \ddd escape sequences can actually fit
                        // in a byte
                        string_escape_n = 10 * string_escape_n + (c - '0');
                        string_escape_i += 1;
                        if (string_escape_i == 3) {
                            if (string_escape_n > std.math.maxInt(u8)) {
                                return self.reportLexErrorInc(LexError.EscapeSequenceTooLarge, result, Token.Id.string);
                            }
                            state = State.string_literal;
                        }
                    },
                    '\r', '\n' => {
                        if (string_escape_i > 0) {
                            return self.reportLexError(LexError.UnfinishedString, result, Token.Id.string);
                        }
                        state = State.string_literal_backslash_line_endings;
                        string_escape_line_ending = c;
                        result.line_number = try self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {
                        // if the escape sequence had any digits, then
                        // we need to backtrack so as not to escape the current
                        // character (since the digits are the things being escaped)
                        if (string_escape_i > 0) {
                            self.index -= 1;
                        }
                        state = State.string_literal;
                    },
                },
                State.string_literal_backslash_line_endings => switch (c) {
                    '\r', '\n' => {
                        // can only escape \r\n or \n\r pairs, not \r\r or \n\n
                        if (c == string_escape_line_ending) {
                            return self.reportLexError(LexError.UnfinishedString, result, Token.Id.string);
                        } else {
                            state = State.string_literal;
                        }
                        result.line_number = try self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {
                        // backtrack so that we don't escape the current char
                        self.index -= 1;
                        state = State.string_literal;
                    },
                },
                State.dash => switch (c) {
                    '-' => {
                        state = State.comment_start;
                    },
                    else => {
                        result.id = Token.Id.single_char;
                        break;
                    },
                },
                State.comment_start => switch (c) {
                    '[' => {
                        state = State.long_comment_start;
                        expected_string_level = 0;
                    },
                    '\r', '\n' => {
                        // comment immediately ends
                        result.start = self.index + 1;
                        state = State.start;
                        result.line_number = try self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {
                        state = State.short_comment;
                    },
                },
                State.long_string_start,
                State.long_comment_start,
                => switch (c) {
                    '=' => {
                        expected_string_level += 1;
                    },
                    '[' => {
                        state = if (state == State.long_comment_start) State.long_comment else State.long_string;
                    },
                    else => {
                        if (state == State.long_comment_start) {
                            if (c == '\n' or c == '\r') {
                                // not a long comment, but the short comment ends immediately
                                result.start = self.index + 1;
                                state = State.start;
                                result.line_number = try self.incrementLineNumber(&last_line_ending_index);
                            } else {
                                state = State.short_comment;
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
                                return self.reportLexError(LexError.InvalidLongStringDelimiter, result, Token.Id.string);
                            } else {
                                result.id = Token.Id.single_char;
                                break;
                            }
                        }
                    },
                },
                State.long_string,
                State.long_comment,
                => switch (c) {
                    ']' => {
                        state = if (state == State.long_comment) State.long_comment_possible_end else State.long_string_possible_end;
                        string_level = 0;
                    },
                    else => {
                        result.line_number = try self.maybeIncrementLineNumber(&last_line_ending_index);
                    },
                },
                State.long_string_possible_end,
                State.long_comment_possible_end,
                => switch (c) {
                    ']' => {
                        if (string_level == expected_string_level) {
                            if (state == State.long_comment_possible_end) {
                                result.start = self.index + 1;
                                state = State.start;
                            } else {
                                self.index += 1;
                                result.id = Token.Id.string;
                                break;
                            }
                        } else {
                            // don't change state, since this char could be the start of a new end sequence
                            // but reset the level back to 0
                            string_level = 0;
                        }
                    },
                    '=' => {
                        string_level += 1;
                    },
                    else => {
                        result.line_number = try self.maybeIncrementLineNumber(&last_line_ending_index);
                        state = if (state == State.long_comment_possible_end) State.long_comment else State.long_string;
                    },
                },
                State.short_comment => switch (c) {
                    '\n', '\r' => {
                        result.start = self.index + 1;
                        state = State.start;
                        result.line_number = try self.incrementLineNumber(&last_line_ending_index);
                    },
                    else => {},
                },
                State.dot => switch (c) {
                    '.' => {
                        state = State.concat;
                    },
                    '0'...'9' => {
                        state = State.number;
                        number_starting_char = '.';
                        number_is_float = true;
                        if (self.check_next_bug_compat) {
                            number_is_null_terminated = false;
                        }
                    },
                    else => {
                        if (self.check_next_bug_compat and c == '\x00') {
                            state = State.concat;
                        } else {
                            result.id = Token.Id.single_char;
                            break;
                        }
                    },
                },
                State.concat => switch (c) {
                    '.' => {
                        result.id = Token.Id.ellipsis;
                        // include this .
                        self.index += 1;
                        break;
                    },
                    else => {
                        if (self.check_next_bug_compat and c == '\x00') {
                            result.id = Token.Id.ellipsis;
                            // include this .
                            self.index += 1;
                            break;
                        } else {
                            result.id = Token.Id.concat;
                            break;
                        }
                    },
                },
                State.number => switch (c) {
                    '0'...'9' => {},
                    '.' => {
                        // multiple decimal points not allowed
                        if (number_is_float) {
                            return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number);
                        }
                        number_is_float = true;
                    },
                    'x', 'X' => {
                        // only 0x is allowed
                        if (number_starting_char != '0') {
                            return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number);
                        }
                        state = State.number_hex_start;
                    },
                    'e', 'E' => {
                        state = State.number_exponent_start;
                        number_exponent_signed_char = null;
                    },
                    // 'a'...'z' minus e and x
                    'a'...'d', 'A'...'D', 'f'...'w', 'F'...'W', 'y'...'z', 'Y'...'Z' => {
                        return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number);
                    },
                    '_' => return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number),
                    else => {
                        if (self.check_next_bug_compat and c == '\x00') {
                            state = State.number_exponent_start;
                            number_exponent_signed_char = null;
                            number_is_null_terminated = true;
                        } else {
                            result.id = Token.Id.number;
                            break;
                        }
                    },
                },
                State.number_hex_start, State.number_hex => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        state = State.number_hex;
                    },
                    'g'...'z', 'G'...'Z' => {
                        return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number);
                    },
                    '_' => return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number),
                    else => {
                        result.id = Token.Id.number;
                        break;
                    },
                },
                State.number_exponent_start => {
                    const should_consume_anything = self.check_next_bug_compat and number_is_null_terminated;
                    if (should_consume_anything) {
                        switch (c) {
                            '\x00', '-', '+', '0'...'9', 'a'...'z', 'A'...'Z', '_' => {
                                state = State.number_exponent;
                            },
                            else => {
                                result.id = Token.Id.number;
                                break;
                            },
                        }
                    } else {
                        switch (c) {
                            '0'...'9' => state = State.number_exponent,
                            '-', '+' => {
                                if (number_exponent_signed_char) |_| {
                                    // this is an error because e.g. "1e--" would lex as "1e-" and "-"
                                    // and "1e-" is always invalid
                                    return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number);
                                }
                                number_exponent_signed_char = c;
                            },
                            else => {
                                // if we get here, then the token up to this point has to be
                                // either 1e, 1e-, 1e+ which *must* be followed by a digit, and
                                // we already know c is not a digit
                                return self.reportLexErrorInc(LexError.MalformedNumber, result, Token.Id.number);
                            },
                        }
                    }
                },
                State.number_exponent => {
                    const should_consume_anything = self.check_next_bug_compat and number_is_null_terminated;
                    if (should_consume_anything) {
                        switch (c) {
                            '0'...'9', 'a'...'z', 'A'...'Z', '_' => {},
                            else => {
                                result.id = Token.Id.number;
                                break;
                            },
                        }
                    } else {
                        switch (c) {
                            '0'...'9' => {},
                            'a'...'z', 'A'...'Z', '_' => return self.reportLexError(LexError.MalformedNumber, result, Token.Id.number),
                            else => {
                                result.id = Token.Id.number;
                                break;
                            },
                        }
                    }
                },
                State.compound_equal => switch (c) {
                    '=' => {
                        switch (self.buffer[self.index - 1]) {
                            '>' => result.id = Token.Id.ge,
                            '<' => result.id = Token.Id.le,
                            '~' => result.id = Token.Id.ne,
                            '=' => result.id = Token.Id.eq,
                            else => unreachable,
                        }
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.id = Token.Id.single_char;
                        break;
                    },
                },
            }
        } else {
            // this will always be true due to the while loop condition as the
            // else block is not evaluated after a break; in the while loop, but
            // rather only when the loop condition fails
            std.debug.assert(self.index == self.buffer.len);
            switch (state) {
                State.start => {},
                State.identifier => {
                    const name = self.buffer[result.start..self.index];
                    if (Token.Keyword.idFromName(name)) |id| {
                        result.id = id;
                    }
                },
                State.dot,
                State.dash,
                State.compound_equal,
                => {
                    result.id = Token.Id.single_char;
                },
                State.concat => {
                    result.id = Token.Id.concat;
                },
                State.number_exponent,
                State.number_hex,
                State.number,
                => {
                    result.id = Token.Id.number;
                },
                State.comment_start,
                State.short_comment,
                State.long_comment_start,
                => {
                    result.start = self.index;
                },
                State.long_string_start => {
                    if (expected_string_level > 0) {
                        return self.reportLexError(LexError.InvalidLongStringDelimiter, result, Token.Id.string);
                    } else {
                        result.id = Token.Id.single_char;
                    }
                },
                // .eof is reported as the error context to conform with how PUC Lua reports unfinished <x> errors
                // when the end of the buffer is reached before the string is closed. Not totally sure why .string
                // isn't used for the unfinished string errors in this case, though--the error message seems nicer
                // that way
                // TODO revisit?
                State.long_comment_possible_end,
                State.long_comment,
                => return self.reportLexError(LexError.UnfinishedLongComment, result, Token.Id.eof),
                State.long_string_possible_end,
                State.long_string,
                => return self.reportLexError(LexError.UnfinishedLongString, result, Token.Id.eof),
                State.string_literal,
                State.string_literal_backslash,
                State.string_literal_backslash_line_endings,
                => return self.reportLexError(LexError.UnfinishedString, result, Token.Id.eof),
                State.number_hex_start,
                State.number_exponent_start,
                => {
                    if (self.check_next_bug_compat and number_is_null_terminated) {
                        result.id = Token.Id.number;
                    } else {
                        return self.reportLexError(LexError.MalformedNumber, result, Token.Id.number);
                    }
                },
            }
        }

        if (veryVerboseLexing) {
            if (self.index < self.buffer.len) {
                std.debug.print(":{d}:'{c}'=\"{s}\"\n", .{ self.index, self.buffer[self.index], self.buffer[result.start..self.index] });
            } else {
                std.debug.print(":eof=\"{s}\"\n", .{self.buffer[result.start..self.index]});
            }
        }

        if (result.id == Token.Id.single_char) {
            result.char = self.buffer[result.start];
        }

        result.end = self.index;
        return result;
    }

    pub fn lookahead(self: *Self) Error!Token {
        var lookaheadLexer = Lexer{
            .buffer = self.buffer,
            .index = self.index,
            .line_number = self.line_number,
            .chunk_name = self.chunk_name,
            .check_next_bug_compat = self.check_next_bug_compat,
            .long_str_nesting_compat = self.long_str_nesting_compat,
        };
        return lookaheadLexer.next();
    }

    fn reportLexError(self: *Self, err: LexError, unfinished_token: Token, id: Token.Id) Error {
        self.error_context = .{
            .token = .{
                .id = id,
                .start = unfinished_token.start,
                .end = self.index,
                .line_number = self.line_number,
                .char = null, // TODO double check that this is always true (single char token can't cause a lex error, right?)
            },
            .err = err,
        };
        return err;
    }

    fn reportLexErrorInc(self: *Self, err: LexError, unfinished_token: Token, id: Token.Id) Error {
        self.index += 1;
        return self.reportLexError(err, unfinished_token, id);
    }

    pub fn renderErrorAlloc(self: *Self, allocator: Allocator) ![]const u8 {
        if (self.error_context) |*ctx| {
            return ctx.renderAlloc(allocator, self);
        } else {
            return error.NoError;
        }
    }

    /// Like incrementLineNumber but checks that the current char is a line ending first
    fn maybeIncrementLineNumber(self: *Self, last_line_ending_index: *?usize) !usize {
        const c = self.buffer[self.index];
        if (c == '\r' or c == '\n') {
            return try self.incrementLineNumber(last_line_ending_index);
        }
        return self.line_number;
    }

    /// Increments line_number appropriately (handling line ending pairs)
    /// and returns the new line number.
    /// note: mutates last_line_ending_index.*
    fn incrementLineNumber(self: *Self, last_line_ending_index: *?usize) !usize {
        if (self.currentIndexFormsLineEndingPair(last_line_ending_index.*)) {
            last_line_ending_index.* = null;
        } else {
            self.line_number += 1;
            last_line_ending_index.* = self.index;
        }
        if (self.line_number >= self.max_lines) {
            // TODO using a dummy token here is pretty janky
            return self.reportLexError(LexError.ChunkHasTooManyLines, .{
                .id = undefined,
                .start = self.index,
                .end = self.index,
                .line_number = self.line_number,
                .char = null,
            }, Token.Id.eof);
        }
        return self.line_number;
    }

    /// \r\n and \n\r pairs are treated as a single line ending (but not \r\r \n\n)
    /// expects self.index and last_line_ending_index (if non-null) to contain line endings
    fn currentIndexFormsLineEndingPair(self: *Self, last_line_ending_index: ?usize) bool {
        if (last_line_ending_index == null) return false;

        // must immediately precede the current index
        if (last_line_ending_index.? != self.index - 1) return false;

        const cur_line_ending = self.buffer[self.index];
        const last_line_ending = self.buffer[last_line_ending_index.?];

        // sanity check
        std.debug.assert(cur_line_ending == '\r' or cur_line_ending == '\n');
        std.debug.assert(last_line_ending == '\r' or last_line_ending == '\n');

        // can't be \n\n or \r\r
        if (last_line_ending == cur_line_ending) return false;

        return true;
    }
};

test "hello \"world\"" {
    try testLex("local hello = \"wor\\\"ld\"", &[_]Token.Id{
        Token.Id.keyword_local,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.string,
    });
}

test "hello 'world'" {
    try testLex("local hello = 'wor\\'ld'", &[_]Token.Id{
        Token.Id.keyword_local,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.string,
    });
}

test "strings" {
    // none of these escaped chars have any meaning, but Lua allows
    // any character to be escaped so this should lex just fine
    try testLex("'\\e\\s\\c\\ any char'", &[_]Token.Id{Token.Id.string});
    try testLex("'\\1'", &[_]Token.Id{Token.Id.string});
    try testLex("'\\12'", &[_]Token.Id{Token.Id.string});
    try testLex("'\\123'", &[_]Token.Id{Token.Id.string});
    try testLex("'\\1234'", &[_]Token.Id{Token.Id.string});
    // carriage returns and newlines can be escaped with \
    try testLex("'\\\n\\\r'", &[_]Token.Id{Token.Id.string});
    try testLex("\".\\\x0d\\\\\\\".\\\x0d\xa5[\\ArA\"", &[_]Token.Id{Token.Id.string});
    // a pair of CR/LF can be escaped with a single \ (either CRLF or LFCR)
    try testLex("'\\\r\n'", &[_]Token.Id{Token.Id.string});
    try testLex("'\\\n\r'", &[_]Token.Id{Token.Id.string});
}

test "long strings" {
    try testLex("[[]]", &[_]Token.Id{Token.Id.string});
    try testLex("[===[\nhello\nworld\n]===]", &[_]Token.Id{Token.Id.string});
    try testLex("[]", &[_]Token.Id{ Token.Id.single_char, Token.Id.single_char });
    // TODO: this depends on LUA_COMPAT_LSTR
    try testLex("[[ [[ ]]", &[_]Token.Id{Token.Id.string});
    // this is always allowed
    try testLex("[=[ [[ ]] ]=]", &[_]Token.Id{Token.Id.string});
    // unfinished end directly into real end
    try testLex("[==[]=]==]", &[_]Token.Id{Token.Id.string});
}

test "comments and dashes" {
    try testLex("-", &[_]Token.Id{Token.Id.single_char});
    try testLex("a-b", &[_]Token.Id{ Token.Id.name, Token.Id.single_char, Token.Id.name });
    try testLex("--", &[_]Token.Id{});
    try testLex("--local hello = 'wor\\'ld'", &[_]Token.Id{});
    try testLex("--[this is a short comment\nreturn", &[_]Token.Id{Token.Id.keyword_return});
    try testLex("--\rreturn", &[_]Token.Id{Token.Id.keyword_return});
    try testLex("--[[local hello = 'wor\\'ld']]", &[_]Token.Id{});
    try testLex("--[==[\nlocal\nhello\n=\n'world'\n]==]", &[_]Token.Id{});
    try testLex("--[==", &[_]Token.Id{});
    try testLex("--[\n]]", &[_]Token.Id{ Token.Id.single_char, Token.Id.single_char });
    // unfinished end directly into real end
    try testLex("--[===[]=]===]", &[_]Token.Id{});
}

test "whitespace" {
    // form feed
    try testLex("_\x0c_W_", &[_]Token.Id{ Token.Id.name, Token.Id.name });
    // vertical tab
    try testLex("_\x0b_W_", &[_]Token.Id{ Token.Id.name, Token.Id.name });
}

test "dots, concat, ellipsis" {
    try testLex(".", &[_]Token.Id{Token.Id.single_char});
    try testLex("a.b", &[_]Token.Id{ Token.Id.name, Token.Id.single_char, Token.Id.name });
    try testLex("..", &[_]Token.Id{Token.Id.concat});
    try testLex("a..b.c", &[_]Token.Id{
        Token.Id.name,
        Token.Id.concat,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.name,
    });
    // this is valid Lua, apparently (abc will be true, test will be the first value in ...)
    try testLex("test=...abc=true", &[_]Token.Id{
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.ellipsis,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.keyword_true,
    });
}

test "= and compound = operators" {
    try testLex("=", &[_]Token.Id{Token.Id.single_char});
    try testLex("a=b", &[_]Token.Id{ Token.Id.name, Token.Id.single_char, Token.Id.name });
    try testLex("a==b", &[_]Token.Id{ Token.Id.name, Token.Id.eq, Token.Id.name });
    try testLex(">=", &[_]Token.Id{Token.Id.ge});
    try testLex("if a~=b and a<=b and b<a then end", &[_]Token.Id{
        Token.Id.keyword_if,
        Token.Id.name,
        Token.Id.ne,
        Token.Id.name,
        Token.Id.keyword_and,
        Token.Id.name,
        Token.Id.le,
        Token.Id.name,
        Token.Id.keyword_and,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.name,
        Token.Id.keyword_then,
        Token.Id.keyword_end,
    });
}

test "numbers" {
    // from the Lua 5.1 manual
    try testLex("3", &[_]Token.Id{Token.Id.number});
    try testLex("3.0", &[_]Token.Id{Token.Id.number});
    try testLex("3.1416", &[_]Token.Id{Token.Id.number});
    try testLex("314.16e-2", &[_]Token.Id{Token.Id.number});
    try testLex("0.31416E1", &[_]Token.Id{Token.Id.number});
    try testLex("0xff", &[_]Token.Id{Token.Id.number});
    try testLex("0x56", &[_]Token.Id{Token.Id.number});

    // other cases
    try testLex(".1", &[_]Token.Id{Token.Id.number});
    try testLex("0xFF", &[_]Token.Id{Token.Id.number});
    try testLex("0XeF", &[_]Token.Id{Token.Id.number});
    try testLex("1e+3", &[_]Token.Id{Token.Id.number});
    // 3e2 and .52 should lex as separate tokens
    try testLex("3e2.52", &[_]Token.Id{ Token.Id.number, Token.Id.number });
}

test "LexError.MalformedNumber" {
    try expectLexError(LexError.MalformedNumber, testLex("1e", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0z", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0x", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0xabcz", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("1xabc", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1.e2", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1.", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1.2", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e3a", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e-", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e-a", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e+", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e--2", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e-)2", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0.1e+-2", &[_]Token.Id{Token.Id.number}));
    // Lua's lexer weirdly 'allows'/consumes _ when lexing numbers (see llex.c:201 in 5.1.5),
    // but as far as I can tell there are no valid ways to define a number with a _ in it.
    // Either way, we should fail with MalformedNumber in the same ways that Lua does,
    // so we need to handle _ similarly to the Lua lexer.
    try expectLexError(LexError.MalformedNumber, testLex("1_2", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0x2__", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("0x__", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("1e__", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex("1e-1_", &[_]Token.Id{Token.Id.number}));
    try expectLexError(LexError.MalformedNumber, testLex(".1_", &[_]Token.Id{Token.Id.number}));
}

test "LexError.InvalidLongStringDelimiter" {
    // see comment in Lexer.next near the return of LexError.InvalidLongStringDelimiter
    const simple = testLex("[==]", &[_]Token.Id{Token.Id.string});
    try expectLexError(LexError.InvalidLongStringDelimiter, simple);

    const number = testLex("[=======4", &[_]Token.Id{Token.Id.string});
    try expectLexError(LexError.InvalidLongStringDelimiter, number);

    const eof = testLex("[==", &[_]Token.Id{Token.Id.string});
    try expectLexError(LexError.InvalidLongStringDelimiter, eof);
}

test "LexError.EscapeSequenceTooLarge" {
    try expectLexError(LexError.EscapeSequenceTooLarge, testLex("'\\256'", &[_]Token.Id{Token.Id.string}));
}

test "LexError.UnfinishedLongComment" {
    const simple = testLex("--[[", &[_]Token.Id{});
    try expectLexError(LexError.UnfinishedLongComment, simple);

    const mismatchedSep = testLex("--[==[ ]=]", &[_]Token.Id{});
    try expectLexError(LexError.UnfinishedLongComment, mismatchedSep);
}

test "LexError.UnfinishedString" {
    const missingQuoteResult = testLex("local hello = \"wor\\\"ld", &[_]Token.Id{
        Token.Id.keyword_local,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.string,
    });
    try expectLexError(LexError.UnfinishedString, missingQuoteResult);

    const newlineResult = testLex("local hello = \"wor\\\"ld\n\"", &[_]Token.Id{
        Token.Id.keyword_local,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.string,
    });
    try expectLexError(LexError.UnfinishedString, newlineResult);
}

test "5.1 check_next bug compat on" {
    try testLexCheckNextBugCompat(".\x00", &[_]Token.Id{Token.Id.concat});
    try testLexCheckNextBugCompat(".\x00\x00", &[_]Token.Id{Token.Id.ellipsis});
    try testLexCheckNextBugCompat("..\x00", &[_]Token.Id{Token.Id.ellipsis});
    try testLexCheckNextBugCompat("1\x00", &[_]Token.Id{Token.Id.number});
    try testLexCheckNextBugCompat("1\x00-5", &[_]Token.Id{Token.Id.number});
    try testLexCheckNextBugCompat("1\x00\x005", &[_]Token.Id{Token.Id.number});
    try testLexCheckNextBugCompat("1\x00\x00anythingcangoherenow", &[_]Token.Id{Token.Id.number});
    try testLexCheckNextBugCompat(".0\x00", &[_]Token.Id{Token.Id.number});
    try testLexCheckNextBugCompat(".0\x00)", &[_]Token.Id{ Token.Id.number, Token.Id.single_char });
    // should lex as: 5\x00z5 ; \x00 ; 9\x00\x00 ; \x00
    try testLexCheckNextBugCompat("5\x00z5\x009\x00\x00\x00", &[_]Token.Id{
        Token.Id.number,
        Token.Id.single_char,
        Token.Id.number,
        Token.Id.single_char,
    });
    try testLexCheckNextBugCompat("5\x00--z5", &[_]Token.Id{
        Token.Id.number,
        Token.Id.single_char,
        Token.Id.name,
    });
    try expectLexError(LexError.MalformedNumber, testLexCheckNextBugCompat("1e\x005", &[_]Token.Id{Token.Id.number}));
}

test "5.1 check_next bug compat off" {
    try testLexNoCheckNextBugCompat(".\x00", &[_]Token.Id{ Token.Id.single_char, Token.Id.single_char });
    try testLexNoCheckNextBugCompat("1\x00", &[_]Token.Id{ Token.Id.number, Token.Id.single_char });
    try testLexNoCheckNextBugCompat("1\x00-5", &[_]Token.Id{ Token.Id.number, Token.Id.single_char, Token.Id.single_char, Token.Id.number });
    // should lex as: 5 ; \x00 ; z5 ; \x00 ; 9 ; \x00 ; \x00 ; \x00
    try testLexNoCheckNextBugCompat("5\x00z5\x009\x00\x00\x00", &[_]Token.Id{
        Token.Id.number,
        Token.Id.single_char,
        Token.Id.name,
        Token.Id.single_char,
        Token.Id.number,
        Token.Id.single_char,
        Token.Id.single_char,
        Token.Id.single_char,
    });
    try expectLexError(LexError.MalformedNumber, testLexNoCheckNextBugCompat("1e\x005", &[_]Token.Id{Token.Id.number}));
}

fn expectLexError(expected: LexError, actual: anytype) !void {
    if (veryVerboseLexing) std.debug.print("\n", .{});
    try std.testing.expectError(expected, actual);
    if (dumpTokensDuringTests) std.debug.print("{}\n", .{actual});
}

fn testLex(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source, source);
    return testLexInitialized(&lexer, expected_tokens);
}

fn testLexCheckNextBugCompat(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source, source);
    lexer.check_next_bug_compat = true;
    return testLexInitialized(&lexer, expected_tokens);
}

fn testLexNoCheckNextBugCompat(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source, source);
    lexer.check_next_bug_compat = false;
    return testLexInitialized(&lexer, expected_tokens);
}

fn testLexInitialized(lexer: *Lexer, expected_tokens: []const Token.Id) !void {
    if (dumpTokensDuringTests) std.debug.print("\n----------------------\n{s}\n----------------------\n", .{lexer.buffer});
    for (expected_tokens) |expected_token_id| {
        const token = try lexer.next();
        if (dumpTokensDuringTests) lexer.dump(&token);
        try std.testing.expectEqual(expected_token_id, token.id);
    }
    const last_token = try lexer.next();
    try std.testing.expectEqual(Token.Id.eof, last_token.id);
}

test "line numbers" {
    try testLexLineNumbers(
        \\a
        \\b
        \\c
    ,
        &[_]TokenAndLineNumber{
            .{ .id = Token.Id.name, .line_number = 1 },
            .{ .id = Token.Id.name, .line_number = 2 },
            .{ .id = Token.Id.name, .line_number = 3 },
            .{ .id = Token.Id.eof, .line_number = 3 },
        },
    );
    try testLexLineNumbers("\n\n\na", &[_]TokenAndLineNumber{
        .{ .id = Token.Id.name, .line_number = 4 },
        .{ .id = Token.Id.eof, .line_number = 4 },
    });
    // \r\n pair separated by a comment
    try testLexLineNumbers("\r--comment\na", &[_]TokenAndLineNumber{
        .{ .id = Token.Id.name, .line_number = 3 },
        .{ .id = Token.Id.eof, .line_number = 3 },
    });

    // line endings in comments should affect the line numbers of the tokens afterwards
    try testLexLineNumbers(
        "n--[[\n]]",
        &[_]TokenAndLineNumber{
            .{ .id = Token.Id.name, .line_number = 1 },
            .{ .id = Token.Id.eof, .line_number = 2 },
        },
    );
}

const TokenAndLineNumber = struct {
    id: Token.Id,
    line_number: usize,
};

fn testLexLineNumbers(source: []const u8, expected_tokens: []const TokenAndLineNumber) !void {
    var lexer = Lexer.init(source, source);
    if (dumpTokensDuringTests) std.debug.print("\n----------------------\n{s}\n----------------------\n", .{lexer.buffer});
    for (expected_tokens) |expected_token| {
        const token = try lexer.next();
        if (dumpTokensDuringTests) lexer.dump(&token);
        try std.testing.expectEqual(expected_token.id, token.id);
        try std.testing.expectEqual(expected_token.line_number, token.line_number);
        if (token.id == Token.Id.eof) {
            return;
        }
    }
    unreachable; // never hit EOF
}

test "chunk has too many lines" {
    var max_lines_lexer = Lexer.init("\n\n\n\n\n", "max_lines");
    // reduce the limit to something manageable so we don't have to test with a giant file
    max_lines_lexer.max_lines = 4;

    while (true) {
        const token = max_lines_lexer.next() catch |err| {
            try std.testing.expectEqual(LexError.ChunkHasTooManyLines, err);
            const err_msg = try max_lines_lexer.renderErrorAlloc(std.testing.allocator);
            defer std.testing.allocator.free(err_msg);
            try std.testing.expectEqualStrings(
                "[string \"max_lines\"]:4: chunk has too many lines",
                err_msg,
            );
            break;
        };
        if (token.id == Token.Id.eof) {
            unreachable;
        }
    }
}

test "lexical element too long" {
    var max_size_lexer = Lexer.init(
        \\this.is.ok.since.each.is.sep.token
        \\"but next" .. "line is"
        \\one_too_many
        \\
    , "max_size");
    // reduce the limit to something manageable so we don't have to test with a giant file
    max_size_lexer.max_lexical_element_size = 11;

    while (true) {
        const token = max_size_lexer.next() catch |err| {
            try std.testing.expectEqual(LexError.LexicalElementTooLong, err);
            const err_msg = try max_size_lexer.renderErrorAlloc(std.testing.allocator);
            defer std.testing.allocator.free(err_msg);
            try std.testing.expectEqualStrings(
                "[string \"max_size\"]:3: lexical element too long",
                err_msg,
            );
            break;
        };
        if (token.id == Token.Id.eof) {
            unreachable;
        }
    }
}
