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

const dumpTokensDuringTests = true;

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

pub const LexError = error{UnfinishedString};

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
    };

    pub fn next(self: *Lexer) LexError!Token {
        const start_index = self.index;
        var result = Token{
            .id = Token.Id.Eof,
            .start = start_index,
            .end = undefined,
        };
        var state = State.Start;
        var string_literal_delim: u8 = undefined;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                State.Start => switch (c) {
                    '\n', '\r' => {
                        result.start = self.index + 1;
                    },
                    ' ', '\t' => {
                        // skip whitespace
                        result.start = self.index + 1;
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
            }
        // while loop didn't break + we are at EOF
        // TODO is this if check redundant?
        } else if (self.index == self.buffer.len) {
            switch (state) {
                State.Start => {},
                State.Identifier => {
                    const name = self.buffer[result.start..self.index];
                    if (Token.Keyword.idFromName(name)) |id| {
                        result.id = id;
                    }
                },
                State.StringLiteral,
                State.StringLiteralBackslash,
                => return LexError.UnfinishedString,
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
    try testTokenize("local hello = \"wor\\\"ld\"", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
}

test "hello 'world'" {
    try testTokenize("local hello = 'wor\\'ld'", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
}

test "LexError.UnfinishedString" {
    const missingQuoteResult = testTokenize("local hello = \"wor\\\"ld", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
    std.testing.expectError(LexError.UnfinishedString, missingQuoteResult);

    const newlineResult = testTokenize("local hello = \"wor\\\"ld\n\"", &[_]Token.Id{
        Token.Id.Keyword_local,
        Token.Id.Name,
        Token.Id.SingleChar,
        Token.Id.String,
    });
    std.testing.expectError(LexError.UnfinishedString, newlineResult);
}

fn testTokenize(source: []const u8, expected_tokens: []const Token.Id) !void {
    var lexer = Lexer.init(source);
    if (dumpTokensDuringTests) std.debug.warn("\n", .{});
    for (expected_tokens) |expected_token_id| {
        const token = try lexer.next();
        if (dumpTokensDuringTests) lexer.dump(&token);
        std.testing.expectEqual(expected_token_id, token.id);
    }
    const last_token = try lexer.next();
    std.testing.expectEqual(Token.Id.Eof, last_token.id);
}
