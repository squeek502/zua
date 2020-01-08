const std = @import("std");

/// Because the lexer has already validated that strings don't contain
/// any invalid characters, this function can be implemented without
/// the possibility of failure. Any failures are a bug in the lexer.
///
/// dest_buf must be at least as big as source to ensure it is large enough
/// to hold the parsed string
/// TODO: should this function be part of lex.Token instead?
pub fn parseString(source_raw: []const u8, dest_buf: []u8) []u8 {
    std.debug.assert(dest_buf.len >= source_raw.len);
    var source: []const u8 = source_raw[0..];

    // trim the start/end delimeters
    var delim_len: usize = undefined;
    var is_long_string: bool = false;
    var skip_first_char: bool = false;
    switch (source[0]) {
        '\'', '"' => delim_len = 1,
        '[' => {
            var num_sep: usize = 0;
            while (source[1 + num_sep] == '=') : (num_sep += 1) {}
            std.debug.assert(source[1 + num_sep] == '[');
            delim_len = 2 + num_sep;
            is_long_string = true;
            // if the first line of a long string is a newline char, it gets skipped
            skip_first_char = source[delim_len] == '\r' or source[delim_len] == '\n';
        },
        else => unreachable,
    }
    source = source[delim_len .. source.len - delim_len];
    if (skip_first_char) source = source[1..];

    // like std.io.SliceOutStream but no need to check bounds of slice
    // and can only append 1 character at a time (also doesn't implement Stream)
    const SliceWriter = struct {
        const Self = @This();

        pos: usize = 0,
        slice: []u8,

        fn write(self: *Self, char: u8) void {
            self.slice[self.pos] = char;
            self.pos += 1;
        }

        fn getWritten(self: Self) []u8 {
            return self.slice[0..self.pos];
        }
    };

    const State = enum {
        Normal,
        Escaped,
        EscapedNumerals,
        EscapedLineEndings,
    };

    var writer = SliceWriter{ .slice = dest_buf };

    var string_escape_n: u8 = 0;
    var string_escape_i: std.math.IntFittingRange(0, 3) = 0;
    var state: State = State.Normal;
    var index: usize = 0;
    while (index < source.len) : (index += 1) {
        const c = source[index];
        switch (state) {
            State.Normal => switch (c) {
                // Lua's string parser transforms all \r to \n
                '\r' => writer.write('\n'),
                '\\' => state = State.Escaped,
                else => writer.write(c),
            },
            State.Escaped => switch (c) {
                '0'...'9' => {
                    string_escape_n = c - '0';
                    string_escape_i = 1;
                    state = State.EscapedNumerals;
                },
                '\r', '\n' => {
                    // escaped \r and \n get transformed to \n
                    writer.write('\n');
                    state = State.EscapedLineEndings;
                },
                else => {
                    switch (c) {
                        'a' => writer.write('\x07'),
                        'b' => writer.write('\x08'),
                        'f' => writer.write('\x0C'),
                        'n' => writer.write('\n'),
                        'r' => writer.write('\r'),
                        't' => writer.write('\t'),
                        'v' => writer.write('\x0B'),
                        else => writer.write(c),
                    }
                    state = State.Normal;
                },
            },
            State.EscapedNumerals => switch(c) {
                '0'...'9' => {
                    string_escape_n = 10 * string_escape_n + (c - '0');
                    string_escape_i += 1;
                    if (string_escape_i == 3) {
                        writer.write(string_escape_n);
                        state = State.Normal;
                    }
                },
                else => {
                    writer.write(string_escape_n);
                    // backtrack so that we handle the current char properly
                    index -= 1;
                    state = State.Normal;
                },
            },
            State.EscapedLineEndings => switch(c) {
                '\r', '\n' => {
                    state = State.Normal;
                },
                else => {
                    // backtrack so that we handle the current char properly
                    index -= 1;
                    state = State.Normal;
                },
            },
        }
    }
    // we could be in a state that still needs processing here,
    // since we could have hit the end of the string while unsure
    // if a \ddd pattern was finished
    switch (state) {
        State.EscapedNumerals => {
            writer.write(string_escape_n);
        },
        State.Normal,
        State.EscapedLineEndings,
        => {},
        else => unreachable,
    }

    return writer.getWritten();
}

test "parseString" {
    var buf_arr: [100]u8 = undefined;
    var buf: []u8 = buf_arr[0..];
    std.testing.expectEqualSlices(u8, "hello", parseString("'hello'", buf));
    std.testing.expectEqualSlices(u8, "hello", parseString("\"hello\"", buf));
    std.testing.expectEqualSlices(u8, "hello", parseString("[[hello]]", buf));
    std.testing.expectEqualSlices(u8, "hello", parseString("[=[hello]=]", buf));
    std.testing.expectEqualSlices(u8, "hello", parseString("[===[hello]===]", buf));
    std.testing.expectEqualSlices(u8, "\\ \n \x0B", parseString("'\\\\ \\n \\v'", buf));

    // long strings skip initial newline
    std.testing.expectEqualSlices(u8, "hello", parseString("[[\nhello]]", buf));
    std.testing.expectEqualSlices(u8, "\nhello", parseString("[[\r\rhello]]", buf));

    // escaped \r gets transformed into \n
    std.testing.expectEqualSlices(u8, "\n", parseString("\"\\\r\"", buf));

    // escaped newlines and newline pairs
    std.testing.expectEqualSlices(u8, "\n\\ ", parseString("\"\\\r\\\\ \"", buf));
    std.testing.expectEqualSlices(u8, "\n\\ ", parseString("\"\\\r\n\\\\ \"", buf));
    std.testing.expectEqualSlices(u8, "\n", parseString("\"\\\n\r\"", buf));

    // escaped numerals
    std.testing.expectEqualSlices(u8, "\x01-\x02", parseString("\"\\1-\\2\"", buf));
}
