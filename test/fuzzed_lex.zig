const std = @import("std");
const lex = @import("zua").lex;

// Tests for comparing the tokens of Zua's lexer with Lua's.
// Expects @import("build_options").fuzzed_lex_inputs_dir to be a path to
// a directory containing a corpus of inputs to test and
// @import("build_options").fuzzed_lex_outputs_dir to be a path to a
// directory containing the tokens obtained by running the input
// through the Lua lexer (in a specific format).
//
// A usable corpus/outputs pair can be obtained from
// https://github.com/squeek502/fuzzing-lua

const verboseTestPrinting = false;
const printTokenBounds = false;
const printErrorContextDifferences = false;

const build_options = @import("build_options");
const inputs_dir_opt = build_options.fuzzed_lex_inputs_dir;
const outputs_dir_opt = build_options.fuzzed_lex_outputs_dir;

test "fuzz_llex input/output pairs" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    var allocator = &arena_allocator.allocator;

    // resolve these now since Zig's std lib on Windows rejects paths with / as the path sep
    const inputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{inputs_dir_opt});
    const outputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{outputs_dir_opt});

    var walker = try std.fs.walkPath(allocator, inputs_dir);
    defer walker.deinit();
    var path_buffer = try std.ArrayList(u8).initCapacity(allocator, outputs_dir.len);
    path_buffer.appendSliceAssumeCapacity(outputs_dir);
    defer path_buffer.deinit();

    var result_buffer: [1024 * 1024]u8 = undefined;

    var n: usize = 0;
    while (try walker.next()) |entry| {
        if (verboseTestPrinting) {
            std.debug.warn("\n{s}\n", .{entry.basename});
        }
        const contents = try entry.dir.readFileAlloc(allocator, entry.basename, std.math.maxInt(usize));
        defer allocator.free(contents);

        path_buffer.shrinkRetainingCapacity(outputs_dir.len);
        try path_buffer.append(std.fs.path.sep);
        try path_buffer.appendSlice(entry.basename);
        const expectedContents = try std.fs.cwd().readFileAlloc(allocator, path_buffer.items, std.math.maxInt(usize));
        defer allocator.free(expectedContents);

        // ignore this error for now, see long_str_nesting_compat TODO
        if (std.mem.indexOf(u8, expectedContents, "nesting of [[...]]") != null) {
            continue;
        }

        var result_stream = std.io.fixedBufferStream(&result_buffer);
        const result_writer = result_stream.writer();

        var lexer = lex.Lexer.init(contents, allocator, "fuzz");
        defer lexer.deinit();
        while (true) {
            const token = lexer.next() catch |e| {
                if (verboseTestPrinting) {
                    std.debug.warn("\n{s}\n", .{e});
                }
                try result_writer.writeByte('\n');
                try result_writer.writeAll(lexer.error_buffer.items);
                break;
            };
            if (verboseTestPrinting) {
                if (printTokenBounds) {
                    std.debug.warn("{d}:{s}:{d}", .{ token.start, token.nameForDisplay(), token.end });
                } else {
                    std.debug.warn("{s}", .{token.nameForDisplay()});
                }
            }
            try result_writer.writeAll(token.nameForDisplay());
            if (token.id == lex.Token.Id.eof) {
                break;
            } else {
                if (verboseTestPrinting) {
                    std.debug.warn(" ", .{});
                }
                try result_writer.print(" ", .{});
            }
        }
        if (verboseTestPrinting) {
            std.debug.warn("\nexpected\n{s}\n", .{expectedContents});
        }
        var nearIndex = std.mem.lastIndexOf(u8, expectedContents, " near '");
        if (nearIndex) |i| {
            std.testing.expectEqualStrings(expectedContents[0..i], result_stream.getWritten()[0..i]);
            if (printErrorContextDifferences) {
                var lastLineEnding = std.mem.lastIndexOf(u8, expectedContents, "\n").? + 1;
                const expectedError = expectedContents[lastLineEnding..];
                const actualError = result_stream.getWritten()[lastLineEnding..];
                if (!std.mem.eql(u8, expectedError, actualError)) {
                    std.debug.print("\n{s}\nexpected: {s}\nactual: {s}\n", .{ entry.basename, expectedContents[lastLineEnding..], result_stream.getWritten()[lastLineEnding..] });
                }
            }
        } else {
            std.testing.expectEqualStrings(expectedContents, result_stream.getWritten());
        }
        n += 1;
    }
    std.debug.warn("{d} input/output pairs checked...", .{n});
}
