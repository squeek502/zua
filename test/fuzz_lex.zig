const std = @import("std");
const lex = @import("zua.lex");

const verboseTestPrinting = false;
const printTokenBounds = false;

test "fuzz_llex input/output pairs" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    var allocator = &arena_allocator.allocator;

    const build_options = @import("build_options");
    const inputs_dir_opt = build_options.fuzz_lex_inputs_dir;
    const outputs_dir_opt = build_options.fuzz_lex_outputs_dir;
    // resolve these now since Zig's std lib on Windows rejects paths with / as the path sep
    const inputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{ inputs_dir_opt });
    const outputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{ outputs_dir_opt });

    var walker = try std.fs.walkPath(allocator, inputs_dir);
    defer walker.deinit();
    var path_buffer = try std.Buffer.init(allocator, outputs_dir);
    defer path_buffer.deinit();
    var result_buffer: [1024 * 1024]u8 = undefined;

    var n: usize = 0;
    while (try walker.next()) |entry| {
        if (verboseTestPrinting) {
            std.debug.warn("\n{}\n", .{entry.basename});
        }
        const contents = try entry.dir.readFileAlloc(allocator, entry.basename, std.math.maxInt(usize));
        defer allocator.free(contents);

        path_buffer.shrink(outputs_dir.len);
        try path_buffer.appendByte(std.fs.path.sep);
        try path_buffer.append(entry.basename);
        const expectedContents = try std.io.readFileAlloc(allocator, path_buffer.toSliceConst());
        defer allocator.free(expectedContents);

        var result_out_stream = std.io.SliceOutStream.init(result_buffer[0..]);
        const result_stream = &result_out_stream.stream;

        var lexer = lex.Lexer.init(contents);
        while (true) {
            const token = lexer.next() catch |e| {
                if (verboseTestPrinting) {
                    std.debug.warn("\n{}\n", .{e});
                }
                break;
            };
            if (verboseTestPrinting) {
                if (printTokenBounds) {
                    std.debug.warn("{}:{}:{}", .{token.start, token.nameForDisplay(), token.end});
                } else {
                    std.debug.warn("{}", .{token.nameForDisplay()});
                }
            }
            try result_stream.print("{}", .{token.nameForDisplay()});
            if (token.id == lex.Token.Id.Eof) {
                break;
            } else {
                if (verboseTestPrinting) {
                    std.debug.warn(" ", .{});
                }
                try result_stream.print(" ", .{});
            }
        }
        if (verboseTestPrinting) {
            std.debug.warn("\nexpected\n{}\n", .{expectedContents});
        }
        var expectedContentsTrimmed = expectedContents;
        if (std.ascii.indexOfIgnoreCase(expectedContents, "\n")) |pos| {
            expectedContentsTrimmed = expectedContents[0..pos];
        }
        std.testing.expectEqualSlices(u8, expectedContentsTrimmed, result_out_stream.getWritten());
        n += 1;
    }
    std.debug.warn("{} input/output pairs checked...", .{n});
}