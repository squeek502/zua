const std = @import("std");
const zua = @import("zua");

const build_options = @import("build_options");
const inputs_dir_path = build_options.lua51_tests_inputs_dir;

test "parsing lua 5.1 test files" {
    const allocator = std.testing.allocator;

    var inputs_dir = try std.fs.cwd().openDir(inputs_dir_path, .{ .iterate = true });
    defer inputs_dir.close();

    var inputs_iterator = inputs_dir.iterate();
    while (try inputs_iterator.next()) |entry| {
        if (entry.kind != .File) continue;
        if (!std.mem.eql(u8, ".lua", std.fs.path.extension(entry.name))) continue;

        const contents = try inputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(contents);

        var source_contents = contents;

        // need to skip past the first line if this is true, since it relies on a special case
        // that skips the first line if it starts with # in luaL_loadfile
        if (source_contents[0] == '#') {
            // no newline after a # means we should just skip the file
            const newline_index = std.mem.indexOfScalar(u8, source_contents, '\n') orelse continue;
            source_contents = source_contents[newline_index..];
        }

        std.debug.print("{s}\n", .{entry.name});

        var lexer = zua.lex.Lexer.init(source_contents, entry.name);
        var parser = zua.parse.Parser.init(&lexer);
        if (parser.parse(allocator)) |tree| {
            defer tree.deinit();
            //try tree.dump(std.io.getStdErr().writer());
        } else |err| {
            const err_msg = try parser.renderErrorAlloc(allocator);
            defer allocator.free(err_msg);

            std.debug.print("{s}\n", .{err_msg});
            return err;
        }
    }
}
