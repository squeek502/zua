const std = @import("std");
const lex = @import("zua").lex;

// Code for generating a potentially huge collection of
// files containing the source of every string literal token
// in the corpus provided in @import("build_options").fuzzed_lex_inputs_dir
// and outputting them to @import("build_options").fuzzed_strings_gen_dir
//
// This is a building block for use later with fuzzed_strings.zig,
// after minimizing/generating outputs with https://github.com/squeek502/fuzzing-lua

const build_options = @import("build_options");
const inputs_dir_opt = build_options.fuzzed_lex_inputs_dir;
const outputs_dir_opt = build_options.fuzzed_strings_gen_dir;

pub fn main() !void {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    var allocator = &arena_allocator.allocator;

    // resolve these now since Zig's std lib on Windows rejects paths with / as the path sep
    const inputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{inputs_dir_opt});
    const outputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{outputs_dir_opt});

    // clean the outputs dir
    std.fs.cwd().deleteTree(outputs_dir) catch |err| switch (err) {
        error.NotDir => {},
        else => |e| return e,
    };
    try std.fs.cwd().makePath(outputs_dir);

    var walker = try std.fs.walkPath(allocator, inputs_dir);
    defer walker.deinit();
    var path_buffer = try std.ArrayList(u8).initCapacity(allocator, outputs_dir.len);
    path_buffer.appendSliceAssumeCapacity(outputs_dir);
    defer path_buffer.deinit();
    var result_buffer: [1024 * 1024]u8 = undefined;

    var n: usize = 0;
    while (try walker.next()) |entry| {
        const contents = try entry.dir.readFileAlloc(allocator, entry.basename, std.math.maxInt(usize));
        defer allocator.free(contents);

        var lexer = lex.Lexer.init(contents, "fuzz");
        while (true) {
            const token = lexer.next() catch |e| {
                break;
            };
            if (token.id == lex.Token.Id.eof) break;
            if (token.id != lex.Token.Id.string) continue;

            path_buffer.shrinkRetainingCapacity(outputs_dir.len);
            try path_buffer.append(std.fs.path.sep);
            var buffer_writer = path_buffer.writer();
            try buffer_writer.print("{}", .{n});

            try std.fs.cwd().writeFile(path_buffer.items, contents[token.start..token.end]);

            n += 1;
            if (n % 100 == 0) {
                std.debug.warn("{}...\r", .{n});
            }
        }
    }
    std.debug.warn("{} files written to '{s}'\n", .{ n, outputs_dir });
}
