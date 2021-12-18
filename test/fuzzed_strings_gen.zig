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
const inputs_dir_path = build_options.fuzzed_lex_inputs_dir;
const outputs_dir_path = build_options.fuzzed_strings_gen_dir;

pub fn main() !void {
    var allocator = std.testing.allocator;

    // clean the outputs dir
    std.fs.cwd().deleteTree(outputs_dir_path) catch |err| switch (err) {
        error.NotDir => {},
        else => |e| return e,
    };
    try std.fs.cwd().makePath(outputs_dir_path);

    var inputs_dir = try std.fs.cwd().openDir(inputs_dir_path, .{ .iterate = true });
    defer inputs_dir.close();
    var outputs_dir = try std.fs.cwd().openDir(outputs_dir_path, .{});
    defer outputs_dir.close();

    var n: usize = 0;
    var inputs_iterator = inputs_dir.iterate();
    while (try inputs_iterator.next()) |entry| {
        if (entry.kind != .File) continue;

        const contents = try inputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(contents);

        var lexer = lex.Lexer.init(contents, "fuzz");
        while (true) {
            const token = lexer.next() catch {
                break;
            };
            if (token.id == lex.Token.Id.eof) break;
            if (token.id != lex.Token.Id.string) continue;

            try outputs_dir.writeFile(entry.name, contents[token.start..token.end]);

            n += 1;
            if (n % 100 == 0) {
                std.debug.print("{}...\r", .{n});
            }
        }
    }
    std.debug.print("{} files written to '{s}'\n", .{ n, outputs_dir_path });
}
