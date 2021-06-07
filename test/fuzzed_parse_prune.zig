const std = @import("std");
const zua = @import("zua");
const parse = zua.parse;

// Prunes the input/output pairs used by fuzzed_parse to exclude inputs
// that cause a lexer error (as opposed to a parser error), since those
// pairs will just be overlap with the fuzzed_lex corpus.
//
// Expects @import("build_options").fuzzed_parse_inputs_dir to be a path to
// a directory containing a corpus of inputs to test and
// @import("build_options").fuzzed_parse_outputs_dir to be a path to a
// directory containing the bytecode obtained by running the input
// through the Lua parser and dumper (or an error string).

const build_options = @import("build_options");
const inputs_dir_path = build_options.fuzzed_parse_inputs_dir;
const outputs_dir_path = build_options.fuzzed_parse_outputs_dir;

pub fn main() !void {
    const allocator = std.testing.allocator;

    var inputs_dir = try std.fs.cwd().openDir(inputs_dir_path, .{ .iterate = true });
    defer inputs_dir.close();
    var outputs_dir = try std.fs.cwd().openDir(outputs_dir_path, .{});
    defer outputs_dir.close();

    var paths_to_remove = std.ArrayList([]const u8).init(allocator);
    defer paths_to_remove.deinit();

    var inputs_iterator = inputs_dir.iterate();
    while (try inputs_iterator.next()) |entry| {
        if (entry.kind != .File) continue;

        const contents = try inputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(contents);

        const expectedContents = try outputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(expectedContents);

        // apparently luac can miscompile certain inputs which gives a blank output file
        if (expectedContents.len == 0) {
            continue;
        }

        const is_bytecode_expected = std.mem.startsWith(u8, expectedContents, zua.dump.signature);
        if (is_bytecode_expected) {
            continue;
        }

        var lexer = zua.lex.Lexer.init(contents, "fuzz");
        var parser = zua.parse.Parser.init(&lexer);
        if (parser.parse(allocator)) |tree| {
            tree.deinit();
        } else |err| {
            if (isInErrorSet(err, zua.lex.LexError)) {
                const duped_path = try allocator.dupe(u8, entry.name);
                try paths_to_remove.append(duped_path);
            }
        }
    }

    std.debug.print("removing {d} pairs...", .{paths_to_remove.items.len});

    for (paths_to_remove.items) |path_to_remove| {
        inputs_dir.deleteFile(path_to_remove) catch |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        };
        outputs_dir.deleteFile(path_to_remove) catch |err| switch (err) {
            error.FileNotFound => {},
            else => return err,
        };
    }

    std.debug.print(" done\n", .{});
}

pub fn isInErrorSet(err: anyerror, comptime T: type) bool {
    for (std.meta.fields(T)) |field| {
        if (std.mem.eql(u8, std.meta.tagName(err), field.name)) {
            return true;
        }
    }
    return false;
}
