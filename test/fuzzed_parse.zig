const std = @import("std");
const zua = @import("zua");
const parse = zua.parse;

// Tests for comparing the output of Zua's parser with Lua's.
// Expects @import("build_options").fuzzed_parse_inputs_dir to be a path to
// a directory containing a corpus of inputs to test and
// @import("build_options").fuzzed_parse_outputs_dir to be a path to a
// directory containing the bytecode obtained by running the input
// through the Lua parser and dumper (or an error string).
//
// A usable corpus/outputs pair can be obtained from
// https://github.com/squeek502/fuzzing-lua

const verboseTestPrinting = false;
const printErrorContextDifferences = false;

const build_options = @import("build_options");
const inputs_dir_opt = build_options.fuzzed_parse_inputs_dir;
const outputs_dir_opt = build_options.fuzzed_parse_outputs_dir;

test "fuzzed_parse input/output pairs" {
    const allocator = std.testing.allocator;

    // resolve these now since Zig's std lib on Windows rejects paths with / as the path sep
    const inputs_dir_path = try std.fs.path.resolve(allocator, &[_][]const u8{inputs_dir_opt});
    defer allocator.free(inputs_dir_path);
    const outputs_dir_path = try std.fs.path.resolve(allocator, &[_][]const u8{outputs_dir_opt});
    defer allocator.free(outputs_dir_path);

    var inputs_dir = try std.fs.cwd().openDir(inputs_dir_path, .{ .iterate = true });
    defer inputs_dir.close();
    var outputs_dir = try std.fs.cwd().openDir(outputs_dir_path, .{});
    defer outputs_dir.close();

    var n: usize = 0;
    var nskipped: usize = 0;
    var nlexerrors: usize = 0;
    var nbytecode: usize = 0;
    var inputs_iterator = inputs_dir.iterate();
    while (try inputs_iterator.next()) |entry| {
        if (entry.kind != .File) continue;

        if (verboseTestPrinting) {
            std.debug.warn("\n{s}\n", .{entry.name});
        }

        const contents = try inputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(contents);
        const expectedContents = try outputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(expectedContents);

        // apparently luac can miscompile certain inputs which gives a blank output file
        if (expectedContents.len == 0) {
            if (verboseTestPrinting) {
                std.debug.print("luac miscompilation: {s}\n", .{entry.name});
            }
            continue;
        }

        const is_bytecode_expected = std.mem.startsWith(u8, expectedContents, zua.dump.signature);
        const is_error_expected = !is_bytecode_expected;

        var lexer = zua.lex.Lexer.init(contents, "fuzz");
        var parser = zua.parse.Parser.init(&lexer);
        if (parser.parse(allocator)) |tree| {
            defer tree.deinit();
            if (is_error_expected) {
                std.debug.print("{s}:\n```\n{e}\n```\n\nexpected error:\n{s}\n\ngot tree:\n", .{ entry.name, contents, expectedContents });
                try tree.dump(std.io.getStdErr().writer());
                std.debug.print("\n", .{});
                unreachable;
            } else {
                nbytecode += 1;
                continue;
            }
        } else |err| {
            if (is_bytecode_expected) {
                std.debug.print("{s}:\n```\n{e}\n```\n\nexpected no error, got:\n{}\n", .{ entry.name, contents, err });
                unreachable;
            } else {
                if (isInErrorSet(err, zua.lex.LexError)) {
                    nlexerrors += 1;
                }

                // ignore this error for now, it's in lcode.c
                if (std.mem.indexOf(u8, expectedContents, "function or expression too complex") != null) {
                    nskipped += 1;
                    continue;
                }
                // ignore this error for now, not sure if it'll be a ParseError
                if (std.mem.indexOf(u8, expectedContents, "has more than 200 local variables") != null) {
                    nskipped += 1;
                    continue;
                }
                // ignore this error for now, not sure if it'll be a ParseError
                if (std.mem.indexOf(u8, expectedContents, "variables in assignment") != null) {
                    nskipped += 1;
                    continue;
                }
                // ignore this error for now, see long_str_nesting_compat TODO
                if (std.mem.indexOf(u8, expectedContents, "nesting of [[...]]") != null) {
                    nskipped += 1;
                    continue;
                }

                const err_msg = try parser.renderErrorAlloc(allocator);
                defer allocator.free(err_msg);

                var nearIndex = std.mem.lastIndexOf(u8, expectedContents, " near '");
                if (nearIndex) |i| {
                    std.testing.expectEqualStrings(expectedContents[0..i], err_msg[0..std.math.min(i, err_msg.len)]);
                    if (printErrorContextDifferences) {
                        if (!std.mem.eql(u8, expectedContents, err_msg)) {
                            std.debug.print("\n{s}\nexpected: {e}\nactual: {e}\n", .{ entry.name, expectedContents, err_msg });
                        }
                    }
                } else {
                    std.testing.expectEqualStrings(expectedContents, err_msg);
                }
            }
        }

        n += 1;
    }
    std.debug.warn(
        "\n{} input/output pairs tested: {} passed, {} skipped (skipped {} bytecode outputs, {} unimplemented errors)\n",
        .{ n + nskipped + nbytecode, n, nskipped + nbytecode, nbytecode, nskipped },
    );
    if (nlexerrors > 0) {
        std.debug.print("note: {d} lexer-specific errors found, can prune them with fuzzed_parse_prune\n", .{nlexerrors});
    }
}

pub fn isInErrorSet(err: anyerror, comptime T: type) bool {
    for (std.meta.fields(T)) |field| {
        if (std.mem.eql(u8, std.meta.tagName(err), field.name)) {
            return true;
        }
    }
    return false;
}
