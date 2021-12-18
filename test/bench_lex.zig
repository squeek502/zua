const std = @import("std");
const lex = @import("zua").lex; // this import relies on addPackagePath in ../build.zig
const Timer = std.time.Timer;
const hash_map = std.hash_map;

// Benchmarking for the Zua lexer
// Expects @import("build_options").fuzz_lex_inputs_dir to be a path to
// a directory containing a corpus of inputs to test. Such a corpus can
// be obtained from https://github.com/squeek502/fuzzing-lua

var timer: Timer = undefined;
const build_options = @import("build_options");
const inputs_dir_path = build_options.fuzzed_lex_inputs_dir;

test "bench fuzz_llex inputs" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    var allocator = arena_allocator.allocator();

    timer = try Timer.start();

    var inputs_dir = try std.fs.cwd().openDir(inputs_dir_path, .{ .iterate = true });
    defer inputs_dir.close();

    std.debug.print("Mode: {}\n", .{@import("builtin").mode});
    var n: usize = 0;
    var time: u64 = 0;
    const num_iterations = 1000;
    var inputs_iterator = inputs_dir.iterate();
    while (try inputs_iterator.next()) |entry| {
        if (entry.kind != .File) continue;

        const contents = try inputs_dir.readFileAlloc(allocator, entry.name, std.math.maxInt(usize));
        defer allocator.free(contents);

        beginMeasure();
        var iteration: usize = 0;
        while (iteration < num_iterations) : (iteration += 1) {
            var lexer = lex.Lexer.init(contents, "bench");
            while (true) {
                const token = lexer.next() catch {
                    break;
                };
                if (token.id == lex.Token.Id.eof) {
                    break;
                }
            }
        }
        time += endMeasure(num_iterations);
        n += 1;
    }
    std.debug.print("Lexed {} files in {}ns ({d}ms)\n", .{ n, time, @intToFloat(f64, time) / (std.time.ns_per_s / std.time.ms_per_s) });
}

fn beginMeasure() void {
    timer.reset();
}

fn endMeasure(iterations: usize) u64 {
    const elapsed_ns = timer.read();
    return elapsed_ns / iterations;
}
