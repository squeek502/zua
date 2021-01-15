const std = @import("std");
const lex = @import("zua").lex; // this import relies on addPackagePath in ../build.zig
const Timer = std.time.Timer;
const hash_map = std.hash_map;

// Benchmarking for the Zua lexer
// Expects @import("build_options").fuzz_lex_inputs_dir to be a path to
// a directory containing a corpus of inputs to test. Such a corpus can
// be obtained from https://github.com/squeek502/fuzzing-lua

var timer: Timer = undefined;

test "bench fuzz_llex inputs" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_allocator.deinit();
    var allocator = &arena_allocator.allocator;

    timer = try Timer.start();

    const build_options = @import("build_options");
    const inputs_dir_opt = build_options.fuzzed_lex_inputs_dir;
    // resolve this now since Zig's std lib on Windows rejects paths with / as the path sep
    const inputs_dir = try std.fs.path.resolve(allocator, &[_][]const u8{inputs_dir_opt});

    std.debug.warn("Mode: {}\n", .{@import("builtin").mode});
    var walker = try std.fs.walkPath(allocator, inputs_dir);
    defer walker.deinit();
    var n: usize = 0;
    var time: u64 = 0;
    const num_iterations = 1000;
    while (try walker.next()) |entry| {
        const contents = try entry.dir.readFileAlloc(allocator, entry.basename, std.math.maxInt(usize));
        defer allocator.free(contents);

        beginMeasure();
        var iteration: usize = 0;
        while (iteration < num_iterations) : (iteration += 1) {
            var lexer = lex.Lexer.init(contents, allocator, "bench");
            defer lexer.deinit();
            while (true) {
                const token = lexer.next() catch |e| {
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
    std.debug.warn("Lexed {} files in {}ns ({d}ms)\n", .{ n, time, @intToFloat(f64, time) / (std.time.ns_per_s / std.time.ms_per_s) });
}

fn beginMeasure() void {
    timer.reset();
}

fn endMeasure(iterations: usize) u64 {
    const elapsed_ns = timer.read();
    return elapsed_ns / iterations;
}
