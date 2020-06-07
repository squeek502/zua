const std = @import("std");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zua", "src/zua.zig");
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    var tests = b.addTest("src/zua.zig");
    tests.setBuildMode(mode);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&tests.step);

    const fuzz_lex_inputs_dir_default = "test/corpus/fuzz_llex";
    const fuzz_lex_outputs_dir_default = "test/output/fuzz_llex";
    const fuzz_lex_inputs_dir = b.option([]const u8, "fuzz_lex_inputs_dir", "Directory with input corpus for fuzz_lex tests") orelse fuzz_lex_inputs_dir_default;
    const fuzz_lex_outputs_dir = b.option([]const u8, "fuzz_lex_outputs_dir", "Directory with expected outputs for fuzz_lex tests") orelse fuzz_lex_outputs_dir_default;

    var fuzz_lex_tests = b.addTest("test/fuzz_lex.zig");
    fuzz_lex_tests.setBuildMode(mode);
    fuzz_lex_tests.addBuildOption([]const u8, "fuzz_lex_inputs_dir", b.fmt("\"{}\"", .{fuzz_lex_inputs_dir}));
    fuzz_lex_tests.addBuildOption([]const u8, "fuzz_lex_outputs_dir", b.fmt("\"{}\"", .{fuzz_lex_outputs_dir}));
    fuzz_lex_tests.addPackagePath("zua", "src/zua.zig");
    const fuzz_lex_test_step = b.step("fuzz_lex", "Test lexer against a fuzzed corpus from fuzzing-lua");
    fuzz_lex_test_step.dependOn(&fuzz_lex_tests.step);

    var bench_lex_tests = b.addTest("test/bench_lex.zig");
    bench_lex_tests.setBuildMode(.ReleaseFast);
    bench_lex_tests.addBuildOption([]const u8, "fuzz_lex_inputs_dir", b.fmt("\"{}\"", .{fuzz_lex_inputs_dir}));
    bench_lex_tests.addPackagePath("zua", "src/zua.zig");
    const bench_lex_test_step = b.step("bench_lex", "Bench lexer against a fuzzed corpus from fuzzing-lua");
    bench_lex_test_step.dependOn(&bench_lex_tests.step);

    const fuzz_strings_inputs_dir_default = "test/corpus/fuzz_strings";
    const fuzz_strings_outputs_dir_default = "test/output/fuzz_strings";
    const fuzz_strings_gen_dir_default = "test/corpus/fuzz_strings_generated";
    const fuzz_strings_inputs_dir = b.option([]const u8, "fuzz_strings_inputs_dir", "Directory with input strings for string parsing tests") orelse fuzz_strings_inputs_dir_default;
    const fuzz_strings_outputs_dir = b.option([]const u8, "fuzz_strings_outputs_dir", "Directory with output strings for string parsing tests") orelse fuzz_strings_outputs_dir_default;
    const fuzz_strings_gen_dir = b.option([]const u8, "fuzz_strings_gen_dir", "Directory to output generated string inputs to") orelse fuzz_strings_gen_dir_default;

    var fuzz_strings = b.addTest("test/fuzz_strings.zig");
    fuzz_strings.setBuildMode(mode);
    fuzz_strings.addBuildOption([]const u8, "fuzz_strings_inputs_dir", b.fmt("\"{}\"", .{fuzz_strings_inputs_dir}));
    fuzz_strings.addBuildOption([]const u8, "fuzz_strings_outputs_dir", b.fmt("\"{}\"", .{fuzz_strings_outputs_dir}));
    fuzz_strings.addPackagePath("zua", "src/zua.zig");
    const fuzz_strings_step = b.step("fuzz_strings", "Test string parsing against a fuzzed corpus from fuzzing-lua");
    fuzz_strings_step.dependOn(&fuzz_strings.step);

    var fuzz_strings_gen = b.addExecutable("fuzz_strings_gen", "test/fuzz_strings_gen.zig");
    fuzz_strings_gen.setBuildMode(mode);
    fuzz_strings_gen.addBuildOption([]const u8, "fuzz_lex_inputs_dir", b.fmt("\"{}\"", .{fuzz_lex_inputs_dir}));
    fuzz_strings_gen.addBuildOption([]const u8, "fuzz_strings_gen_dir", b.fmt("\"{}\"", .{fuzz_strings_gen_dir}));
    fuzz_strings_gen.addPackagePath("zua", "src/zua.zig");

    const fuzz_strings_gen_run_step = b.step("fuzz_strings_gen_run", "Generate string inputs from a fuzzed corpus of lexer inputs");
    fuzz_strings_gen_run_step.dependOn(&fuzz_strings_gen.run().step);
}
