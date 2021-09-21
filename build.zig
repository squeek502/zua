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

    const fuzzed_lex_inputs_dir_default = "test/corpus/fuzz_llex";
    const fuzzed_lex_outputs_dir_default = "test/output/fuzz_llex";
    const fuzzed_lex_inputs_dir = b.option([]const u8, "fuzzed_lex_inputs_dir", "Directory with input corpus for fuzzed_lex tests") orelse fuzzed_lex_inputs_dir_default;
    const fuzzed_lex_outputs_dir = b.option([]const u8, "fuzzed_lex_outputs_dir", "Directory with expected outputs for fuzzed_lex tests") orelse fuzzed_lex_outputs_dir_default;

    const fuzzed_lex_options = b.addOptions();
    fuzzed_lex_options.addOption([]const u8, "fuzzed_lex_inputs_dir", fuzzed_lex_inputs_dir);
    fuzzed_lex_options.addOption([]const u8, "fuzzed_lex_outputs_dir", fuzzed_lex_outputs_dir);

    var fuzzed_lex_tests = b.addTest("test/fuzzed_lex.zig");
    fuzzed_lex_tests.setBuildMode(mode);
    fuzzed_lex_tests.addOptions("build_options", fuzzed_lex_options);
    fuzzed_lex_tests.addPackagePath("zua", "src/zua.zig");
    const fuzzed_lex_test_step = b.step("fuzzed_lex", "Test lexer against a fuzzed corpus from fuzzing-lua");
    fuzzed_lex_test_step.dependOn(&fuzzed_lex_tests.step);

    var bench_lex_tests = b.addTest("test/bench_lex.zig");
    bench_lex_tests.setBuildMode(.ReleaseFast);
    bench_lex_tests.addOptions("build_options", fuzzed_lex_options);
    bench_lex_tests.addPackagePath("zua", "src/zua.zig");
    const bench_lex_test_step = b.step("bench_lex", "Bench lexer against a fuzzed corpus from fuzzing-lua");
    bench_lex_test_step.dependOn(&bench_lex_tests.step);

    const fuzzed_strings_inputs_dir_default = "test/corpus/fuzz_strings";
    const fuzzed_strings_outputs_dir_default = "test/output/fuzz_strings";
    const fuzzed_strings_gen_dir_default = "test/corpus/fuzz_strings_generated";
    const fuzzed_strings_inputs_dir = b.option([]const u8, "fuzzed_strings_inputs_dir", "Directory with input strings for string parsing tests") orelse fuzzed_strings_inputs_dir_default;
    const fuzzed_strings_outputs_dir = b.option([]const u8, "fuzzed_strings_outputs_dir", "Directory with output strings for string parsing tests") orelse fuzzed_strings_outputs_dir_default;
    const fuzzed_strings_gen_dir = b.option([]const u8, "fuzzed_strings_gen_dir", "Directory to output generated string inputs to") orelse fuzzed_strings_gen_dir_default;

    const fuzzed_strings_options = b.addOptions();
    fuzzed_strings_options.addOption([]const u8, "fuzzed_strings_inputs_dir", fuzzed_strings_inputs_dir);
    fuzzed_strings_options.addOption([]const u8, "fuzzed_strings_outputs_dir", fuzzed_strings_outputs_dir);
    fuzzed_strings_options.addOption([]const u8, "fuzzed_strings_gen_dir", fuzzed_strings_gen_dir);

    var fuzzed_strings = b.addTest("test/fuzzed_strings.zig");
    fuzzed_strings.setBuildMode(mode);
    fuzzed_strings.addOptions("build_options", fuzzed_strings_options);
    fuzzed_strings.addPackagePath("zua", "src/zua.zig");
    const fuzzed_strings_step = b.step("fuzzed_strings", "Test string parsing against a fuzzed corpus from fuzzing-lua");
    fuzzed_strings_step.dependOn(&fuzzed_strings.step);

    const fuzzed_strings_gen_options = b.addOptions();
    fuzzed_strings_gen_options.addOption([]const u8, "fuzzed_lex_inputs_dir", fuzzed_lex_inputs_dir);
    fuzzed_strings_gen_options.addOption([]const u8, "fuzzed_strings_gen_dir", fuzzed_strings_gen_dir);

    var fuzzed_strings_gen = b.addExecutable("fuzzed_strings_gen", "test/fuzzed_strings_gen.zig");
    fuzzed_strings_gen.setBuildMode(mode);
    fuzzed_strings_gen.addOptions("build_options", fuzzed_strings_gen_options);
    fuzzed_strings_gen.addPackagePath("zua", "src/zua.zig");
    const fuzzed_strings_gen_run_step = b.step("fuzzed_strings_gen_run", "Generate string inputs from a fuzzed corpus of lexer inputs");
    fuzzed_strings_gen_run_step.dependOn(&fuzzed_strings_gen.run().step);

    const fuzzed_parse_inputs_dir_default = "test/corpus/fuzz_lparser";
    const fuzzed_parse_outputs_dir_default = "test/output/fuzz_lparser";
    const fuzzed_parse_inputs_dir = b.option([]const u8, "fuzzed_parse_inputs_dir", "Directory with input corpus for fuzzed_parse tests") orelse fuzzed_parse_inputs_dir_default;
    const fuzzed_parse_outputs_dir = b.option([]const u8, "fuzzed_parse_outputs_dir", "Directory with expected outputs for fuzzed_parse tests") orelse fuzzed_parse_outputs_dir_default;

    const fuzzed_parse_options = b.addOptions();
    fuzzed_parse_options.addOption([]const u8, "fuzzed_parse_inputs_dir", fuzzed_parse_inputs_dir);
    fuzzed_parse_options.addOption([]const u8, "fuzzed_parse_outputs_dir", fuzzed_parse_outputs_dir);

    var fuzzed_parse_tests = b.addTest("test/fuzzed_parse.zig");
    fuzzed_parse_tests.setBuildMode(mode);
    fuzzed_parse_tests.addOptions("build_options", fuzzed_parse_options);
    fuzzed_parse_tests.addPackagePath("zua", "src/zua.zig");
    const fuzzed_parse_test_step = b.step("fuzzed_parse", "Test parser against a fuzzed corpus from fuzzing-lua");
    fuzzed_parse_test_step.dependOn(&fuzzed_parse_tests.step);

    var fuzzed_parse_prune = b.addExecutable("fuzzed_parse_prune", "test/fuzzed_parse_prune.zig");
    fuzzed_parse_prune.setBuildMode(mode);
    fuzzed_parse_prune.addOptions("build_options", fuzzed_parse_options);
    fuzzed_parse_prune.addPackagePath("zua", "src/zua.zig");
    const fuzzed_parse_prune_step = b.step("fuzzed_parse_prune", "Prune fuzzed parser corpus to remove lexer-specific error outputs");
    fuzzed_parse_prune_step.dependOn(&fuzzed_parse_prune.run().step);
}
