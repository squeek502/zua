const std = @import("std");

pub fn build(b: *std.Build) void {
    const mode = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const exe = b.addExecutable(.{
        .name = "zua",
        .root_source_file = .{ .path = "src/zua.zig" },
        .target = target,
        .optimize = mode,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const zua = b.addModule("zua", .{
        .root_source_file = .{ .path = "src/zua.zig" },
        .target = target,
        .optimize = mode,
    });
    const zuatest = b.createModule(.{
        .root_source_file = .{ .path = "test/lib.zig" },
        .target = target,
        .optimize = mode,
    });
    zuatest.addIncludePath(.{ .path = "lua-5.1/src" });

    const lualib = addLuaLibrary(b, mode, target);

    var tests = b.addTest(.{
        .root_source_file = .{ .path = "src/zua.zig" },
        .target = target,
        .optimize = mode,
    });
    tests.linkLibrary(lualib);
    tests.root_module.addImport("zuatest", zuatest);
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests.step);

    var testlib_test = b.addTest(.{
        .name = "testlib",
        .root_source_file = .{ .path = "test/lib.zig" },
        .target = target,
        .optimize = mode,
    });
    testlib_test.linkLibrary(lualib);
    testlib_test.addIncludePath(.{ .path = "lua-5.1/src" });
    const run_testlib_test = b.addRunArtifact(testlib_test);
    const testlib_test_step = b.step("testlib", "Run test library tests");
    testlib_test_step.dependOn(&run_testlib_test.step);

    const fuzzed_lex_inputs_dir_default = "test/corpus/fuzz_llex";
    const fuzzed_lex_outputs_dir_default = "test/output/fuzz_llex";
    const fuzzed_lex_inputs_dir = b.option([]const u8, "fuzzed_lex_inputs_dir", "Directory with input corpus for fuzzed_lex tests") orelse fuzzed_lex_inputs_dir_default;
    const fuzzed_lex_outputs_dir = b.option([]const u8, "fuzzed_lex_outputs_dir", "Directory with expected outputs for fuzzed_lex tests") orelse fuzzed_lex_outputs_dir_default;

    const fuzzed_lex_options = b.addOptions();
    fuzzed_lex_options.addOption([]const u8, "fuzzed_lex_inputs_dir", fuzzed_lex_inputs_dir);
    fuzzed_lex_options.addOption([]const u8, "fuzzed_lex_outputs_dir", fuzzed_lex_outputs_dir);

    var fuzzed_lex_tests = b.addTest(.{
        .name = "test-fuzzed_lex",
        .root_source_file = .{ .path = "test/fuzzed_lex.zig" },
        .target = target,
        .optimize = mode,
    });
    fuzzed_lex_tests.root_module.addOptions("build_options", fuzzed_lex_options);
    fuzzed_lex_tests.root_module.addImport("zua", zua);
    const run_fuzzed_lex_tests = b.addRunArtifact(fuzzed_lex_tests);
    const fuzzed_lex_test_step = b.step("fuzzed_lex", "Test lexer against a fuzzed corpus from fuzzing-lua");
    fuzzed_lex_test_step.dependOn(&run_fuzzed_lex_tests.step);

    var bench_lex_tests = b.addTest(.{
        .name = "bench_lex",
        .root_source_file = .{ .path = "test/bench_lex.zig" },
        .target = target,
        .optimize = .ReleaseFast,
    });
    bench_lex_tests.root_module.addOptions("build_options", fuzzed_lex_options);
    bench_lex_tests.root_module.addImport("zua", zua);
    const run_bench_lex_tests = b.addRunArtifact(bench_lex_tests);
    const bench_lex_test_step = b.step("bench_lex", "Bench lexer against a fuzzed corpus from fuzzing-lua");
    bench_lex_test_step.dependOn(&run_bench_lex_tests.step);

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

    var fuzzed_strings = b.addTest(.{
        .name = "test-fuzzed_strings",
        .root_source_file = .{ .path = "test/fuzzed_strings.zig" },
        .target = target,
        .optimize = mode,
    });
    fuzzed_strings.root_module.addOptions("build_options", fuzzed_strings_options);
    fuzzed_strings.root_module.addImport("zua", zua);
    const run_fuzzed_strings = b.addRunArtifact(fuzzed_strings);
    const fuzzed_strings_step = b.step("fuzzed_strings", "Test string parsing against a fuzzed corpus from fuzzing-lua");
    fuzzed_strings_step.dependOn(&run_fuzzed_strings.step);

    const fuzzed_strings_gen_options = b.addOptions();
    fuzzed_strings_gen_options.addOption([]const u8, "fuzzed_lex_inputs_dir", fuzzed_lex_inputs_dir);
    fuzzed_strings_gen_options.addOption([]const u8, "fuzzed_strings_gen_dir", fuzzed_strings_gen_dir);

    var fuzzed_strings_gen = b.addExecutable(.{
        .name = "fuzzed_strings_gen",
        .root_source_file = .{ .path = "test/fuzzed_strings_gen.zig" },
        .target = target,
        .optimize = mode,
    });
    fuzzed_strings_gen.root_module.addOptions("build_options", fuzzed_strings_gen_options);
    fuzzed_strings_gen.root_module.addImport("zua", zua);
    const run_fuzzed_strings_gen = b.addRunArtifact(fuzzed_strings_gen);
    const fuzzed_strings_gen_step = b.step("fuzzed_strings_gen", "Build fuzzed_strings_gen binary (but don't run it)");
    fuzzed_strings_gen_step.dependOn(&fuzzed_strings_gen.step);
    const fuzzed_strings_gen_run_step = b.step("fuzzed_strings_gen_run", "Generate string inputs from a fuzzed corpus of lexer inputs");
    fuzzed_strings_gen_run_step.dependOn(&run_fuzzed_strings_gen.step);

    const fuzzed_parse_inputs_dir_default = "test/corpus/fuzz_lparser";
    const fuzzed_parse_outputs_dir_default = "test/output/fuzz_lparser";
    const fuzzed_parse_inputs_dir = b.option([]const u8, "fuzzed_parse_inputs_dir", "Directory with input corpus for fuzzed_parse tests") orelse fuzzed_parse_inputs_dir_default;
    const fuzzed_parse_outputs_dir = b.option([]const u8, "fuzzed_parse_outputs_dir", "Directory with expected outputs for fuzzed_parse tests") orelse fuzzed_parse_outputs_dir_default;

    const fuzzed_parse_options = b.addOptions();
    fuzzed_parse_options.addOption([]const u8, "fuzzed_parse_inputs_dir", fuzzed_parse_inputs_dir);
    fuzzed_parse_options.addOption([]const u8, "fuzzed_parse_outputs_dir", fuzzed_parse_outputs_dir);

    var fuzzed_parse_tests = b.addTest(.{
        .name = "test-fuzzed_parse",
        .root_source_file = .{ .path = "test/fuzzed_parse.zig" },
        .target = target,
        .optimize = mode,
    });
    fuzzed_parse_tests.root_module.addOptions("build_options", fuzzed_parse_options);
    fuzzed_parse_tests.root_module.addImport("zua", zua);
    const run_fuzzed_parse_tests = b.addRunArtifact(fuzzed_parse_tests);
    const fuzzed_parse_test_step = b.step("fuzzed_parse", "Test parser against a fuzzed corpus from fuzzing-lua");
    fuzzed_parse_test_step.dependOn(&run_fuzzed_parse_tests.step);

    var fuzzed_parse_prune = b.addExecutable(.{
        .name = "fuzzed_parse_prune",
        .root_source_file = .{ .path = "test/fuzzed_parse_prune.zig" },
        .target = target,
        .optimize = mode,
    });
    fuzzed_parse_prune.root_module.addOptions("build_options", fuzzed_parse_options);
    fuzzed_parse_prune.root_module.addImport("zua", zua);
    const run_fuzzed_parse_prune = b.addRunArtifact(fuzzed_parse_prune);
    const fuzzed_parse_prune_step = b.step("fuzzed_parse_prune", "Prune fuzzed parser corpus to remove lexer-specific error outputs");
    fuzzed_parse_prune_step.dependOn(&run_fuzzed_parse_prune.step);

    const lua51_tests_inputs_dir_default = "lua-5.1/test";
    const lua51_tests_inputs_dir = b.option([]const u8, "lua51_tests_inputs_dir", "Directory with the PUC Lua test files") orelse lua51_tests_inputs_dir_default;

    const lua51_tests_options = b.addOptions();
    lua51_tests_options.addOption([]const u8, "lua51_tests_inputs_dir", lua51_tests_inputs_dir);

    var lua51_tests = b.addTest(.{
        .name = "test-lua51tests",
        .root_source_file = .{ .path = "test/lua51_tests.zig" },
        .target = target,
        .optimize = mode,
    });
    lua51_tests.root_module.addOptions("build_options", lua51_tests_options);
    lua51_tests.root_module.addImport("zua", zua);
    const run_lua51_tests = b.addRunArtifact(lua51_tests);
    const lua51_tests_step = b.step("lua51_tests", "Run tests using the Lua 5.1 test files");
    lua51_tests_step.dependOn(&run_lua51_tests.step);
}

fn addLuaLibrary(b: *std.Build, mode: std.builtin.OptimizeMode, target: std.Build.ResolvedTarget) *std.Build.Step.Compile {
    var lualib = b.addStaticLibrary(.{
        .name = "lua",
        .target = target,
        .optimize = mode,
    });
    lualib.linkLibC();
    lualib.addIncludePath(.{ .path = "lua-5.1/src" });
    lualib.addCSourceFiles(.{
        .root = .{ .path = "lua-5.1/src" },
        .files = &.{
            "lapi.c",
            "lcode.c",
            "ldebug.c",
            "ldo.c",
            "ldump.c",
            "lfunc.c",
            "lgc.c",
            "llex.c",
            "lmem.c",
            "lobject.c",
            "lopcodes.c",
            "lparser.c",
            "lstate.c",
            "lstring.c",
            "ltable.c",
            "ltm.c",
            "lundump.c",
            "lvm.c",
            "lzio.c",
            "lauxlib.c",
            "lbaselib.c",
            "ldblib.c",
            "liolib.c",
            "lmathlib.c",
            "loslib.c",
            "ltablib.c",
            "lstrlib.c",
            "loadlib.c",
            "linit.c",
        },
        .flags = &.{"-std=gnu99"},
    });

    return lualib;
}
