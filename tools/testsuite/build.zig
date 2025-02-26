const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    // const optimize = b.standardOptimizeOption(.{});

    const run = b.step("run", "Run");

    const testsuite_dep = b.dependency("github:WebAssembly/testsuite", .{
        .target = target,
        .optimize = .ReleaseFast,
    });

    const wabt_dep = b.dependency("wabt", .{
        .target = target,
        .optimize = .ReleaseFast,
    });

    const wast2json_exe = wabt_dep.artifact("wast2json");

    const wast_json_runner_exe = addWastJsonRunner(b, .{
        .target = target,
        .optimize = .Debug,
    });

    for (base_filenames) |base_filename| {
        const run_wast2json_exe = b.addRunArtifact(wast2json_exe);

        const wast_path = try std.fmt.allocPrint(b.allocator, "{s}.wast", .{base_filename});

        run_wast2json_exe.addFileArg(testsuite_dep.path(wast_path));
        run_wast2json_exe.addArg("-o");
        const wast_json_path = run_wast2json_exe.addOutputFileArg(try std.fmt.allocPrint(b.allocator, "{s}.json", .{base_filename}));

        const run_wast_json_runner_exe = b.addRunArtifact(wast_json_runner_exe);

        run_wast_json_runner_exe.addFileArg(wast_json_path);

        run_wast_json_runner_exe.step.dependOn(&run_wast2json_exe.step);

        run.dependOn(&run_wast_json_runner_exe.step);
    }
}

const base_filenames = [_][]const u8{
    // "f32",
    // "f32_bitwise",
    // "f32_cmp",
    // "f64",
    // "f64_cmp",
    // "f64_bitwise",
    // "i32",
    // "i64",
    // "conversions",
    // "local_get",
    "block",
};

const AddWastJsonRunner = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
};

fn addWastJsonRunner(b: *std.Build, options: AddWastJsonRunner) *std.Build.Step.Compile {
    const exe = b.addExecutable(.{
        .name = "wast-json-runner",
        .target = options.target,
        .optimize = options.optimize,
        .root_source_file = b.path("src/wast_json_runner.zig"),
    });

    b.installArtifact(exe);

    const wasmrt_dep = b.dependency("jaune:wasm-runtime", .{});

    exe.root_module.addImport("jaune:wasm-runtime", wasmrt_dep.module("root"));

    return exe;
}
