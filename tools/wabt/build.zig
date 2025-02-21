const std = @import("std");

const AddToolExecutableOptions = struct {
    name: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    root_source_file: []const u8,
};

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    {
        const wat2wasm_exe = addToolExecutable(b, .{
            .name = "wat2wasm",
            .target = target,
            .optimize = optimize,
            .root_source_file = "./tools/wat2wasm.cc",
        });

        const run = b.step("run_wat2wasm", "Run wat2wasm");
        const run_exe = b.addRunArtifact(wat2wasm_exe);

        run_exe.step.dependOn(b.getInstallStep());

        if (b.args) |args| {
            run_exe.addArgs(args);
        }

        run.dependOn(&run_exe.step);
    }

    {
        const wast2json_exe = addToolExecutable(b, .{
            .name = "wast2json",
            .target = target,
            .optimize = optimize,
            .root_source_file = "./tools/wast2json.cc",
        });
        const run = b.step("run_wast2json", "Run wast2json");

        const run_exe = b.addRunArtifact(wast2json_exe);

        run_exe.step.dependOn(b.getInstallStep());

        if (b.args) |args| {
            run_exe.addArgs(args);
        }
        run.dependOn(&run_exe.step);
    }
}

fn addToolExecutable(b: *std.Build, options: AddToolExecutableOptions) *std.Build.Step.Compile {
    const exe = b.addExecutable(.{
        .name = options.name,
        .target = options.target,
        .optimize = options.optimize,
    });

    b.installArtifact(exe);

    const picosha2_sources_dep = b.dependency("picosha2:sources", .{});

    exe.root_module.addIncludePath(picosha2_sources_dep.path("./picosha2.h").dirname());

    const wabt_sources_dep = b.dependency("wabt:sources", .{});

    exe.root_module.addCMacro("gnu_printf", "printf");

    exe.root_module.addCSourceFiles(.{
        .root = wabt_sources_dep.path("src"),
        .files = &[_][]const u8{
            "./opcode-code-table.c",
        },
        .flags = &[_][]const u8{
            "-std=c17",
            "-Werror",
        },
    });

    exe.root_module.addCSourceFiles(.{
        .root = wabt_sources_dep.path("src"),
        .files = &[_][]const u8{
            options.root_source_file,

            "./apply-names.cc",
            "./binary-reader-ir.cc",
            "./binary-reader-logging.cc",
            "./binary-reader-objdump.cc",
            "./binary-reader-stats.cc",
            "./binary-reader.cc",
            "./binary-writer-spec.cc",
            "./binary-writer.cc",
            "./binary.cc",
            "./binding-hash.cc",

            "./color.cc",
            "./common.cc",
            "./config.cc",
            "./decompiler.cc",
            "./emscripten-helpers.cc",
            "./error-formatter.cc",
            "./expr-visitor.cc",
            "./feature.cc",
            "./filenames.cc",
            "./generate-names.cc",
            "./ir-util.cc",
            "./ir.cc",
            "./leb128.cc",
            "./lexer-source-line-finder.cc",
            "./lexer-source.cc",
            "./literal.cc",
            "./opcode.cc",
            "./option-parser.cc",
            "./resolve-names.cc",
            "./sha256.cc",
            "./shared-validator.cc",
            "./stream.cc",
            "./token.cc",
            "./tracing.cc",
            "./type-checker.cc",
            "./utf8.cc",
            "./validator.cc",
            "./wast-lexer.cc",
            "./wast-parser.cc",
            "./wat-writer.cc",
        },
        .flags = &[_][]const u8{
            "-std=c++17",
            "-Werror",
        },
    });

    exe.root_module.addIncludePath(wabt_sources_dep.path("include"));

    const wabt_config_h = b.addConfigHeader(
        .{
            .include_path = "wabt/config.h",
            .style = .{
                .cmake = wabt_sources_dep.path("src/config.h.in"),
            },
        },
        .{
            .WABT_VERSION_STRING = "1.0.36",
            .COMPILER_IS_CLANG = 1,
            .COMPILER_IS_GNU = 0,
            .COMPILER_IS_MSVC = 0,
            .HAVE_SNPRINTF = 1,
            .HAVE_STRCASECMP = 1,
            .HAVE_SSIZE_T = 1,
            .HAVE_ALLOCA_H = 0,
        },
    );

    exe.root_module.addConfigHeader(wabt_config_h);

    exe.linkLibCpp();

    return exe;
}

// fn cleanupTempFiles(step: *std.Build.Step, node: *std.Progress.Node) anyerror!void {
//     _ = node; // autofix
//     _ = step; // autofix
//     std.log.debug("cleanup", .{});
// }
