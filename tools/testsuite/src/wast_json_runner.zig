const std = @import("std");

const wasm = @import("jaune:wasm-runtime");
const runtime = wasm.runtime;
const module = wasm.module;

const JsonWast = struct {
    const ExpectedValue = struct {
        type: []u8,
        value: ?[]u8 = null,

        fn eqlRuntimeValue(self: ExpectedValue, other: runtime.Value) !bool {
            if (self.value) |value| {
                if (std.mem.eql(u8, value, "nan:arithmetic")) {
                    return switch (other) {
                        .f32 => |v| std.math.isNan(v),
                        .f64 => |v| std.math.isNan(v),
                        else => error.UnsupportedType,
                    };
                } else if (std.mem.eql(u8, value, "nan:canonical")) {
                    return switch (other) {
                        .f32 => |v| std.math.isNan(v),
                        .f64 => |v| std.math.isNan(v),
                        else => error.UnsupportedType,
                    };
                } else {
                    const c = try (Value{
                        .type = self.type,
                        .value = value,
                    }).toRuntimeValue();

                    return c.eql(other);
                }
            } else {
                return false;
            }
        }
    };

    const Value = struct {
        type: []u8,
        value: []u8,

        fn toRuntimeValue(self: Value) !runtime.Value {
            if (std.mem.eql(u8, self.type, "f32")) {
                const parsed = try std.fmt.parseUnsigned(u32, self.value, 10);

                return .{
                    .f32 = @as(f32, @bitCast(parsed)),
                };
            }
            if (std.mem.eql(u8, self.type, "f64")) {
                const parsed = try std.fmt.parseUnsigned(u64, self.value, 10);

                return .{
                    .f64 = @as(f64, @bitCast(parsed)),
                };
            }
            if (std.mem.eql(u8, self.type, "i32")) {
                const parsed = try std.fmt.parseUnsigned(u32, self.value, 10);

                return .{
                    .i32 = @as(i32, @bitCast(parsed)),
                };
            }
            if (std.mem.eql(u8, self.type, "i64")) {
                const parsed = try std.fmt.parseUnsigned(u64, self.value, 10);

                return .{
                    .i64 = @as(i64, @bitCast(parsed)),
                };
            }

            return error.UnsupportedValue;
        }
    };

    const Action = struct {
        type: []u8,
        field: []u8,
        args: []Value,
    };

    const Command = struct {
        type: []u8,
        line: u32,
        filename: ?[]u8 = null,

        text: ?[]u8 = null,
        module_type: ?[]u8 = null,

        action: ?Action = null,
        expected: ?[]ExpectedValue = null,
    };

    commands: []Command,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip(); // skip arg 0

    const input_path = args.next() orelse {
        return error.MissingInputPathArgument;
    };

    const input_dir = std.fs.path.dirname(input_path) orelse {
        return error.NoInputDirectory;
    };

    std.log.info("input_path: {s}", .{input_path});

    const input_file = try std.fs.cwd().openFile(input_path, .{});
    defer input_file.close();

    var diagnostics = std.json.Diagnostics{};

    var scanner = std.json.reader(allocator, input_file.reader());
    defer scanner.deinit();

    scanner.enableDiagnostics(&diagnostics);

    var input_parsed = std.json.parseFromTokenSource(JsonWast, allocator, &scanner, .{
        .ignore_unknown_fields = true,
    }) catch |e| {
        std.log.err("{s}:{d}:{d}: {}", .{ input_path, diagnostics.getLine(), diagnostics.getColumn(), e });
        return error.JsonWastParsingFail;
    };
    defer input_parsed.deinit();

    var current_module: ?module.Module = null;
    errdefer {
        if (current_module) |*mod| {
            mod.deinit();
        }
    }

    for (input_parsed.value.commands) |command| {
        if (std.mem.eql(u8, command.type, "module")) {
            const filename = command.filename orelse {
                return error.ModuleFilenameMissing;
            };

            if (current_module) |*mod| {
                mod.deinit();
            }

            const file_path = try std.fs.path.join(allocator, &.{ input_dir, filename });
            defer allocator.free(file_path);

            std.log.info("load module: {s}", .{file_path});

            const file = try std.fs.cwd().openFile(file_path, .{});
            defer file.close();

            const reader = file.reader();

            const mod = try module.Module.readAlloc(allocator, reader);

            current_module = mod;
        } else if (std.mem.eql(u8, command.type, "assert_return")) {
            const mod = current_module orelse {
                return error.NoModule;
            };

            assertReturn(allocator, &mod, &command) catch |err| {
                std.log.err("command fail: line={d}", .{command.line});
                return err;
            };
            // std.log.info("{s}: {d}: passed", .{ action.field, command.line });
        }
    }

    if (current_module) |*mod| {
        mod.deinit();
    }

    std.log.info("all tests passed", .{});
}

fn assertReturn(allocator: std.mem.Allocator, mod: *const module.Module, command: *const JsonWast.Command) !void {
    const action: JsonWast.Action = command.action orelse {
        return error.ActionMissing;
    };
    const expected: []JsonWast.ExpectedValue = command.expected orelse {
        return error.ExpectedMissing;
    };

    if (!std.mem.eql(u8, action.type, "invoke")) {
        return error.UnsupportedAction;
    }

    const fn_index = mod.findExportedFunctionIndex(action.field) orelse {
        return error.NoFunction;
    };

    var rt = runtime.Runtime.init();

    const parameters = try allocator.alloc(runtime.Value, action.args.len);
    defer allocator.free(parameters);

    for (parameters, action.args) |*p, a| {
        p.* = try a.toRuntimeValue();
    }

    const results = try allocator.alloc(runtime.Value, expected.len);
    defer allocator.free(results);

    try runtime.invokeFunction(mod, &rt, fn_index, parameters, results);

    for (expected, results) |e, r| {
        if (!try e.eqlRuntimeValue(r)) {
            std.log.err("{s}: {d}: expected={s}({s}), given={} ", .{ action.field, command.line, e.value orelse "<null>", e.type, r });

            for (parameters, 0..) |p, i| {
                std.log.err("  {d}: {}", .{ i, p });
            }

            return error.Fail;
        }
    }
}
