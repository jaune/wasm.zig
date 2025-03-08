const std = @import("std");

const wasm = @import("jaune:wasm-runtime");
const runtime = wasm.runtime;
const module = wasm.module;
const Program = wasm.program.Program;

const BinaryModuleReader = wasm.BinaryModuleReader;

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

    std.log.info("json_path: {s}", .{input_path});

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

    std.log.info("wast_path: {s}", .{input_parsed.value.source_filename});

    var modules = try std.BoundedArray(*module.Module, 10).init(0);
    defer {
        for (modules.slice()) |m| {
            m.deinit();
            allocator.destroy(m);
        }
    }

    var program = runtime.Program.init(allocator);
    defer program.deinit();

    var current_module_index: ?module.ModuleInstanceIndex = null;

    for (input_parsed.value.commands) |command| {
        if (std.mem.eql(u8, command.type, "module")) {
            const filename = command.filename orelse {
                return error.ModuleFilenameMissing;
            };

            const file_path = try std.fs.path.join(allocator, &.{ input_dir, filename });
            defer allocator.free(file_path);

            std.log.info("load module: {s}", .{file_path});

            const file = try std.fs.cwd().openFile(file_path, .{});
            defer file.close();

            const reader = file.reader();

            const mod = try allocator.create(module.Module);

            mod.* = try BinaryModuleReader.readAllAlloc(allocator, reader);

            try modules.append(mod);

            current_module_index = try program.instantiateModule(mod);
        } else if (std.mem.eql(u8, command.type, "assert_return")) {
            const mod_idx = current_module_index orelse {
                return error.NoModule;
            };

            assertReturn(allocator, &program, mod_idx, &command) catch |err| {
                std.log.err("command fail: line={d}", .{command.line});
                return err;
            };
        } else if (std.mem.eql(u8, command.type, "assert_trap")) {
            const mod_idx = current_module_index orelse {
                return error.NoModule;
            };

            assertTrap(allocator, &program, mod_idx, &command) catch |err| {
                std.log.err("command fail: line={d}", .{command.line});
                return err;
            };
        }
    }

    std.log.info("all tests passed", .{});
}

fn assertTrap(allocator: std.mem.Allocator, program: *Program, mod_idx: module.ModuleInstanceIndex, command: *const JsonWast.Command) !void {
    const action: JsonWast.Action = command.action orelse {
        return error.ActionMissing;
    };

    const text: []const u8 = command.text orelse {
        return error.TextMissing;
    };

    if (!std.mem.eql(u8, action.type, "invoke")) {
        return error.UnsupportedAction;
    }

    const mod = program.module_instances.get(mod_idx).module;

    const fn_index = mod.findExportedFunctionIndex(action.field) orelse {
        return error.MissingExportedFunction;
    };

    const parameters = try allocator.alloc(runtime.Value, action.args.len);
    defer allocator.free(parameters);

    for (parameters, action.args) |*p, a| {
        p.* = try a.toRuntimeValue();
    }

    const funciton_type = try mod.getFunctionType(fn_index);

    const results = try allocator.alloc(runtime.Value, funciton_type.results.len);
    defer allocator.free(results);

    const expected = errorFromText(text) orelse {
        std.log.err("MissingErrorFromText: {s}", .{text});
        return error.MissingErrorFromText;
    };

    var rt = runtime.Runtime.init(program);

    runtime.invokeFunction(allocator, &rt, mod_idx, fn_index, parameters, results) catch |given| {
        if (given != expected) {
            std.log.err("assert_trap: {s}: {d}: expected={}, given={} ", .{ action.field, command.line, expected, given });
            return error.Fail;
        }
    };
}

fn errorFromText(text: []const u8) ?PanicError {
    if (std.mem.eql(u8, text, "integer divide by zero")) {
        return PanicError.DivisionByZero;
    }
    if (std.mem.eql(u8, text, "integer overflow")) {
        return PanicError.Overflow;
    }
    if (std.mem.eql(u8, text, "invalid conversion to integer")) {
        return PanicError.InvalidCastToInt;
    }
    if (std.mem.eql(u8, text, "undefined element")) {
        return PanicError.UndefinedElement;
    }
    return null;
}

const PanicError = error{
    DivisionByZero,
    Overflow,
    InvalidCastToInt,
    UndefinedElement,
};

fn assertReturn(allocator: std.mem.Allocator, program: *Program, mod_idx: module.ModuleInstanceIndex, command: *const JsonWast.Command) !void {
    const action: JsonWast.Action = command.action orelse {
        return error.ActionMissing;
    };
    const expected: []JsonWast.ExpectedValue = command.expected orelse {
        return error.ExpectedMissing;
    };

    if (!std.mem.eql(u8, action.type, "invoke")) {
        return error.UnsupportedAction;
    }

    // std.log.info("assert_return: {d}", .{command.line});
    const mod = program.module_instances.get(mod_idx).module;

    const fn_index = mod.findExportedFunctionIndex(action.field) orelse {
        return error.NoFunction;
    };

    const parameters = try allocator.alloc(runtime.Value, action.args.len);
    defer allocator.free(parameters);

    for (parameters, action.args) |*p, a| {
        p.* = try a.toRuntimeValue();
    }

    const results = try allocator.alloc(runtime.Value, expected.len);
    defer allocator.free(results);

    var rt = runtime.Runtime.init(program);

    // std.log.info("_____ assert_return: {s}: {d}", .{ action.field, command.line });

    runtime.invokeFunction(allocator, &rt, mod_idx, fn_index, parameters, results) catch |err| {
        std.log.err("assert_return: {s}: {d}: error: {}", .{ action.field, command.line, err });

        std.log.err("+ parameters", .{});
        for (parameters, 0..) |p, i| {
            std.log.err("{d}: {}", .{ i, p });
        }

        runtime.logRuntime(&rt);

        return err;
    };

    var has_fail = false;

    for (expected, results) |e, r| {
        if (!try e.testRuntimeValue(r)) {
            if (e.value) |ev| {
                const erv = try (JsonWast.Value{
                    .type = e.type,
                    .value = ev,
                }).toRuntimeValue();

                std.log.err("assert_return: {s}: {d}: expected={}, given={} ", .{ action.field, command.line, erv, r });

                if (std.meta.activeTag(erv) != std.meta.activeTag(r)) {
                    std.log.err("expected={}, given={}", .{ erv, r });
                    return error.NoMatchingValueType;
                }

                switch (erv) {
                    .f32 => std.log.err("expected=b{b}, given=b{b}", .{ @as(u32, @bitCast(erv.f32)), @as(u32, @bitCast(r.f32)) }),
                    .f64 => std.log.err("expected=b{b}, given=b{b}", .{ @as(u64, @bitCast(erv.f64)), @as(u64, @bitCast(r.f64)) }),
                    .i32 => std.log.err("expected=b{b}, given=b{b}", .{ @as(u32, @bitCast(erv.i32)), @as(u32, @bitCast(r.i32)) }),
                    .i64 => std.log.err("expected=b{b}, given=b{b}", .{ @as(u64, @bitCast(erv.i64)), @as(u64, @bitCast(r.i64)) }),
                    else => return error.UnsupportedValueType,
                }
            } else {
                std.log.err("assert_return: {s}: {d}: expected=<null>({s}), given={} ", .{ action.field, command.line, e.type, r });
            }

            for (parameters, 0..) |p, i| {
                std.log.err("{d}: {}", .{ i, p });
            }

            has_fail = true;
        }
    }

    if (has_fail) {
        runtime.logRuntime(&rt);
        return error.Fail;
    }
}

const JsonWast = struct {
    const ExpectedValue = struct {
        type: []u8,
        value: ?[]u8 = null,

        fn testRuntimeValue(self: ExpectedValue, other: runtime.Value) !bool {
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
                    const jv = Value{
                        .type = self.type,
                        .value = value,
                    };
                    const rv = try jv.toRuntimeValue();

                    switch (rv) {
                        .i32, .i64, .extern_reference, .function_reference => {
                            return rv.eql(other);
                        },
                        .f32 => |f| {
                            if (std.math.isNan(f) and std.math.isNan(other.f32)) {
                                return true;
                            }
                            return rv.eql(other);
                        },
                        .f64 => |f| {
                            if (std.math.isNan(f) and std.math.isNan(other.f64)) {
                                return true;
                            }
                            return rv.eql(other);
                        },
                        else => return error.UnsupportedValue,
                    }
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
            if (std.mem.eql(u8, self.type, "externref")) {
                const parsed = try std.fmt.parseUnsigned(u32, self.value, 10);

                return .{
                    .extern_reference = @as(module.ExternReference, @truncate(parsed)),
                };
            }

            std.log.err("UnsupportedValue: {}", .{self});
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
    source_filename: []const u8,
};
