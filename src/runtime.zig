const std = @import("std");

const Module = @import("./module.zig").Module;
const Instruction = @import("./module.zig").Instruction;
const InstructionTag = @import("./module.zig").InstructionTag;
const FunctionType = @import("./module.zig").FunctionType;

const InstructionFunctions = @import("./instruction_functions.zig");

pub const Value = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,

    pub fn eql(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return false;
        }

        return switch (a) {
            .i32 => a.i32 == b.i32,
            .i64 => a.i64 == b.i64,
            .f32 => a.f32 == b.f32,
            .f64 => a.f64 == b.f64,
        };
    }
};

pub const Runtime = struct {
    const Len: type = std.math.IntFittingRange(0, 1024);
    const ValueStackType = std.BoundedArray(Value, 1024);

    const CallStackEntry = struct {
        locals_start: Len,
        locals_length: Len,
    };
    const CallStackType = std.BoundedArray(CallStackEntry, 1024);

    const Self = @This();

    call_stack: CallStackType,
    value_stack: ValueStackType,

    pub fn popValue(self: *Self, comptime T: type) T {
        switch (T) {
            i32 => {
                return self.value_stack.pop().i32;
            },
            i64 => {
                return self.value_stack.pop().i64;
            },
            f32 => {
                return self.value_stack.pop().f32;
            },
            f64 => {
                return self.value_stack.pop().f64;
            },
            else => {
                @compileError("Unsupported value type");
            },
        }
    }

    pub fn pushValue(self: *Self, comptime T: type, value: T) !void {
        switch (T) {
            i32 => {
                try self.value_stack.append(.{ .i32 = value });
            },
            i64 => {
                try self.value_stack.append(.{ .i64 = value });
            },
            f32 => {
                try self.value_stack.append(.{ .f32 = value });
            },
            f64 => {
                try self.value_stack.append(.{ .f64 = value });
            },
            else => {
                @compileError("Unsupported value type");
            },
        }
    }

    pub fn init() Self {
        return .{
            .call_stack = CallStackType{},
            .value_stack = ValueStackType{},
        };
    }
};

fn executeInstruction(runtime: *Runtime, instruction: Instruction) !void {
    switch (instruction) {
        inline else => |parameters, tag| {
            const tag_name = comptime @tagName(tag);
            const func = comptime @field(InstructionFunctions, tag_name);

            const ArgTuple: type = comptime std.meta.ArgsTuple(@TypeOf(func));
            var args: ArgTuple = undefined;

            args.@"0" = runtime;

            inline for (std.meta.fields(@TypeOf(parameters)), 1..) |p, i| {
                @field(args, std.meta.fields(ArgTuple)[i].name) = @field(parameters, p.name);
            }

            _ = try @call(.auto, func, args);
        },
    }
}

pub fn invokeFunction(m: *const Module, runtime: *Runtime, func_idx: u32, parameters: []const Value, results: []Value) !void {
    const b = m.function_bodies[func_idx];
    const function_type: FunctionType = m.function_types[m.function_type_indices[func_idx]];

    if (function_type.parameters.len != parameters.len) {
        return error.WrongNumberOfParameters;
    }

    if (function_type.results.len != results.len) {
        return error.WrongNumberOfResults;
    }

    var frame = try runtime.call_stack.addOne();

    frame.locals_start = runtime.value_stack.len;

    for (function_type.parameters, 0..) |p, i| {
        switch (p) {
            .i32 => {
                const value = switch (parameters[i]) {
                    .i32 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.pushValue(i32, value);
            },
            .i64 => {
                const value = switch (parameters[i]) {
                    .i64 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.pushValue(i64, value);
            },
            .f32 => {
                const value = switch (parameters[i]) {
                    .f32 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.pushValue(f32, value);
            },
            .f64 => {
                const value = switch (parameters[i]) {
                    .f64 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.pushValue(f64, value);
            },
            else => {
                return error.UnsupportedParameterType;
            },
        }
    }

    const locals_end = runtime.value_stack.len;

    frame.locals_length = locals_end - frame.locals_start;

    for (b.expression) |instruction| {
        try executeInstruction(runtime, instruction);
    }

    const expected_length = locals_end + function_type.results.len;

    if (expected_length != runtime.value_stack.len) {
        return error.WrongNumberOfResults;
    }

    for (function_type.results, 0..) |p, i| {
        const value = runtime.value_stack.pop();

        switch (p) {
            .i32 => {
                results[i] = switch (value) {
                    .i32 => value,
                    else => {
                        return error.InvalidResultType;
                    },
                };
            },
            .i64 => {
                results[i] = switch (value) {
                    .i64 => value,
                    else => {
                        return error.InvalidResultType;
                    },
                };
            },
            .f32 => {
                results[i] = switch (value) {
                    .f32 => value,
                    else => {
                        return error.InvalidResultType;
                    },
                };
            },
            .f64 => {
                results[i] = switch (value) {
                    .f64 => value,
                    else => {
                        return error.InvalidResultType;
                    },
                };
            },
            else => {
                return error.UnsupportedResultType;
            },
        }
    }

    runtime.value_stack.len -= frame.locals_length;
    runtime.call_stack.len -= 1;

    if (runtime.value_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
    if (runtime.call_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
}
