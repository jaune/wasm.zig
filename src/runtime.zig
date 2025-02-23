const std = @import("std");

const Module = @import("./module.zig").Module;
pub const ValueType = @import("./module.zig").ValueType;

const Instruction = @import("./module.zig").Instruction;
const InstructionTag = @import("./module.zig").InstructionTag;
const FunctionType = @import("./module.zig").FunctionType;

const InstructionFunctions = @import("./instruction_functions.zig");

pub const Value = union(ValueType) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,

    v128: void,
    function_reference: void,
    extern_reference: void,

    pub fn eql(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return false;
        }

        return switch (a) {
            .i32 => a.i32 == b.i32,
            .i64 => a.i64 == b.i64,
            .f32 => a.f32 == b.f32,
            .f64 => a.f64 == b.f64,
            else => false,
        };
    }
};

pub const Runtime = struct {
    const ValueStackLength: type = std.math.IntFittingRange(0, 1024);
    const ValueStackType = std.BoundedArray(Value, 1024);

    const FrameKind = enum {
        block,
        call,
    };

    pub const Frame = struct {
        kind: FrameKind,

        function_type: FunctionType,

        results_start: ValueStackLength,
        locals_start: ValueStackLength,
        locals_length: ValueStackLength,
    };
    const CallStackType = std.BoundedArray(Frame, 1024);

    const Self = @This();

    call_stack: CallStackType,
    value_stack: ValueStackType,

    pub fn peekCurrentFrame(self: *const Self) !*const Frame {
        if (self.call_stack.len == 0) {
            return error.CallStackEmpty;
        }

        return &self.call_stack.get(self.call_stack.len - 1);
    }

    pub fn popValuesIntoSlice(self: *Self, results: []Value) !void {
        for (results) |*result| {
            const value: Value = self.value_stack.popOrNull() orelse {
                return error.ValueStackEmpty;
            };

            result.* = value;
        }
    }

    pub fn popValue(self: *Self, comptime T: type) !T {
        const value: Value = self.value_stack.popOrNull() orelse {
            return error.ValueStackEmpty;
        };

        return switch (T) {
            i32 => switch (value) {
                .i32 => value.i32,
                else => error.WrongType,
            },
            i64 => switch (value) {
                .i64 => value.i64,
                else => error.WrongType,
            },
            f32 => switch (value) {
                .f32 => value.f32,
                else => error.WrongType,
            },
            f64 => switch (value) {
                .f64 => value.f64,
                else => error.WrongType,
            },
            else => {
                @compileError("Unsupported value type");
            },
        };
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
            // std.log.debug("execute {}", .{tag});

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
    const function_body = m.function_bodies[func_idx];
    const function_type: FunctionType = m.function_types[m.function_type_indices[func_idx]];

    if (function_type.parameters.len != parameters.len) {
        return error.WrongNumberOfParameters;
    }

    if (function_type.results.len != results.len) {
        return error.WrongNumberOfResults;
    }

    const locals_length = std.math.cast(Runtime.ValueStackLength, function_body.locals.len) orelse {
        return error.TooManyLocals;
    };

    const parameters_length = std.math.cast(Runtime.ValueStackLength, function_type.parameters.len) orelse {
        return error.TooManyLocals;
    };

    try runtime.call_stack.append(.{
        .kind = .call,
        .results_start = runtime.value_stack.len + parameters_length + locals_length,
        .locals_start = runtime.value_stack.len,
        .locals_length = parameters_length + locals_length,
        .function_type = function_type,
    });

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

    for (function_body.locals) |l| {
        switch (l.type) {
            .i32 => {
                try runtime.pushValue(i32, 0);
            },
            .i64 => {
                try runtime.pushValue(i64, 0);
            },
            .f32 => {
                try runtime.pushValue(f32, 0);
            },
            .f64 => {
                try runtime.pushValue(f64, 0);
            },
            else => {
                return error.UnsupportedParameterType;
            },
        }
    }

    for (function_body.expression) |instruction| {
        try executeInstruction(runtime, instruction);
    }

    try runtime.popValuesIntoSlice(results);

    if (runtime.value_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
    if (runtime.call_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
}
