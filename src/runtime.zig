const std = @import("std");

const Module = @import("./module.zig").Module;
pub const ValueType = @import("./module.zig").ValueType;

const Instruction = @import("./module.zig").Instruction;
const InstructionTag = @import("./module.zig").InstructionTag;
const FunctionType = @import("./module.zig").FunctionType;
const Expression = @import("./module.zig").Expression;
const FunctionTypeTndex = @import("./module.zig").FunctionTypeTndex;
const InstructionIndex = @import("./module.zig").InstructionIndex;

const ArgumentsTypeOfInstruction = @import("./module.zig").ArgumentsTypeOfInstruction;

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

    pub const CallStackEntry = struct {
        results_start: ValueStackLength,
        locals_start: ValueStackLength,
        locals_length: ValueStackLength,
        function_type_index: FunctionTypeTndex,
    };
    const CallStackType = std.BoundedArray(CallStackEntry, 1024);

    const LabelKind = enum {
        block,
        loop,
    };
    const LabelStackEntry = struct {
        kind: LabelKind,

        start: InstructionIndex,
        end: InstructionIndex,

        function_type: FunctionTypeTndex,
    };
    const LabelStackType = std.BoundedArray(LabelStackEntry, 1024);

    const Self = @This();

    call_stack: CallStackType,
    value_stack: ValueStackType,
    label_stack: LabelStackType,

    instruction_pointer: u32,

    pub fn init() Self {
        return .{
            .instruction_pointer = 0,
            .call_stack = CallStackType{},
            .value_stack = ValueStackType{},
            .label_stack = LabelStackType{},
        };
    }

    pub fn peekCurrentCall(self: *const Self) !CallStackEntry {
        if (self.call_stack.len == 0) {
            return error.EmptyCallStack;
        }
        return self.call_stack.get(self.call_stack.len - 1);
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
};

fn executeExpression(module: *const Module, runtime: *Runtime, expression: *const Expression) !void {
    runtime.instruction_pointer = 0;

    while (true) {
        const instruction = expression.instructions[runtime.instruction_pointer];

        switch (instruction.tag) {
            .block => |tag| {
                const arguments_index = instruction.arguments_index orelse {
                    std.log.err("execute {}", .{tag});

                    return error.NoArgumentsIndex;
                };
                const arguments = expression.instruction_arguments[arguments_index].block;

                try runtime.label_stack.append(.{
                    .kind = .block,
                    .start = arguments.start,
                    .end = arguments.end,
                    .function_type = arguments.function_type_index,
                });
            },
            .loop => |tag| {
                const arguments_index = instruction.arguments_index orelse {
                    std.log.err("execute {}", .{tag});

                    return error.NoArgumentsIndex;
                };
                const arguments = expression.instruction_arguments[arguments_index].loop;

                try runtime.label_stack.append(.{
                    .kind = .loop,
                    .start = arguments.start,
                    .end = arguments.end,
                    .function_type = arguments.function_type_index,
                });
            },

            .loop_end => {},
            .block_end => {},
            .if_end => {},

            .expression_end => {
                const entry = runtime.call_stack.popOrNull() orelse {
                    return error.NoFrameToEnd;
                };

                const function_type = module.function_types[entry.function_type_index];
                const expected_end = entry.results_start + function_type.results.len;

                if (expected_end != runtime.value_stack.len) {
                    std.log.err("expected_end={}, end={}", .{ expected_end, runtime.value_stack.len });
                    return error.WrongValueStackSize;
                }

                for (function_type.results, entry.results_start..) |result_type, i| {
                    const value: Value = runtime.value_stack.get(i);

                    if (result_type != std.meta.activeTag(value)) {
                        return error.WrongResultType;
                    }
                }

                const value_stack_slice = runtime.value_stack.slice();

                std.mem.copyForwards(
                    Value,
                    value_stack_slice[entry.locals_start..],
                    value_stack_slice[entry.results_start..(entry.results_start + function_type.results.len)],
                );

                runtime.value_stack.len -= entry.locals_length;
                return;
            },
            inline .br, .br_if, .@"local.get", .@"local.set", .@"local.tee", .@"global.get", .@"global.set", .@"i32.const", .@"i64.const", .@"f32.const", .@"f64.const" => |tag| {
                const arguments_index = instruction.arguments_index orelse {
                    std.log.err("execute {}", .{tag});

                    return error.NoArgumentsIndex;
                };

                const tag_name = comptime @tagName(tag);
                const func = comptime @field(InstructionFunctions, tag_name);

                const ArgTuple: type = comptime std.meta.ArgsTuple(@TypeOf(func));
                var args: ArgTuple = undefined;

                args.@"0" = runtime;

                const arguments = expression.instruction_arguments[arguments_index];

                inline for (std.meta.fields(ArgumentsTypeOfInstruction(tag)), 1..) |p, i| {
                    @field(args, std.meta.fields(ArgTuple)[i].name) = @field(@field(arguments, tag_name), p.name);
                }

                _ = try @call(.auto, func, args);
            },
            inline else => |tag| {
                const tag_name = comptime @tagName(tag);
                const func = comptime @field(InstructionFunctions, tag_name);

                _ = try @call(.auto, func, .{runtime});
            },
        }

        runtime.instruction_pointer += 1;
    }
}

pub fn invokeFunction(m: *const Module, runtime: *Runtime, func_idx: u32, parameters: []const Value, results: []Value) !void {
    const function_body = m.function_bodies[func_idx];
    const function_type_index = m.function_type_indices[func_idx];
    const function_type: FunctionType = m.function_types[function_type_index];

    if (function_type.parameters.len != parameters.len) {
        return error.WrongNumberOfParameters;
    }

    if (function_type.results.len != results.len) {
        return error.WrongNumberOfResults;
    }

    const body_locals_length = std.math.cast(Runtime.ValueStackLength, function_body.locals.len) orelse {
        return error.TooManyLocals;
    };

    const parameters_length = std.math.cast(Runtime.ValueStackLength, function_type.parameters.len) orelse {
        return error.TooManyLocals;
    };

    const locals_length = body_locals_length + parameters_length;
    const results_start = runtime.value_stack.len + locals_length;

    try runtime.call_stack.append(.{
        .results_start = results_start,
        .locals_start = runtime.value_stack.len,
        .locals_length = locals_length,
        .function_type_index = function_type_index,
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

    if (results_start != runtime.value_stack.len) {
        return error.AssertValueStack;
    }

    try executeExpression(m, runtime, &function_body.expression);

    try runtime.popValuesIntoSlice(results);

    if (runtime.value_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
    if (runtime.call_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
}
