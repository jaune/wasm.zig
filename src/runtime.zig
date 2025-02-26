const std = @import("std");

const Module = @import("./module.zig").Module;
pub const ValueType = @import("./module.zig").ValueType;
pub const Value = @import("./module.zig").Value;

const Instruction = @import("./module.zig").Instruction;
const InstructionTag = @import("./module.zig").InstructionTag;
const FunctionType = @import("./module.zig").FunctionType;
const Expression = @import("./module.zig").Expression;
const FunctionTypeTndex = @import("./module.zig").FunctionTypeTndex;
const InstructionIndex = @import("./module.zig").InstructionIndex;
const LabelIndex = @import("./module.zig").LabelIndex;
const FunctionBody = @import("./module.zig").FunctionBody;

const ArgumentsTypeOfInstruction = @import("./module.zig").ArgumentsTypeOfInstruction;

const InstructionFunctions = @import("./instruction_functions.zig");

pub fn logFunctionBody(body: *const FunctionBody) void {
    std.log.info("+ FunctionBody +", .{});

    std.log.info("++ Locals ++", .{});
    for (body.locals, 0..) |l, i| {
        std.log.info("{}: {}", .{ i, l });
    }

    std.log.info("++ Expression ++", .{});
    for (body.expression.instructions, 0..) |e, i| {
        switch (e.tag) {
            .@"n.const" => {
                const payload_index = e.payload_index.?;
                const payload = body.expression.constant_payloads[payload_index];

                std.log.info("{}: {s} (value={})", .{ i, @tagName(e.tag), payload.value });
            },
            .@"if" => {
                const payload_index = e.payload_index.?;
                const payload = body.expression.if_payloads[payload_index];

                std.log.info("{}: {s} (true={} false={})", .{
                    i,
                    @tagName(e.tag),
                    payload.true,
                    payload.false,
                });
            },
            .block => {
                const payload_index = e.payload_index.?;
                const payload = body.expression.label_payloads[payload_index];

                std.log.info("{}: {s} (start={} end={} function_type_index={})", .{
                    i,
                    @tagName(e.tag),
                    payload.start,
                    payload.end,
                    payload.function_type_index,
                });
            },
            else => {
                std.log.info("{}: {s}", .{ i, @tagName(e.tag) });
            },
        }
    }

    std.log.info("+++++", .{});
}

pub fn logRuntime(runtime: *const Runtime) void {
    std.log.info("+++++ Runtime", .{});

    std.log.info("call stack:", .{});

    for (runtime.call_stack.slice(), 0..) |c, i| {
        std.log.info("  {d}: {}", .{ i, c });
    }

    std.log.info("label stack:", .{});

    for (runtime.label_stack.slice(), 0..) |c, i| {
        std.log.info("  {d}: {}", .{ i, c });
    }
    std.log.info("value stack:", .{});

    for (runtime.value_stack.slice(), 0..) |c, i| {
        std.log.info("  {d}: {}", .{ i, c });
    }

    std.log.info("+++++", .{});
}

const FunctionIndex = @import("./module.zig").FunctionIndex;

pub const Runtime = struct {
    const max_value_stack_capacity = 1024;
    const ValueStackLength: type = std.math.IntFittingRange(0, max_value_stack_capacity);
    const ValueStackType = std.BoundedArray(Value, max_value_stack_capacity);

    pub const CallStackEntry = struct {
        function_index: FunctionIndex,
        function_type_index: FunctionTypeTndex,
        results_start: ValueStackLength,
        locals_start: ValueStackLength,
        locals_length: ValueStackLength,
        labels_start: LabelStackLength,
    };

    const max_call_stack_capacity = 1024;
    const CallStackType = std.BoundedArray(CallStackEntry, max_call_stack_capacity);

    const max_label_stack_capacity = 1024;

    const LabelKind = enum {
        block,
        loop,
    };
    const LabelStackEntry = struct {
        kind: LabelKind,

        start: InstructionIndex,
        end: InstructionIndex,

        value_stack_length_at_push: ValueStackLength,

        function_type: FunctionTypeTndex,
    };
    const LabelStackLength: type = std.math.IntFittingRange(0, max_label_stack_capacity);
    const LabelStackType = std.BoundedArray(LabelStackEntry, max_label_stack_capacity);

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

    pub fn branchFromLabelIndex(self: *Self, index: u16) !void {
        const frame = try self.peekCurrentCall();

        const idx = std.math.cast(LabelStackLength, frame.labels_start + index) orelse {
            return error.TooBig;
        };
        const l = self.label_stack.get(idx);

        switch (l.kind) {
            .block => {
                self.instruction_pointer = l.end;
                self.label_stack.len = idx;
            },
            .loop => {
                self.label_stack.len = idx + 1;
                self.instruction_pointer = l.start;
            },
        }
    }

    pub fn peekCurrentCall(self: *const Self) !CallStackEntry {
        if (self.call_stack.len == 0) {
            return error.EmptyCallStack;
        }
        return self.call_stack.get(self.call_stack.len - 1);
    }

    pub fn popLabel(self: *Self) !LabelStackEntry {
        if (self.label_stack.len == 0) {
            return error.EmptyLabelStack;
        }
        return self.label_stack.pop();
    }

    pub fn popAnyValuesIntoSlice(self: *Self, results: []Value) !void {
        for (results) |*result| {
            const value: Value = self.value_stack.popOrNull() orelse {
                return error.ValueStackEmpty;
            };

            result.* = value;
        }
    }

    pub fn popAnyValue(self: *Self) !Value {
        const value: Value = self.value_stack.popOrNull() orelse {
            return error.ValueStackEmpty;
        };

        return value;
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

    pub fn pushNZeroValues(self: *Self, comptime T: type, n: usize) !void {
        switch (T) {
            i32 => {
                try self.value_stack.appendNTimes(.{ .i32 = 0 }, n);
            },
            i64 => {
                try self.value_stack.appendNTimes(.{ .i64 = 0 }, n);
            },
            f32 => {
                try self.value_stack.appendNTimes(.{ .f32 = 0 }, n);
            },
            f64 => {
                try self.value_stack.appendNTimes(.{ .f64 = 0 }, n);
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
};

fn executeExpression(module: *const Module, runtime: *Runtime, expression: *const Expression) !void {
    runtime.instruction_pointer = 0;

    while (true) {
        const instruction = expression.instructions[runtime.instruction_pointer];

        switch (instruction.tag) {
            .nop => {
                // NOTE: Do nothing.
            },

            .@"unreachable" => {
                return error.Unreachable;
            },

            .call => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.call_payloads[payload_index];
                const function_index = payload.function_index;
                const function_type_index = module.function_type_indices[function_index];
                const function_type = module.function_types[function_type_index];
                const function_body = module.function_bodies[function_index];

                const frame = try runtime.peekCurrentCall();

                if (runtime.value_stack.len < (frame.results_start + function_type.parameters.len)) {
                    return error.NotEnoughValuesOnTheStack;
                }

                const parameters_length = std.math.cast(Runtime.ValueStackLength, function_type.parameters.len) orelse {
                    return error.TooBig;
                };

                const parameters_start = runtime.value_stack.len - parameters_length;

                for (function_type.parameters, runtime.value_stack.slice()[parameters_start..]) |pt, v| {
                    try v.assertValueType(pt);
                }

                try pushLocals(runtime, function_body.locals);

                const function_body_locals_len = std.math.cast(Runtime.ValueStackLength, function_body.locals.len) orelse {
                    return error.TooBig;
                };

                const locals_length = parameters_length + function_body_locals_len;

                try runtime.call_stack.append(.{
                    .function_index = function_index,
                    .function_type_index = function_type_index,
                    .labels_start = runtime.label_stack.len,
                    .locals_start = parameters_start,
                    .locals_length = locals_length,
                    .results_start = runtime.value_stack.len,
                });

                return error.NotImplementedYet;
            },

            .block, .loop => |tag| {
                const payload_index = instruction.payload_index orelse {
                    std.log.err("execute {}", .{tag});
                    return error.NoPayloadIndex;
                };
                const payload = expression.label_payloads[payload_index];

                try runtime.label_stack.append(.{
                    .kind = switch (tag) {
                        .block => .block,
                        .loop => .loop,
                        else => return error.UnhandledLabelKind,
                    },
                    .start = payload.start,
                    .end = payload.end,
                    .value_stack_length_at_push = runtime.value_stack.len,
                    .function_type = payload.function_type_index,
                });
            },

            .loop_end => {
                const label = try runtime.popLabel();

                if (label.kind != .loop) {
                    return error.WrongLabelPopped;
                }
            },
            .block_end => {
                const label = try runtime.popLabel();

                if (label.kind != .block) {
                    return error.WrongLabelPopped;
                }

                const function_type = module.function_types[label.function_type];

                const value_stack_slice = runtime.value_stack.slice();

                std.mem.copyForwards(
                    Value,
                    value_stack_slice[label.value_stack_length_at_push..],
                    value_stack_slice[(runtime.value_stack.len - function_type.results.len)..runtime.value_stack.len],
                );

                runtime.value_stack.len = label.value_stack_length_at_push + @as(Runtime.ValueStackLength, @intCast(function_type.results.len));

                runtime.instruction_pointer = label.end;
                // continue;
            },

            .@"return", .expression_end => |tag| {
                const entry = runtime.call_stack.popOrNull() orelse {
                    return error.NoFrameToEnd;
                };

                switch (tag) {
                    .expression_end => {
                        if (runtime.label_stack.len != entry.labels_start) {
                            return error.WrongLabelStackSize;
                        }
                    },
                    .@"return" => {
                        // NOTE: reset labels stack
                        runtime.label_stack.len = entry.labels_start;
                    },
                    else => return error.UnhandledInstruction,
                }

                const function_type = module.function_types[entry.function_type_index];
                const expected_end = entry.results_start + function_type.results.len;

                if (expected_end != runtime.value_stack.len) {
                    std.log.err("expected_end={}, given_end={}", .{ expected_end, runtime.value_stack.len });
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

            .branch => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.branch_payloads[payload_index];

                try runtime.branchFromLabelIndex(payload.label_index);
            },

            .branch_if => {
                const condition = try runtime.popValue(i32);

                if (condition != 0) {
                    const payload_index = instruction.payload_index orelse {
                        return error.NoPayloadIndex;
                    };
                    const payload = expression.branch_payloads[payload_index];

                    try runtime.branchFromLabelIndex(payload.label_index);
                }
            },

            .@"if" => {
                const condition = try runtime.popValue(i32);

                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.if_payloads[payload_index];

                runtime.instruction_pointer = if (condition != 0) payload.true else payload.false;
                continue;
            },

            .branch_table => {
                const active = try runtime.popValue(i32);

                if (active < 0) {
                    return error.BranchTableOutOfBounds;
                }

                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.branch_table_payloads[payload_index];

                if (active < payload.branches.len) {
                    const a = std.math.cast(usize, active) orelse {
                        return error.InvalidBranchIndex;
                    };
                    try runtime.branchFromLabelIndex(payload.branches[a]);
                } else {
                    try runtime.branchFromLabelIndex(payload.fallback);
                }
            },

            .drop => {
                if (runtime.value_stack.len == 0) {
                    return error.EmptyValueStack;
                }
                runtime.value_stack.len -= 1;
            },

            .@"n.const" => |tag| {
                const payload_index = instruction.payload_index orelse {
                    std.log.err("execute {}", .{tag});
                    return error.NoPayloadIndex;
                };
                const payload = expression.constant_payloads[payload_index];

                try runtime.value_stack.append(payload.value);
            },

            inline .@"local.get", .@"local.set", .@"local.tee", .@"global.get", .@"global.set" => |tag| {
                const payload_index = instruction.payload_index orelse {
                    std.log.err("execute {}", .{tag});

                    return error.NoArgumentsIndex;
                };

                const tag_name = comptime @tagName(tag);
                const func = comptime @field(InstructionFunctions, tag_name);

                const ArgTuple: type = comptime std.meta.ArgsTuple(@TypeOf(func));
                var args: ArgTuple = undefined;

                args.@"0" = runtime;

                const arguments = expression.instruction_arguments[payload_index];

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

    var body_locals_length: Runtime.ValueStackLength = 0;

    for (function_body.locals) |l| {
        body_locals_length += std.math.cast(Runtime.ValueStackLength, l.n) orelse {
            return error.TooManyLocals;
        };
    }

    const parameters_length = std.math.cast(Runtime.ValueStackLength, function_type.parameters.len) orelse {
        return error.TooManyLocals;
    };

    const locals_length = body_locals_length + parameters_length;
    const results_start = runtime.value_stack.len + locals_length;

    try runtime.call_stack.append(.{
        .function_index = func_idx,
        .function_type_index = function_type_index,
        .results_start = results_start,
        .locals_start = runtime.value_stack.len,
        .locals_length = locals_length,
        .labels_start = runtime.label_stack.len,
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

    try pushLocals(runtime, function_body.locals);

    if (results_start != runtime.value_stack.len) {
        return error.AssertValueStack;
    }

    try executeExpression(m, runtime, &function_body.expression);

    // catch |err| {
    //     // logFunctionBody(&function_body);
    //     // logRuntime(runtime);

    //     std.log.err("ADRIEN !!!! {}", .{err});

    //     return err;
    // };

    try runtime.popAnyValuesIntoSlice(results);

    if (runtime.value_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
    if (runtime.call_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
}

const Local = @import("./module.zig").Local;

fn pushLocals(runtime: *Runtime, locals: []Local) !void {
    for (locals) |l| {
        switch (l.type) {
            .i32 => {
                try runtime.pushNZeroValues(i32, l.n);
            },
            .i64 => {
                try runtime.pushNZeroValues(i64, l.n);
            },
            .f32 => {
                try runtime.pushNZeroValues(f32, l.n);
            },
            .f64 => {
                try runtime.pushNZeroValues(f64, l.n);
            },
            else => {
                return error.UnsupportedParameterType;
            },
        }
    }
}
