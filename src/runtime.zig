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
const ReferenceType = @import("./module.zig").ReferenceType;
const Limits = @import("./module.zig").Limits;
const FunctionReference = @import("./module.zig").FunctionReference;

const ModuleInstanceIndex = @import("./program.zig").ModuleInstanceIndex;
pub const Program = @import("./program.zig").Program;

const ArgumentsTypeOfInstruction = @import("./module.zig").ArgumentsTypeOfInstruction;

const InstructionFunctions = @import("./instruction_functions.zig");

const FunctionIndex = @import("./module.zig").FunctionIndex;

pub const Runtime = struct {
    const max_value_stack_capacity = 1024;
    const ValueStackLength: type = std.math.IntFittingRange(0, max_value_stack_capacity);
    const ValueStackType = std.BoundedArray(Value, max_value_stack_capacity);

    pub const Frame = struct {
        expression: *const Expression,
        instruction_pointer: u32,
        module_instance_index: ModuleInstanceIndex,
        labels_start: LabelStackLength,
    };

    pub const CallStackEntry = struct {
        frame: Frame,

        function_index: FunctionIndex,
        function_type_index: FunctionTypeTndex,

        results_start: ValueStackLength,
        locals_start: ValueStackLength,
        parameters_start: ValueStackLength,
    };

    const max_call_stack_capacity = 512;
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

    call_stack: CallStackType = .{},
    value_stack: ValueStackType = .{},
    label_stack: LabelStackType = .{},
    program: *Program,

    pub fn init(program: *Program) Self {
        return .{
            .program = program,
        };
    }

    pub fn branchFromLabelIndex(self: *Self, frame: *Frame, index: u16) !void {
        const idx = std.math.cast(LabelStackLength, frame.labels_start + index) orelse {
            return error.TooBig;
        };
        const l = self.label_stack.get(idx);

        switch (l.kind) {
            .block => {
                self.label_stack.len = idx;
                frame.instruction_pointer = l.end;
            },
            .loop => {
                self.label_stack.len = idx + 1;
                frame.instruction_pointer = l.start;
            },
        }
    }

    pub fn getCurrentFrameResultsStart(self: *const Self) Runtime.ValueStackLength {
        if (self.call_stack.len == 0) {
            return 0;
        }

        const frame = self.call_stack.get(self.call_stack.len - 1);

        return frame.results_start;
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
        const values = self.value_stack.slice();

        for (results, values[(self.value_stack.len - results.len)..]) |*result, value| {
            result.* = value;
        }
        self.value_stack.len -= std.math.cast(ValueStackLength, results.len) orelse {
            return error.CastingError;
        };
    }

    pub fn popAnyValue(self: *Self) !Value {
        const value: Value = self.value_stack.popOrNull() orelse {
            return error.ValueStackEmpty;
        };

        return value;
    }

    pub fn pushAnyValue(self: *Self, value: Value) !void {
        try self.value_stack.append(value);
    }

    pub fn popValueFunctionReference(self: *Self) !FunctionReference {
        const value: Value = self.value_stack.popOrNull() orelse {
            return error.ValueStackEmpty;
        };

        return switch (value) {
            .function_reference => |r| r,
            else => error.WrongType,
        };
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

pub fn executeExpression(runtime: *Runtime, root_module_instance_index: ModuleInstanceIndex, root_expression: *const Expression) !void {
    var root_frame = Runtime.Frame{
        .instruction_pointer = 0,
        .expression = root_expression,
        .module_instance_index = root_module_instance_index,
        .labels_start = runtime.label_stack.len,
    };

    var current_frame: *Runtime.Frame = &root_frame;

    interpreter_loop: while (true) {
        const expression = current_frame.expression;

        if (current_frame.instruction_pointer >= expression.instructions.len) {
            return error.OutOfBounds;
        }

        const instruction = expression.instructions[current_frame.instruction_pointer];

        switch (instruction.tag) {
            .nop => {
                // NOTE: Do nothing.
                current_frame.instruction_pointer += 1;
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

                try pushCall(runtime, current_frame.module_instance_index, function_index);

                current_frame = &runtime.call_stack.slice()[runtime.call_stack.len - 1].frame;
            },

            .call_indirect => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.call_indirect_payloads[payload_index];
                const element_index_i32 = try runtime.popValue(i32);

                const element_index = std.math.cast(usize, element_index_i32) orelse {
                    return error.CastingError;
                };

                const module_instance_index = current_frame.module_instance_index;
                const module_instance = runtime.program.module_instances.get(module_instance_index);

                const function_index = try module_instance.getFunctionIndexFromTable(payload.table_index, element_index, payload.function_type_index);

                try pushCall(runtime, module_instance_index, function_index);

                current_frame = &runtime.call_stack.slice()[runtime.call_stack.len - 1].frame;
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

                current_frame.instruction_pointer += 1;
            },

            .loop_end => {
                const label = try runtime.popLabel();

                if (label.kind != .loop) {
                    return error.WrongLabelPopped;
                }

                current_frame.instruction_pointer += 1;
            },
            .block_end => {
                const label = try runtime.popLabel();

                if (label.kind != .block) {
                    return error.WrongLabelPopped;
                }

                const module_instance_index = current_frame.module_instance_index;

                const module_instance = runtime.program.module_instances.get(module_instance_index);
                const function_type = module_instance.module.function_types[label.function_type];

                const value_stack_slice = runtime.value_stack.slice();

                std.mem.copyForwards(
                    Value,
                    value_stack_slice[label.value_stack_length_at_push..],
                    value_stack_slice[(runtime.value_stack.len - function_type.results.len)..runtime.value_stack.len],
                );

                runtime.value_stack.len = label.value_stack_length_at_push + @as(Runtime.ValueStackLength, @intCast(function_type.results.len));

                current_frame.instruction_pointer = label.end + 1;
            },

            .@"return", .expression_end => |tag| {
                const entry = runtime.call_stack.popOrNull() orelse {
                    break :interpreter_loop;
                };

                switch (tag) {
                    .expression_end => {
                        if (runtime.label_stack.len != entry.frame.labels_start) {
                            return error.WrongLabelStackSize;
                        }
                    },
                    .@"return" => {
                        runtime.label_stack.len = entry.frame.labels_start;
                    },
                    else => return error.UnhandledInstruction,
                }

                const module_instance = runtime.program.module_instances.get(current_frame.module_instance_index);
                const function_type = module_instance.module.function_types[entry.function_type_index];
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
                    value_stack_slice[entry.parameters_start..],
                    value_stack_slice[entry.results_start..(entry.results_start + function_type.results.len)],
                );

                const results_length = std.math.cast(Runtime.ValueStackLength, function_type.results.len) orelse {
                    return error.TooBig;
                };

                runtime.value_stack.len = entry.parameters_start + results_length;

                if (runtime.call_stack.len == 0) {
                    current_frame = &root_frame;
                } else {
                    current_frame = &runtime.call_stack.slice()[runtime.call_stack.len - 1].frame;
                }

                current_frame.instruction_pointer += 1;
            },

            .branch => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.branch_payloads[payload_index];

                try runtime.branchFromLabelIndex(current_frame, payload.label_index);
                current_frame.instruction_pointer += 1;
            },

            .branch_if => {
                const condition = try runtime.popValue(i32);

                if (condition != 0) {
                    const payload_index = instruction.payload_index orelse {
                        return error.NoPayloadIndex;
                    };
                    const payload = expression.branch_payloads[payload_index];

                    try runtime.branchFromLabelIndex(current_frame, payload.label_index);
                }
                current_frame.instruction_pointer += 1;
            },

            .@"if" => {
                const condition = try runtime.popValue(i32);

                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.if_payloads[payload_index];

                current_frame.instruction_pointer = if (condition != 0) payload.true else payload.false;
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
                    try runtime.branchFromLabelIndex(current_frame, payload.branches[a]);
                } else {
                    try runtime.branchFromLabelIndex(current_frame, payload.fallback);
                }
                current_frame.instruction_pointer += 1;
            },

            .drop => {
                if (runtime.value_stack.len == 0) {
                    return error.EmptyValueStack;
                }
                runtime.value_stack.len -= 1;
                current_frame.instruction_pointer += 1;
            },

            .@"n.const" => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.constant_payloads[payload_index];

                try runtime.value_stack.append(payload.value);
                current_frame.instruction_pointer += 1;
            },

            .@"ref.func" => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.function_reference_payloads[payload_index];

                try runtime.value_stack.append(.{
                    .function_reference = try runtime.program.getFunctionReference(
                        current_frame.module_instance_index,
                        payload.function_index,
                    ),
                });
                current_frame.instruction_pointer += 1;
            },

            inline .@"local.get", .@"local.set", .@"local.tee" => |tag| {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.local_accessor_payloads[payload_index];

                const tag_name = comptime @tagName(tag);
                const func = comptime @field(InstructionFunctions, tag_name);

                _ = try @call(.auto, func, .{ runtime, payload.local_index });
                current_frame.instruction_pointer += 1;
            },

            inline .@"global.get", .@"global.set" => |tag| {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.global_accessor_payloads[payload_index];

                const tag_name = comptime @tagName(tag);
                const func = comptime @field(InstructionFunctions, tag_name);

                _ = try @call(.auto, func, .{ runtime, payload.global_index });
                current_frame.instruction_pointer += 1;
            },

            inline else => |tag| {
                const tag_name = comptime @tagName(tag);
                const func = comptime @field(InstructionFunctions, tag_name);

                _ = try @call(.auto, func, .{runtime});
                current_frame.instruction_pointer += 1;
            },
        }
    }
}

pub fn pushCall(runtime: *Runtime, mi_idx: ModuleInstanceIndex, function_index: FunctionIndex) !void {
    const module = runtime.program.module_instances.get(mi_idx).module;
    const function_type_index = module.function_type_indices[function_index];
    const function_type = module.function_types[function_type_index];
    const function_body = &module.function_bodies[function_index];

    const parameters_length = std.math.cast(Runtime.ValueStackLength, function_type.parameters.len) orelse {
        return error.TooBig;
    };
    const locals_length = try computeLocalLength(function_body.locals);
    const current_results_start = runtime.getCurrentFrameResultsStart();

    if ((current_results_start + parameters_length) > runtime.value_stack.len) {
        return error.MissingParamaterOnStack;
    }

    const parameters_start = runtime.value_stack.len - parameters_length;
    const locals_start = parameters_start + parameters_length;
    const results_start = locals_start + locals_length;

    for (function_type.parameters, runtime.value_stack.slice()[parameters_start..]) |pt, v| {
        try v.assertValueType(pt);
    }

    if (locals_start != runtime.value_stack.len) {
        return error.AssertValueStackSize;
    }

    try pushLocals(runtime, function_body.locals);

    if (results_start != runtime.value_stack.len) {
        return error.AssertValueStackSize;
    }

    try runtime.call_stack.append(.{
        .frame = .{
            .instruction_pointer = 0,
            .module_instance_index = mi_idx,
            .expression = &function_body.expression,
            .labels_start = runtime.label_stack.len,
        },
        .function_index = function_index,
        .function_type_index = function_type_index,
        .parameters_start = parameters_start,
        .locals_start = locals_start,
        .results_start = results_start,
    });
}

const ExpressionBuilder = @import("./expression_builder.zig").ExpressionBuilder;

fn createInvokeExpression(allocator: std.mem.Allocator, function_index: FunctionIndex) !Expression {
    var builder: ExpressionBuilder = ExpressionBuilder.init(allocator);
    defer builder.deinit();

    try builder.appendCall(.{
        .function_index = function_index,
    });
    try builder.appendEnd();

    return try builder.build(allocator);
}

pub fn invokeFunction(allocator: std.mem.Allocator, runtime: *Runtime, root_module_instance_index: ModuleInstanceIndex, function_index: u32, parameters: []const Value, results: []Value) !void {
    // std.log.debug("invokeFunction fn={}", .{function_index});

    const m = runtime.program.module_instances.get(root_module_instance_index).module;
    const function_type: FunctionType = try m.getFunctionType(function_index);

    if (function_type.parameters.len != parameters.len) {
        return error.WrongNumberOfParameters;
    }

    if (function_type.results.len != results.len) {
        return error.WrongNumberOfResults;
    }

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

    var root_expression = try createInvokeExpression(allocator, function_index);
    defer root_expression.free(allocator);

    try executeExpression(runtime, root_module_instance_index, &root_expression);

    if (runtime.value_stack.len != function_type.results.len) {
        return error.MissingResult;
    }

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

fn computeLocalLength(locals: []Local) !Runtime.ValueStackLength {
    var count: Runtime.ValueStackLength = 0;

    for (locals) |l| {
        const n = std.math.cast(Runtime.ValueStackLength, l.n) orelse {
            return error.TooBig;
        };

        count += n;
    }

    return count;
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
