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
const ModuleInstanceIndex = @import("./module.zig").ModuleInstanceIndex;
const AnyReference = @import("./module.zig").AnyReference;
const MemoryPageIndex = @import("./module.zig").MemoryPageIndex;

const logExpression = @import("./module.zig").logExpression;
const logExpressionInstruction = @import("./module.zig").logExpressionInstruction;

pub const Program = @import("./program.zig").Program;

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
        values_start: ValueStackLength,
    };

    pub const CallStackEntry = struct {
        frame: Frame,

        function_index: FunctionIndex,
        function_type_index: FunctionTypeTndex,

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
    current_frame: ?*Frame = null,

    pub fn init(program: *Program) Self {
        return .{
            .program = program,
        };
    }

    pub fn endBlock(self: *Self, frame: *Frame, label: LabelStackEntry) !void {
        if (label.kind != .block) {
            return error.WrongLabelPopped;
        }

        const function_type: FunctionType = try self.program.functionTypeFromIndex(frame.module_instance_index, label.function_type);

        const values = self.value_stack.slice();

        const min_vstack_len = label.value_stack_length_at_push + function_type.results.len - function_type.parameters.len;

        if (self.value_stack.len < min_vstack_len) {
            std.log.err("AssertValueStackSize: expected={} given={}", .{ min_vstack_len, self.value_stack.len });
            return error.AssertValueStackSize;
        }

        const results_source_start = self.value_stack.len - function_type.results.len;

        for (function_type.results, values[results_source_start..]) |result, value| {
            if (std.meta.activeTag(value) != result) {
                std.log.err("AssertValueStackType: expected={} given={}", .{ result, std.meta.activeTag(value) });
                return error.AssertValueStackType;
            }
        }

        const results_destination_start = label.value_stack_length_at_push - function_type.parameters.len;

        std.mem.copyForwards(
            Value,
            values[results_destination_start..],
            values[results_source_start..self.value_stack.len],
        );

        self.value_stack.len = @as(Runtime.ValueStackLength, @intCast(results_destination_start + function_type.results.len));

        frame.instruction_pointer = label.end;
    }

    pub fn branchFromLabelIndex(self: *Self, frame: *Frame, index: u16) !void {
        const i = std.math.cast(LabelStackLength, self.label_stack.len - index) orelse {
            return error.TooBig;
        };

        if (i == frame.labels_start) {
            self.label_stack.len = frame.labels_start;
            frame.instruction_pointer = @intCast(frame.expression.instructions.len - 2);
            return;
        }

        const idx = i - 1;
        const label = self.label_stack.get(idx);

        switch (label.kind) {
            .block => {
                self.label_stack.len = idx;
                try self.endBlock(frame, label);
            },
            .loop => {
                self.label_stack.len = idx + 1;
                frame.instruction_pointer = label.start;
            },
        }
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

    pub fn pushAnyValue(self: *Self, value: Value) !void {
        try self.value_stack.append(value);
    }

    pub fn popAnyValue(self: *Self) !Value {
        const current_frame = self.current_frame orelse {
            return error.NoFrame;
        };

        if (self.value_stack.len <= current_frame.values_start) {
            return error.TryPopingLocals;
        }

        const value: Value = self.value_stack.popOrNull() orelse {
            return error.ValueStackEmpty;
        };

        return value;
    }

    pub fn popAnyReferenceValue(self: *Self) !AnyReference {
        const value: Value = try self.popAnyValue();

        return switch (value) {
            .function_reference => |r| .{ .function = r },
            .extern_reference => |r| .{ .@"extern" = r },
            else => error.WrongType,
        };
    }

    pub fn popValue(self: *Self, comptime T: type) !T {
        const value: Value = try self.popAnyValue();

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
        .values_start = runtime.value_stack.len,
    };

    runtime.current_frame = &root_frame;
    // defer runtime.current_frame = null;

    interpreter_loop: while (true) {
        const current_frame = runtime.current_frame orelse {
            return error.NoFrame;
        };

        const expression = current_frame.expression;

        if (current_frame.instruction_pointer >= expression.instructions.len) {
            return error.OutOfBounds;
        }

        const instruction = expression.instructions[current_frame.instruction_pointer];

        logExpressionInstruction(expression, current_frame.instruction_pointer);

        switch (instruction.tag) {
            .nop => {
                // NOTE: Do nothing.
                current_frame.instruction_pointer += 1;
            },

            .@"unreachable" => {
                logExpression(current_frame.expression);
                return error.Unreachable;
            },

            .call => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.call_payloads[payload_index];
                const function_index = payload.function_index;

                try pushCall(runtime, current_frame.module_instance_index, function_index);

                runtime.current_frame = &runtime.call_stack.slice()[runtime.call_stack.len - 1].frame;
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

                const reference = try module_instance.getAnyElement(payload.table_index, element_index);

                // TODO: assert function/exten fucntion_type match

                const ref: FunctionReference = switch (reference) {
                    .function => |ref| ref,
                    else => {
                        return error.Unsupported;
                    },
                };

                try pushCall(runtime, ref.module_instance_index, ref.function_index);

                runtime.current_frame = &runtime.call_stack.slice()[runtime.call_stack.len - 1].frame;
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

                try runtime.endBlock(current_frame, label);
                current_frame.instruction_pointer += 1;
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
                const expected_end = entry.frame.values_start + function_type.results.len;

                if (tag == .expression_end) {
                    if (runtime.value_stack.len != expected_end) {
                        std.log.err("expected_end={}, given_end={}", .{ expected_end, runtime.value_stack.len });
                        return error.AssertValueStackSize;
                    }
                }
                if (tag == .@"return") {
                    if (runtime.value_stack.len < expected_end) {
                        std.log.err("expected_end={}, given_end={}", .{ expected_end, runtime.value_stack.len });
                        return error.AssertValueStackSize;
                    }
                }

                const vstack = runtime.value_stack.slice();
                const results = vstack[(vstack.len - function_type.results.len)..vstack.len];

                for (function_type.results, results) |result_type, result_value| {
                    if (result_type != std.meta.activeTag(result_value)) {
                        return error.WrongResultType;
                    }
                }

                std.mem.copyForwards(
                    Value,
                    vstack[entry.parameters_start..],
                    results,
                );

                const results_length = std.math.cast(Runtime.ValueStackLength, function_type.results.len) orelse {
                    return error.TooBig;
                };

                runtime.value_stack.len = entry.parameters_start + results_length;

                const new_current_frame = if (runtime.call_stack.len == 0)
                    &root_frame
                else
                    &runtime.call_stack.slice()[runtime.call_stack.len - 1].frame;

                new_current_frame.instruction_pointer += 1;
                runtime.current_frame = new_current_frame;
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
                    .function_reference = .{
                        .module_instance_index = current_frame.module_instance_index,
                        .function_index = payload.function_index,
                    },
                });
                current_frame.instruction_pointer += 1;
            },

            .@"memory.size" => {
                const new_size = try runtime.program.@"memory.size"(current_frame.module_instance_index, 0);
                try runtime.pushValue(i32, new_size);

                current_frame.instruction_pointer += 1;
            },

            .@"memory.grow" => {
                const g = try runtime.popValue(i32);

                const growth = std.math.cast(MemoryPageIndex, g) orelse {
                    std.log.err("CastingError: {}", .{g});
                    return error.CastingError;
                };

                const new_size: i32 = runtime.program.@"memory.grow"(current_frame.module_instance_index, 0, growth) catch -1;

                try runtime.pushValue(i32, new_size);

                current_frame.instruction_pointer += 1;
            },

            .@"global.set" => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.global_accessor_payloads[payload_index];
                const value = try runtime.popAnyValue();

                try runtime.program.@"global.set"(current_frame.module_instance_index, payload.global_index, value);

                current_frame.instruction_pointer += 1;
            },

            .@"global.get" => {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.global_accessor_payloads[payload_index];
                const value = try runtime.program.@"global.get"(current_frame.module_instance_index, payload.global_index);

                try runtime.pushAnyValue(value);

                current_frame.instruction_pointer += 1;
            },

            inline .@"i32.load", .@"i64.load", .@"f32.load", .@"f64.load", .@"i32.load8_s", .@"i32.load8_u", .@"i32.load16_s", .@"i32.load16_u", .@"i64.load8_s", .@"i64.load8_u", .@"i64.load16_s", .@"i64.load16_u", .@"i64.load32_s", .@"i64.load32_u" => |tag| {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.memory_accessor_payloads[payload_index];

                const V: type = comptime switch (tag) {
                    .@"i32.load" => i32,
                    .@"i64.load" => i64,
                    .@"f32.load" => f32,
                    .@"f64.load" => f64,
                    .@"i32.load8_s" => i32,
                    .@"i32.load8_u" => i32,
                    .@"i32.load16_s" => i32,
                    .@"i32.load16_u" => i32,
                    .@"i64.load8_s" => i64,
                    .@"i64.load8_u" => i64,
                    .@"i64.load16_s" => i64,
                    .@"i64.load16_u" => i64,
                    .@"i64.load32_s" => i64,
                    .@"i64.load32_u" => i64,
                    else => @compileError("Unsupported type tag=" ++ tag),
                };

                const o = try runtime.popValue(i32);

                const offset = std.math.cast(u32, o) orelse {
                    return error.CastingError;
                };
                const addr = (payload.offset + offset);

                const value = try runtime.program.@"i.load"(V, current_frame.module_instance_index, 0, addr);

                try runtime.pushValue(V, value);

                current_frame.instruction_pointer += 1;
            },

            inline .@"i32.store", .@"i64.store", .@"f32.store", .@"f64.store", .@"i32.store8", .@"i32.store16", .@"i64.store8", .@"i64.store16", .@"i64.store32" => |tag| {
                const payload_index = instruction.payload_index orelse {
                    return error.NoPayloadIndex;
                };
                const payload = expression.memory_accessor_payloads[payload_index];

                const V: type = comptime switch (tag) {
                    .@"i32.store" => i32,
                    .@"i64.store" => i64,
                    .@"f32.store" => f32,
                    .@"f64.store" => f64,
                    .@"i32.store8" => i32,
                    .@"i32.store16" => i32,
                    .@"i64.store8" => i64,
                    .@"i64.store16" => i64,
                    .@"i64.store32" => i64,
                    else => @compileError("Unsupported type tag=" ++ tag),
                };

                const v = try runtime.popValue(V);
                const o = try runtime.popValue(i32);

                const offset = std.math.cast(u32, o) orelse {
                    return error.CastingError;
                };
                const addr = (payload.offset + offset);

                try runtime.program.@"i.store"(V, current_frame.module_instance_index, 0, addr, v);

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

            .@"ref.null function" => {
                return error.UnsupportedInstruction;
            },

            .@"ref.null extern" => {
                return error.UnsupportedInstruction;
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

    const frame = runtime.current_frame orelse {
        return error.NoFrame;
    };

    const expected_vstack_len = frame.values_start + parameters_length;

    if (runtime.value_stack.len < expected_vstack_len) {
        std.log.err("MissingParamaterOnStack: {} < {}", .{ runtime.value_stack.len, expected_vstack_len });
        return error.MissingParamaterOnStack;
    }

    const parameters_start = runtime.value_stack.len - parameters_length;
    const locals_start = parameters_start + parameters_length;
    const values_start = locals_start + locals_length;

    for (function_type.parameters, runtime.value_stack.slice()[parameters_start..]) |pt, v| {
        try v.assertValueType(pt);
    }

    if (locals_start != runtime.value_stack.len) {
        return error.AssertValueStackSize;
    }

    try pushLocals(runtime, function_body.locals);

    if (values_start != runtime.value_stack.len) {
        return error.AssertValueStackSize;
    }

    try runtime.call_stack.append(.{
        .frame = .{
            .instruction_pointer = 0,
            .module_instance_index = mi_idx,
            .expression = &function_body.expression,
            .labels_start = runtime.label_stack.len,
            .values_start = values_start,
        },
        .function_index = function_index,
        .function_type_index = function_type_index,
        .parameters_start = parameters_start,
        .locals_start = locals_start,
    });
}

const ExpressionBuilder = @import("./expression_builder.zig").ExpressionBuilder;

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

    var builder: ExpressionBuilder = ExpressionBuilder.init(allocator);
    defer builder.deinit();

    for (function_type.parameters, parameters) |pt, p| {
        if (std.meta.activeTag(p) == pt) {
            try builder.appendConst(p);
        } else {
            return error.InvalidParameterType;
        }
    }

    try builder.appendCall(.{
        .function_index = function_index,
    });
    try builder.appendEnd();

    var invoke_expression = try builder.build(allocator);
    defer invoke_expression.free(allocator);

    try executeExpression(runtime, root_module_instance_index, &invoke_expression);

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

    std.log.info("current_frame:", .{});
    if (runtime.current_frame) |frame| {
        std.log.info("  labels_start: {}", .{frame.labels_start});
        std.log.info("  values_start: {}", .{frame.values_start});
    } else {
        std.log.info("<null>", .{});
    }

    std.log.info("call stack:", .{});

    for (runtime.call_stack.slice(), 0..) |c, i| {
        std.log.info("  {d}: values_start={}", .{ i, c.frame.values_start });
        // std.log.info("  {d}: {}", .{ i, c });
    }

    std.log.info("label stack:", .{});

    for (runtime.label_stack.slice(), 0..) |c, i| {
        std.log.info("  {d}: {}", .{ i, c });
    }
    std.log.info("value stack:", .{});

    for (runtime.value_stack.slice(), 0..) |v, i| {
        std.log.info("  {d}: {}", .{ i, v });
    }

    std.log.info("+++++", .{});
}
