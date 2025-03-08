const std = @import("std");

const Instruction = @import("./module.zig").Instruction;
const Expression = @import("./module.zig").Expression;

const BranchPayload = @import("./module.zig").BranchPayload;
const LabelPayload = @import("./module.zig").LabelPayload;
const ConstantPayload = @import("./module.zig").ConstantPayload;
const FunctionTypeIndex = @import("./module.zig").FunctionTypeIndex;
const BranchTablePayload = @import("./module.zig").BranchTablePayload;
const IfPayload = @import("./module.zig").IfPayload;
const CallPayload = @import("./module.zig").CallPayload;
const CallIndirectPayload = @import("./module.zig").CallIndirectPayload;
const MemoryAccessorPayload = @import("./module.zig").MemoryAccessorPayload;
const LocalAccessorPayload = @import("./module.zig").LocalAccessorPayload;
const GlobalAccessorPayload = @import("./module.zig").GlobalAccessorPayload;
const FunctionReferencePayload = @import("./module.zig").FunctionReferencePayload;

const InstructionTag = @import("./module.zig").InstructionTag;
const InstructionIndex = @import("./module.zig").InstructionIndex;
const InstructionPayloadIndex = @import("./module.zig").InstructionPayloadIndex;
const LabelIndex = @import("./module.zig").LabelIndex;
const FunctionIndex = @import("./module.zig").FunctionIndex;

const Value = @import("./module.zig").Value;

const LabelKind = enum {
    loop,
    block,
    @"if",
    @"else",
};
const LabelStackEntry = struct {
    kind: LabelKind,
    payload_index: InstructionPayloadIndex,
};

pub const ExpressionBuilder = struct {
    const Self = @This();

    label_stack: std.ArrayList(LabelStackEntry),
    is_expression_completed: bool,

    instructions: std.ArrayList(Instruction),

    branch_payloads: std.ArrayList(BranchPayload),
    label_payloads: std.ArrayList(LabelPayload),
    if_payloads: std.ArrayList(IfPayload),
    constant_payloads: std.ArrayList(ConstantPayload),
    branch_table_payloads: std.ArrayList(BranchTablePayload),
    call_payloads: std.ArrayList(CallPayload),
    call_indirect_payloads: std.ArrayList(CallIndirectPayload),
    memory_accessor_payloads: std.ArrayList(MemoryAccessorPayload),
    local_accessor_payloads: std.ArrayList(LocalAccessorPayload),
    global_accessor_payloads: std.ArrayList(GlobalAccessorPayload),
    function_reference_payloads: std.ArrayList(FunctionReferencePayload),

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .label_stack = std.ArrayList(LabelStackEntry).init(allocator),
            .is_expression_completed = false,

            .instructions = std.ArrayList(Instruction).init(allocator),
            .branch_payloads = std.ArrayList(BranchPayload).init(allocator),
            .label_payloads = std.ArrayList(LabelPayload).init(allocator),
            .if_payloads = std.ArrayList(IfPayload).init(allocator),
            .constant_payloads = std.ArrayList(ConstantPayload).init(allocator),
            .branch_table_payloads = std.ArrayList(BranchTablePayload).init(allocator),
            .call_payloads = std.ArrayList(CallPayload).init(allocator),
            .call_indirect_payloads = std.ArrayList(CallIndirectPayload).init(allocator),
            .memory_accessor_payloads = std.ArrayList(MemoryAccessorPayload).init(allocator),
            .local_accessor_payloads = std.ArrayList(LocalAccessorPayload).init(allocator),
            .global_accessor_payloads = std.ArrayList(GlobalAccessorPayload).init(allocator),
            .function_reference_payloads = std.ArrayList(FunctionReferencePayload).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.label_stack.deinit();
        self.instructions.deinit();
        self.branch_payloads.deinit();
        self.label_payloads.deinit();
        self.constant_payloads.deinit();
        self.branch_table_payloads.deinit();
        self.if_payloads.deinit();
        self.call_payloads.deinit();
        self.call_indirect_payloads.deinit();
        self.memory_accessor_payloads.deinit();
        self.local_accessor_payloads.deinit();
        self.global_accessor_payloads.deinit();
        self.function_reference_payloads.deinit();
    }

    pub fn currentIndex(self: *const Self) !InstructionIndex {
        return castOrError(InstructionIndex, self.instructions.items.len);
    }

    pub fn appendBranchTable(self: *Self, payload: BranchTablePayload) !void {
        try self.instructions.append(.{
            .tag = .branch_table,
            .payload_index = try castOrError(InstructionPayloadIndex, self.branch_table_payloads.items.len),
        });
        try self.branch_table_payloads.append(payload);
    }

    pub fn appendBranch(self: *Self, tag: InstructionTag, label_index: LabelIndex) !void {
        if (label_index > self.label_stack.items.len) {
            std.log.err("InvalidLabelIndex: label_index={}, self.label_stack.items.len={}", .{ label_index, self.label_stack.items.len });
            return error.InvalidLabelIndex;
        }

        try self.instructions.append(.{
            .tag = switch (tag) {
                .branch, .branch_if => |t| t,
                else => {
                    return error.Unreachable;
                },
            },
            .payload_index = try castOrError(InstructionPayloadIndex, self.branch_payloads.items.len),
        });
        try self.branch_payloads.append(.{
            .label_index = label_index,
        });
    }

    pub fn appendMemoryAccessor(self: *Self, tag: InstructionTag, payload: MemoryAccessorPayload) !void {
        try self.instructions.append(.{
            .tag = switch (tag) {
                .@"i32.load", .@"i64.load", .@"f32.load", .@"f64.load", .@"i32.load8_s", .@"i32.load8_u", .@"i32.load16_s", .@"i32.load16_u", .@"i64.load8_s", .@"i64.load8_u", .@"i64.load16_s", .@"i64.load16_u", .@"i64.load32_s", .@"i64.load32_u", .@"i32.store", .@"i64.store", .@"f32.store", .@"f64.store", .@"i32.store8", .@"i32.store16", .@"i64.store8", .@"i64.store16", .@"i64.store32" => |t| t,
                else => {
                    return error.Unsupported;
                },
            },
            .payload_index = try castOrError(InstructionPayloadIndex, self.memory_accessor_payloads.items.len),
        });
        try self.memory_accessor_payloads.append(payload);
    }

    pub fn appendCall(self: *Self, payload: CallPayload) !void {
        try self.instructions.append(.{
            .tag = .call,
            .payload_index = try castOrError(InstructionPayloadIndex, self.call_payloads.items.len),
        });
        try self.call_payloads.append(payload);
    }

    pub fn appendCallIndirect(self: *Self, payload: CallIndirectPayload) !void {
        try self.instructions.append(.{
            .tag = .call_indirect,
            .payload_index = try castOrError(InstructionPayloadIndex, self.call_indirect_payloads.items.len),
        });
        try self.call_indirect_payloads.append(payload);
    }

    pub fn appendLocalAccessor(self: *Self, tag: InstructionTag, local_index: u32) !void {
        try self.instructions.append(.{
            .tag = switch (tag) {
                .@"local.get", .@"local.set", .@"local.tee" => |t| t,
                else => {
                    return error.Unsupported;
                },
            },
            .payload_index = try castOrError(InstructionPayloadIndex, self.local_accessor_payloads.items.len),
        });
        try self.local_accessor_payloads.append(.{
            .local_index = local_index,
        });
    }

    pub fn appendGlobalAccessor(self: *Self, tag: InstructionTag, global_index: u32) !void {
        try self.instructions.append(.{
            .tag = switch (tag) {
                .@"global.get", .@"global.set" => |t| t,
                else => {
                    return error.Unsupported;
                },
            },
            .payload_index = try castOrError(InstructionPayloadIndex, self.global_accessor_payloads.items.len),
        });
        try self.global_accessor_payloads.append(.{
            .global_index = global_index,
        });
    }

    pub fn appendConst(self: *Self, value: Value) !void {
        try self.instructions.append(.{
            .tag = .@"n.const",
            .payload_index = try castOrError(InstructionPayloadIndex, self.constant_payloads.items.len),
        });
        try self.constant_payloads.append(.{
            .value = value,
        });
    }

    pub fn appendReferenceFunction(self: *Self, fn_idx: FunctionIndex) !void {
        try self.instructions.append(.{
            .tag = .@"ref.func",
            .payload_index = try castOrError(InstructionPayloadIndex, self.constant_payloads.items.len),
        });
        try self.function_reference_payloads.append(.{
            .function_index = fn_idx,
        });
    }

    pub fn appendBlock(self: *Self, kind: LabelKind, fn_idx: FunctionTypeIndex) !void {
        const k = switch (kind) {
            .block, .loop => |k| k,
            else => return error.Unsupported,
        };

        const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, self.instructions.items.len) orelse {
            return error.TooManyInstruction;
        };

        const payload_index = try castOrError(InstructionPayloadIndex, self.label_payloads.items.len);
        try self.instructions.append(.{
            .tag = switch (k) {
                .block => .block,
                .loop => .loop,
                else => return error.Unreachable,
            },
            .payload_index = payload_index,
        });
        try self.label_payloads.append(.{
            .start = instruction_index,
            .end = instruction_index,
            .function_type_index = fn_idx,
        });
        try self.label_stack.append(.{
            .kind = k,
            .payload_index = payload_index,
        });
    }

    pub fn appendIf(self: *Self, fn_idx: FunctionTypeIndex) !void {
        const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, self.instructions.items.len) orelse {
            return error.TooManyInstruction;
        };

        const block_instruction_index = instruction_index + 1;

        try self.instructions.append(.{
            .tag = .@"if",
            .payload_index = try castOrError(InstructionPayloadIndex, self.if_payloads.items.len),
        });
        try self.if_payloads.append(.{
            .true = block_instruction_index,
            .false = 0,
        });

        const payload_index = try castOrError(InstructionPayloadIndex, self.label_payloads.items.len);
        try self.instructions.append(.{
            .tag = .block,
            .payload_index = payload_index,
        });
        try self.label_payloads.append(.{
            .start = block_instruction_index,
            .end = 0,
            .function_type_index = fn_idx,
        });
        try self.label_stack.append(.{
            .kind = .@"if",
            .payload_index = payload_index,
        });
    }

    pub fn appendElse(self: *Self) !void {
        if (self.is_expression_completed) {
            return error.ExpressionAlreadyComplete;
        }

        try self.appendTag(.block_end);

        const instruction_index = try self.currentIndex();

        const if_label = self.label_stack.items[self.label_stack.items.len - 1];
        const if_label_payload = self.label_payloads.items[if_label.payload_index];

        if (if_label.kind != .@"if") {
            return error.PreviousLabelShouldBeIf;
        }

        const if_instruction = self.instructions.items[if_label_payload.start - 1];
        const if_instruction_payload = &self.if_payloads.items[if_instruction.payload_index.?];

        if_instruction_payload.false = instruction_index;

        const payload_index = try castOrError(InstructionPayloadIndex, self.label_payloads.items.len);
        try self.instructions.append(.{
            .tag = .block,
            .payload_index = payload_index,
        });
        try self.label_payloads.append(.{
            .start = instruction_index,
            .end = instruction_index,
            .function_type_index = if_label_payload.function_type_index,
        });
        try self.label_stack.append(.{
            .kind = .@"else",
            .payload_index = payload_index,
        });
    }

    pub fn appendEnd(self: *Self) !void {
        if (self.is_expression_completed) {
            return error.ExpressionAlreadyComplete;
        }

        if (self.label_stack.items.len == 0) {
            try self.appendTag(.expression_end);
            self.is_expression_completed = true;
            return;
        }

        const instruction_index = try self.currentIndex();

        const entry = self.label_stack.pop();

        const label_payload = &self.label_payloads.items[entry.payload_index];

        switch (entry.kind) {
            .loop => {
                try self.instructions.append(.{ .tag = .loop_end });

                label_payload.end = instruction_index;
            },
            .block => {
                try self.instructions.append(.{ .tag = .block_end });

                label_payload.end = instruction_index;
            },
            .@"if" => {
                try self.instructions.append(.{ .tag = .block_end });

                label_payload.end = instruction_index;

                const if_instruction = self.instructions.items[label_payload.start - 1];
                if (if_instruction.tag != .@"if") {
                    for (self.instructions.items) |i| {
                        std.log.debug("{}", .{i});
                    }

                    std.log.err("expected={}, given={}", .{ InstructionTag.@"if", if_instruction.tag });
                    return error.PreviousInstructionShouldBeIf;
                }
                const if_instruction_payload_index = if_instruction.payload_index orelse {
                    return error.NullPayload;
                };
                const if_instruction_payload = &self.if_payloads.items[if_instruction_payload_index];

                if_instruction_payload.false = instruction_index;
            },
            .@"else" => {
                try self.instructions.append(.{ .tag = .block_end });

                label_payload.end = instruction_index;

                const if_label = self.label_stack.pop();

                if (if_label.kind != .@"if") {
                    return error.PreviousLabelShouldBeIf;
                }
                const if_label_payload = &self.label_payloads.items[if_label.payload_index];

                if_label_payload.end = instruction_index;

                const if_instruction = self.instructions.items[if_label_payload.start - 1];
                if (if_instruction.tag != .@"if") {
                    return error.PreviousInstructionShouldBeIf;
                }
                const if_instruction_payload_index = if_instruction.payload_index orelse {
                    return error.NullPayload;
                };
                const if_instruction_payload = &self.if_payloads.items[if_instruction_payload_index];

                if_instruction_payload.false = label_payload.start;
            },
        }
    }

    pub fn appendTag(self: *Self, tag: InstructionTag) !void {
        // TODO: check tag is simple
        try self.instructions.append(.{ .tag = tag });
    }

    pub fn build(self: *const Self, allocator: std.mem.Allocator) !Expression {
        if (!self.is_expression_completed) {
            return error.UnfinishedBusiness;
        }

        if (self.label_stack.items.len != 0) {
            return error.UnfinishedBusiness;
        }

        return Expression{
            .instructions = try allocAndCopySlice(allocator, Instruction, self.instructions.items),
            .branch_payloads = try allocAndCopySlice(allocator, BranchPayload, self.branch_payloads.items),
            .label_payloads = try allocAndCopySlice(allocator, LabelPayload, self.label_payloads.items),
            .constant_payloads = try allocAndCopySlice(allocator, ConstantPayload, self.constant_payloads.items),
            .branch_table_payloads = try allocAndCopySlice(allocator, BranchTablePayload, self.branch_table_payloads.items),
            .if_payloads = try allocAndCopySlice(allocator, IfPayload, self.if_payloads.items),
            .call_payloads = try allocAndCopySlice(allocator, CallPayload, self.call_payloads.items),
            .call_indirect_payloads = try allocAndCopySlice(allocator, CallIndirectPayload, self.call_indirect_payloads.items),
            .memory_accessor_payloads = try allocAndCopySlice(allocator, MemoryAccessorPayload, self.memory_accessor_payloads.items),
            .local_accessor_payloads = try allocAndCopySlice(allocator, LocalAccessorPayload, self.local_accessor_payloads.items),
            .global_accessor_payloads = try allocAndCopySlice(allocator, GlobalAccessorPayload, self.global_accessor_payloads.items),
            .function_reference_payloads = try allocAndCopySlice(allocator, FunctionReferencePayload, self.function_reference_payloads.items),
        };
    }
};

fn allocAndCopySlice(allocator: std.mem.Allocator, comptime T: type, source: []T) ![]T {
    const result = try allocator.alloc(T, source.len);

    std.mem.copyBackwards(T, result, source);

    return result;
}

fn castOrError(comptime T: type, v: anytype) !T {
    return std.math.cast(T, v) orelse {
        return error.CastingError;
    };
}
