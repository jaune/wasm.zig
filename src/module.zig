const std = @import("std");

pub const InstructionTag = @import("./instruction_tag.zig").InstructionTag;

pub const max_module_instances = 10;
pub const ModuleInstanceIndex: type = std.math.IntFittingRange(0, max_module_instances);

pub const max_memory_pages = 128;
pub const MemoryPageIndex: type = std.math.IntFittingRange(0, max_memory_pages);

pub const MemoryInstanceIndex = u16;

pub const FunctionReference = struct {
    module_instance_index: ModuleInstanceIndex,
    function_index: FunctionIndex,
};

pub const max_extern_functions = 255;
pub const ExternReference: type = std.math.IntFittingRange(0, max_extern_functions);

pub const ReferenceType = enum {
    function,
    @"extern",
};

pub const AnyReference = union(ReferenceType) {
    function: FunctionReference,
    @"extern": ExternReference,
};

pub const MemoryIndex = u32;
pub const GlobalIndex = u32;
pub const TableIndex = u32;
pub const FunctionIndex = u32;

const AnyIndex = union(enum) {
    function: FunctionIndex,
    table: TableIndex,
    memory: MemoryIndex,
    global: GlobalIndex,
};

pub const Export = struct {
    index: AnyIndex,
    name: []const u8,
};

pub const FunctionType = struct {
    parameters: []ValueType,
    results: []ValueType,
};

pub const FunctionTypeIndex = u32;

pub const Module = struct {
    area: std.heap.ArenaAllocator,

    exports: []Export,
    empty_type_index: u32,
    function_types: []FunctionType,
    function_type_indices: []FunctionTypeIndex,
    function_bodies: []FunctionBody,
    tables: []Table,
    element_segments: []ElementSegment,
    memory_limits: []Limits,
    global_definitions: []GlobalDefinition,

    const Self = @This();

    pub fn findExportedFunctionIndex(self: *const Self, name: []const u8) ?FunctionIndex {
        for (self.exports) |e| {
            switch (e.index) {
                .function => |idx| {
                    if (std.mem.eql(u8, name, e.name)) {
                        return idx;
                    }
                },
                else => {},
            }
        }

        return null;
    }

    pub fn getFunctionTypeIndex(self: *const Self, fn_index: FunctionIndex) !FunctionTypeIndex {
        if (fn_index >= self.function_type_indices.len) {
            return error.OutOfBounds;
        }

        return self.function_type_indices[fn_index];
    }

    pub fn getFunctionType(self: *const Self, fn_index: FunctionIndex) !FunctionType {
        if (fn_index >= self.function_type_indices.len) {
            return error.OutOfBounds;
        }

        return self.function_types[self.function_type_indices[fn_index]];
    }

    pub fn deinit(self: Self) void {
        self.area.deinit();
    }
};

pub const FunctionBody = struct {
    locals: []Local,
    expression: Expression,
};

const Tuple = std.meta.Tuple;
const EmptyTuple: type = Tuple(&[_]type{});

pub const Local = struct {
    n: u32,
    type: ValueType,
};

pub const ValueType = enum {
    i32,
    i64,
    f32,
    f64,
    v128,
    function_reference,
    extern_reference,
};

pub const Value = union(ValueType) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,

    v128,

    function_reference: FunctionReference,
    extern_reference: ExternReference,

    pub fn assertValueType(v: Value, vt: ValueType) !void {
        if (std.meta.activeTag(v) != vt) {
            return error.AssertValueType;
        }
    }

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

pub const FunctionTypeTndex = u32;
pub const InstructionIndex = u32;

pub const LabelIndex = u16;

pub const LabelPayload = struct {
    start: InstructionIndex,
    end: InstructionIndex,
    function_type_index: FunctionTypeTndex,
};

pub const IfPayload = struct {
    true: InstructionIndex,
    false: InstructionIndex,
};

pub const CallPayload = struct {
    function_index: FunctionIndex,
};

pub const MemoryAccessorPayload = struct {
    @"align": u32,
    offset: u32,
};

pub const LocalAccessorPayload = struct {
    local_index: u32,
};

pub const GlobalAccessorPayload = struct {
    global_index: u32,
};

pub const FunctionReferencePayload = struct {
    function_index: FunctionTypeIndex,
};

pub const CallIndirectPayload = struct {
    function_type_index: FunctionTypeIndex,
    table_index: TableIndex,
};

pub const BranchPayload = struct {
    label_index: LabelIndex,
};

pub const ConstantPayload = struct {
    value: Value,
};

pub const BranchTablePayload = struct {
    branches: []LabelIndex,
    fallback: LabelIndex,
};

pub const Expression = struct {
    instructions: []Instruction,

    branch_payloads: []BranchPayload,
    label_payloads: []LabelPayload,
    constant_payloads: []ConstantPayload,
    branch_table_payloads: []BranchTablePayload,
    if_payloads: []IfPayload,
    call_payloads: []CallPayload,
    call_indirect_payloads: []CallIndirectPayload,
    memory_accessor_payloads: []MemoryAccessorPayload,
    local_accessor_payloads: []LocalAccessorPayload,
    global_accessor_payloads: []GlobalAccessorPayload,
    function_reference_payloads: []FunctionReferencePayload,

    const Self = @This();

    pub fn free(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.instructions);

        allocator.free(self.branch_payloads);
        allocator.free(self.label_payloads);
        allocator.free(self.constant_payloads);
        allocator.free(self.branch_table_payloads);
        allocator.free(self.if_payloads);
        allocator.free(self.call_payloads);
        allocator.free(self.call_indirect_payloads);
        allocator.free(self.memory_accessor_payloads);
        allocator.free(self.local_accessor_payloads);
        allocator.free(self.global_accessor_payloads);
        allocator.free(self.function_reference_payloads);
    }
};

pub const InstructionPayloadIndex = u16;

pub const Instruction = struct {
    tag: InstructionTag,
    payload_index: ?InstructionPayloadIndex = null,
};

pub const Table = struct {
    type: ReferenceType,
    limits: Limits,
};

pub const Limits = struct {
    min: u32,
    max: ?u32 = null,
};

pub const ElementSegment = struct {
    const Mode = union(enum) {
        passive: void,
        decalrative: void,
        active: struct {
            table: u32,
            offset: Expression,
        },
    };

    init: []Expression,
    type: ReferenceType,
    mode: Mode,
};

pub const GlobalDefinition = struct {
    type: ValueType,
    mutability: Mutability,
    expression: Expression,
};

pub const Mutability = enum {
    constant,
    variable,
};

pub fn logExpressionInstruction(expression: *const Expression, i: usize) void {
    const e = expression.instructions[i];

    switch (e.tag) {
        .@"n.const" => {
            const payload_index = e.payload_index.?;
            const payload = expression.constant_payloads[payload_index];

            std.log.info("{}: {s} (value={})", .{ i, @tagName(e.tag), payload.value });
        },
        .@"if" => {
            const payload_index = e.payload_index.?;
            const payload = expression.if_payloads[payload_index];

            std.log.info("{}: {s} (true={} false={})", .{
                i,
                @tagName(e.tag),
                payload.true,
                payload.false,
            });
        },
        .block => {
            const payload_index = e.payload_index.?;
            const payload = expression.label_payloads[payload_index];

            std.log.info("{}: {s} (start={} end={} function_type_index={})", .{
                i,
                @tagName(e.tag),
                payload.start,
                payload.end,
                payload.function_type_index,
            });
        },
        .branch => {
            const payload_index = e.payload_index.?;
            const payload = expression.branch_payloads[payload_index];

            std.log.info("{}: {s} (label_index={})", .{
                i,
                @tagName(e.tag),
                payload.label_index,
            });
        },
        else => {
            std.log.info("{}: {s}", .{ i, @tagName(e.tag) });
        },
    }
}

pub fn logExpression(expression: *const Expression) void {
    for (0..expression.instructions.len) |i| {
        logExpressionInstruction(expression, i);
    }
}

pub fn logFunctionBody(body: *const FunctionBody) void {
    std.log.info("+ FunctionBody +", .{});

    std.log.info("++ Locals ++", .{});
    for (body.locals, 0..) |l, i| {
        std.log.info("{}: {}", .{ i, l });
    }

    std.log.info("++ Expression ++", .{});
    logExpression(body.expression);

    std.log.info("+++++", .{});
}
