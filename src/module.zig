const std = @import("std");

pub const InstructionTag = @import("./instruction_tag.zig").InstructionTag;

pub const Export = struct {
    pub const DescritionType = enum {
        function,
        table,
        memory,
        global,
    };

    index: u32,
    name: []const u8,
    type: DescritionType,
};

pub const FunctionType = struct {
    parameters: []ValueType,
    results: []ValueType,
};

pub const FunctionTypeIndex = u32;
pub const FunctionIndex = u32;

pub const Module = struct {
    area: std.heap.ArenaAllocator,

    exports: []Export,
    empty_type_index: u32,
    function_types: []FunctionType,
    function_type_indices: []FunctionTypeIndex,
    function_bodies: []FunctionBody,

    const Self = @This();

    pub fn findExportedFunctionIndex(self: *const Self, name: []const u8) ?u32 {
        for (self.exports) |e| {
            if (e.type == .function and std.mem.eql(u8, name, e.name)) {
                return e.index;
            }
        }

        return null;
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

const TableIndex = u32;

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

pub const InstructionArguments = union {
    @"local.get": Tuple(&[_]type{u32}),
    @"local.set": Tuple(&[_]type{u32}),
    @"local.tee": Tuple(&[_]type{u32}),

    @"global.get": Tuple(&[_]type{u32}),
    @"global.set": Tuple(&[_]type{u32}),
};

pub fn ArgumentsTypeOfInstruction(comptime tag: InstructionTag) type {
    const field_index = std.meta.fieldIndex(InstructionArguments, @tagName(tag)) orelse {
        @compileError("InstructionArguments no argument type for " ++ @tagName(tag));
    };
    return std.meta.fields(InstructionArguments)[field_index].type;
}

pub const Expression = struct {
    instructions: []Instruction,
    instruction_arguments: []InstructionArguments,

    branch_payloads: []BranchPayload,
    label_payloads: []LabelPayload,
    constant_payloads: []ConstantPayload,
    branch_table_payloads: []BranchTablePayload,
    if_payloads: []IfPayload,
    call_payloads: []CallPayload,
    call_indirect_payloads: []CallIndirectPayload,
    memory_accessor_payloads: []MemoryAccessorPayload,
};

pub const InstructionPayloadIndex = u16;

pub const Instruction = struct {
    tag: InstructionTag,
    payload_index: ?InstructionPayloadIndex = null,
};

pub const ReferenceType = enum {
    function,
    @"extern",
};

pub const Table = struct {
    element: ReferenceType,
    limits: Limits,
};

pub const Limits = struct {
    min: u32,
    max: ?u32 = null,
};
