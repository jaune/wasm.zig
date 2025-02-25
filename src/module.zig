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

pub const Module = struct {
    area: std.heap.ArenaAllocator,

    exports: []Export,
    empty_type_index: u32,
    function_types: []FunctionType,
    function_type_indices: []u32,
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

pub const FunctionTypeTndex = u32;
pub const InstructionIndex = u32;

const LabelArguments = struct {
    start: InstructionIndex,
    end: InstructionIndex,
    function_type_index: FunctionTypeTndex,
};

pub const InstructionArguments = union {
    br: Tuple(&[_]type{u32}),
    br_if: Tuple(&[_]type{u32}),

    block: LabelArguments,
    loop: LabelArguments,

    @"local.get": Tuple(&[_]type{u32}),
    @"local.set": Tuple(&[_]type{u32}),
    @"local.tee": Tuple(&[_]type{u32}),
    @"global.get": Tuple(&[_]type{u32}),
    @"global.set": Tuple(&[_]type{u32}),

    @"i32.const": Tuple(&[_]type{i32}),
    @"i64.const": Tuple(&[_]type{i64}),
    @"f32.const": Tuple(&[_]type{f32}),
    @"f64.const": Tuple(&[_]type{f64}),
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
};

pub const Instruction = struct {
    tag: InstructionTag,
    arguments_index: ?u32 = null,
};
