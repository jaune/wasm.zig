const std = @import("std");

const instructionTagFromOpcode = @import("./instruction_tag_from_opcode.zig").instructionTagFromOpcode;
const readOpcode = @import("./read_opcode.zig").readOpcode;
const InstructionTag = @import("./instruction_tag.zig").InstructionTag;

const Reader = std.fs.File.Reader;
const FileReader = Reader;

const Value = @import("./module.zig").Value;
const ValueType = @import("./module.zig").ValueType;
const Instruction = @import("./module.zig").Instruction;
const Local = @import("./module.zig").Local;
const Export = @import("./module.zig").Export;
const FunctionType = @import("./module.zig").FunctionType;
const FunctionBody = @import("./module.zig").FunctionBody;
const Module = @import("./module.zig").Module;
const Expression = @import("./module.zig").Expression;
const InstructionArguments = @import("./module.zig").InstructionArguments;
const ArgumentsTypeOfInstruction = @import("./module.zig").ArgumentsTypeOfInstruction;
const InstructionIndex = @import("./module.zig").InstructionIndex;
const InstructionPayloadIndex = @import("./module.zig").InstructionPayloadIndex;
const LabelIndex = @import("./module.zig").LabelIndex;
const BranchPayload = @import("./module.zig").BranchPayload;
const LabelPayload = @import("./module.zig").LabelPayload;
const ConstantPayload = @import("./module.zig").ConstantPayload;
const FunctionTypeIndex = @import("./module.zig").FunctionTypeIndex;
const BranchTablePayload = @import("./module.zig").BranchTablePayload;
const IfPayload = @import("./module.zig").IfPayload;
const CallPayload = @import("./module.zig").CallPayload;
const CallIndirectPayload = @import("./module.zig").CallIndirectPayload;
const MemoryAccessorPayload = @import("./module.zig").MemoryAccessorPayload;

const ValueTypeCode = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,

    f32 = 0x7D,
    f64 = 0x7C,

    v128 = 0x7B,

    function_reference = 0x70,
    extern_reference = 0x6F,

    fn toValueType(v: ValueTypeCode) ValueType {
        return switch (v) {
            .i32 => .i32,
            .i64 => .i64,
            .f32 => .f32,
            .f64 => .f64,
            .v128 => .v128,
            .function_reference => .function_reference,
            .extern_reference => .extern_reference,
        };
    }
};

const ExportDescritionTypeCode = enum(u8) {
    function = 0x00,
    table = 0x01,
    memory = 0x02,
    global = 0x03,

    fn toExportDescritionType(v: ExportDescritionTypeCode) Export.DescritionType {
        return switch (v) {
            .function => .function,
            .table => .table,
            .memory => .memory,
            .global => .global,
        };
    }
};

fn intToEnumOrNull(comptime E: type, raw: u8) ?E {
    const enum_info = @typeInfo(E).Enum;

    if (enum_info.tag_type != u8) {
        @compileError("Enum tag type should be u8");
    }

    inline for (enum_info.fields) |field| {
        if (raw == field.value) {
            return @enumFromInt(field.value);
        }
    }

    return null;
}

fn enumFromInt(comptime E: type, raw: u8) !E {
    return intToEnumOrNull(E, raw) orelse {
        std.log.err("InvalidEnumValue: {s} raw=0x{x}", .{ @typeName(E), raw });
        return error.InvalidEnumValue;
    };
}

fn readEnum(comptime E: type, reader: anytype) !E {
    const raw = try reader.readByte();
    return enumFromInt(E, raw);
}

const WASM_MAGIC = [_]u8{ 0x00, 0x61, 0x73, 0x6D };
const WASM_VERSION = [_]u8{ 0x01, 0x00, 0x00, 0x00 };

const SectionId = enum(u8) {
    custom_section = 0,
    type_section = 1,
    import_section = 2,
    function_section = 3,
    table_section = 4,
    memory_section = 5,
    global_section = 6,
    export_section = 7,
    start_section = 8,
    element_section = 9,
    code_section = 10,
    data_section = 11,
    data_count_section = 12,
};

fn allocResultTypeFromValueType(allocator: std.mem.Allocator, vt: ValueType) ![]ValueType {
    const result_type = try allocator.alloc(ValueType, 1);

    result_type[0] = vt;

    return result_type;
}

fn readValueType(reader: Reader) !ValueType {
    return (try readEnum(ValueTypeCode, reader)).toValueType();
}

fn readResultType(allocator: std.mem.Allocator, reader: Reader) ![]ValueType {
    const count = try std.leb.readULEB128(u32, reader);

    const result_type = try allocator.alloc(ValueType, count);

    for (result_type) |*value_type| {
        value_type.* = try readValueType(reader);
    }

    return result_type;
}

const BlockType = union(enum) {
    empty,
    value_type: ValueType,
    function_type_index: FunctionTypeIndex,

    fn toFunctionTypeIndex(self: BlockType, section: *const FunctionTypeSection) FunctionTypeIndex {
        return switch (self) {
            .function_type_index => self.function_type_index,
            .value_type => |vt| section.empty_type_index + 1 + @intFromEnum(vt),
            .empty => section.empty_type_index,
        };
    }
};

fn readBlockType(reader: FileReader) !BlockType {
    var file_reader_context: std.fs.File = reader.context;

    const starting_position = try file_reader_context.getPos();

    const file_reader = file_reader_context.reader();

    const b = try file_reader.readByte();

    if (b == 0x40) {
        return .empty;
    }

    const e = intToEnumOrNull(ValueTypeCode, b) orelse {
        try file_reader_context.seekTo(starting_position);

        const function_type_index = try std.leb.readILEB128(i32, file_reader);

        if (function_type_index < 0) {
            return error.InvalidFunctionTypeIndex;
        }

        const index: FunctionTypeIndex = std.math.cast(FunctionTypeIndex, function_type_index) orelse {
            return error.InvalidFunctionTypeIndex;
        };

        return BlockType{
            .function_type_index = index,
        };
    };

    return BlockType{ .value_type = e.toValueType() };
}

fn readBranchTablePaylaod(allocator: std.mem.Allocator, reader: FileReader) !BranchTablePayload {
    const count = try std.leb.readULEB128(u32, reader);

    const branches = try allocator.alloc(LabelIndex, count);

    for (branches) |*branch| {
        branch.* = try std.leb.readULEB128(LabelIndex, reader);
    }

    return BranchTablePayload{
        .branches = branches,
        .fallback = try std.leb.readULEB128(LabelIndex, reader),
    };
}

fn readValue(comptime T: type, reader: FileReader) !Value {
    return switch (T) {
        i32 => Value{ .i32 = try std.leb.readILEB128(i32, reader) },
        i64 => Value{ .i64 = try std.leb.readILEB128(i64, reader) },
        f32 => Value{ .f32 = try readFloat(f32, reader) },
        f64 => Value{ .f64 = try readFloat(f64, reader) },
        else => @compileError("Unhandle type " ++ @typeName(T)),
    };
}

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

fn readExpression(allocator: std.mem.Allocator, reader: FileReader, function_type_section: *const FunctionTypeSection) !Expression {
    var instructions = std.ArrayList(Instruction).init(allocator);
    var instruction_arguments = std.ArrayList(InstructionArguments).init(allocator);

    var label_stack = try std.BoundedArray(LabelStackEntry, 1024).init(0);
    var branch_payloads = try std.BoundedArray(BranchPayload, 1024).init(0);
    var label_payloads = try std.BoundedArray(LabelPayload, 1024).init(0);
    var if_payloads = try std.BoundedArray(IfPayload, 1024).init(0);
    var constant_payloads = try std.BoundedArray(ConstantPayload, 1024).init(0);
    var branch_table_payloads = try std.BoundedArray(BranchTablePayload, 1024).init(0);
    var call_payloads = try std.BoundedArray(CallPayload, 1024).init(0);
    var call_indirect_payloads = try std.BoundedArray(CallIndirectPayload, 1024).init(0);
    var memory_accessor_payloads = try std.BoundedArray(MemoryAccessorPayload, 1024).init(0);

    while (true) {
        const opcode = try readOpcode(reader);

        const maybe_simple_tag = instructionTagFromOpcode(opcode);

        if (maybe_simple_tag) |tag| {
            try instructions.append(.{ .tag = tag });
        } else {
            switch (opcode) {
                .end => {
                    if (label_stack.len == 0) {
                        try instructions.append(.{ .tag = .expression_end });
                        break;
                    }

                    const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, instructions.items.len) orelse {
                        return error.TooManyInstruction;
                    };

                    const entry = label_stack.pop();

                    const label_payload = &label_payloads.slice()[entry.payload_index];

                    switch (entry.kind) {
                        .loop => {
                            try instructions.append(.{ .tag = .loop_end });

                            label_payload.end = instruction_index;
                        },
                        .block => {
                            try instructions.append(.{ .tag = .block_end });

                            label_payload.end = instruction_index;
                        },
                        .@"if" => {
                            try instructions.append(.{ .tag = .block_end });

                            label_payload.end = instruction_index;

                            const if_instruction = instructions.items[label_payload.start - 1];
                            if (if_instruction.tag != .@"if") {
                                for (instructions.items) |i| {
                                    std.log.debug("{}", .{i});
                                }

                                std.log.err("expected={}, given={}", .{ InstructionTag.@"if", if_instruction.tag });
                                return error.PreviousInstructionShouldBeIf;
                            }
                            const if_instruction_payload_index = if_instruction.payload_index orelse {
                                return error.NullPayload;
                            };
                            const if_instruction_payload = &if_payloads.slice()[if_instruction_payload_index];

                            if_instruction_payload.false = instruction_index;
                        },
                        .@"else" => {
                            try instructions.append(.{ .tag = .block_end });

                            label_payload.end = instruction_index;

                            const if_label = label_stack.pop();

                            if (if_label.kind != .@"if") {
                                return error.PreviousLabelShouldBeIf;
                            }
                            const if_label_payload = &label_payloads.slice()[if_label.payload_index];

                            if_label_payload.end = instruction_index;

                            const if_instruction = instructions.items[if_label_payload.start - 1];
                            if (if_instruction.tag != .@"if") {
                                return error.PreviousInstructionShouldBeIf;
                            }
                            const if_instruction_payload_index = if_instruction.payload_index orelse {
                                return error.NullPayload;
                            };
                            const if_instruction_payload = &if_payloads.slice()[if_instruction_payload_index];

                            if_instruction_payload.false = label_payload.start;
                        },
                    }
                },

                .@"else" => {
                    try instructions.append(.{
                        .tag = .block_end,
                    });

                    const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, instructions.items.len) orelse {
                        return error.TooManyInstruction;
                    };

                    const if_label = label_stack.get(label_stack.len - 1);
                    const if_label_payload = label_payloads.slice()[if_label.payload_index];

                    if (if_label.kind != .@"if") {
                        return error.PreviousLabelShouldBeIf;
                    }

                    const if_instruction = instructions.items[if_label_payload.start - 1];
                    const if_instruction_payload = &if_payloads.slice()[if_instruction.payload_index.?];

                    if_instruction_payload.false = instruction_index;

                    const payload_index = label_payloads.len;
                    try instructions.append(.{
                        .tag = .block,
                        .payload_index = payload_index,
                    });
                    try label_payloads.append(.{
                        .start = instruction_index,
                        .end = instruction_index,
                        .function_type_index = if_label_payload.function_type_index,
                    });
                    try label_stack.append(.{
                        .kind = .@"else",
                        .payload_index = payload_index,
                    });
                },

                .@"if" => {
                    const bt = try readBlockType(reader);

                    const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, instructions.items.len) orelse {
                        return error.TooManyInstruction;
                    };

                    const block_instruction_index = instruction_index + 1;

                    try instructions.append(.{
                        .tag = .@"if",
                        .payload_index = if_payloads.len,
                    });
                    try if_payloads.append(.{
                        .true = block_instruction_index,
                        .false = 0,
                    });

                    const payload_index = label_payloads.len;
                    try instructions.append(.{
                        .tag = .block,
                        .payload_index = payload_index,
                    });
                    try label_payloads.append(.{
                        .start = block_instruction_index,
                        .end = 0,
                        .function_type_index = bt.toFunctionTypeIndex(function_type_section),
                    });
                    try label_stack.append(.{
                        .kind = .@"if",
                        .payload_index = payload_index,
                    });
                },

                .block, .loop => |op| {
                    const bt = try readBlockType(reader);

                    const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, instructions.items.len) orelse {
                        return error.TooManyInstruction;
                    };

                    const payload_index = label_payloads.len;
                    try instructions.append(.{
                        .tag = switch (op) {
                            .block => .block,
                            .loop => .loop,
                            .@"if" => .@"if",
                            else => return error.Unreachable,
                        },
                        .payload_index = payload_index,
                    });
                    try label_payloads.append(.{
                        .start = instruction_index,
                        .end = instruction_index,
                        .function_type_index = bt.toFunctionTypeIndex(function_type_section),
                    });
                    try label_stack.append(.{
                        .kind = switch (op) {
                            .block => .block,
                            .loop => .loop,
                            .@"if" => .@"if",
                            else => {
                                return error.Unreachable;
                            },
                        },
                        .payload_index = payload_index,
                    });
                },

                .@"i32.const", .@"i64.const", .@"f32.const", .@"f64.const" => |op| {
                    const value = switch (op) {
                        .@"i32.const" => try readValue(i32, reader),
                        .@"i64.const" => try readValue(i64, reader),
                        .@"f32.const" => try readValue(f32, reader),
                        .@"f64.const" => try readValue(f64, reader),
                        else => return error.Unreachable,
                    };

                    try instructions.append(.{
                        .tag = .@"n.const",
                        .payload_index = constant_payloads.len,
                    });
                    try constant_payloads.append(.{
                        .value = value,
                    });
                },

                .@"v128.const" => {
                    return error.UnhandledOpcode;
                },

                .call => {
                    try instructions.append(.{
                        .tag = .call,
                        .payload_index = call_payloads.len,
                    });
                    try call_payloads.append(.{
                        .function_index = try std.leb.readULEB128(u32, reader),
                    });
                },

                .call_indirect => {
                    try instructions.append(.{
                        .tag = .call_indirect,
                        .payload_index = call_indirect_payloads.len,
                    });
                    try call_indirect_payloads.append(.{
                        .function_type_index = try std.leb.readULEB128(u32, reader),
                        .table_index = try std.leb.readULEB128(u32, reader),
                    });
                },

                .@"i32.load", .@"i64.load", .@"f32.load", .@"f64.load", .@"i32.load8_s", .@"i32.load8_u", .@"i32.load16_s", .@"i32.load16_u", .@"i64.load8_s", .@"i64.load8_u", .@"i64.load16_s", .@"i64.load16_u", .@"i64.load32_s", .@"i64.load32_u", .@"i32.store", .@"i64.store", .@"f32.store", .@"f64.store", .@"i32.store8", .@"i32.store16", .@"i64.store8", .@"i64.store16", .@"i64.store32" => |tag| {
                    try instructions.append(.{
                        .tag = switch (tag) {
                            .@"i32.load" => .@"i32.load",
                            .@"i64.load" => .@"i64.load",
                            .@"f32.load" => .@"f32.load",
                            .@"f64.load" => .@"f64.load",
                            .@"i32.load8_s" => .@"i32.load8_s",
                            .@"i32.load8_u" => .@"i32.load8_u",
                            .@"i32.load16_s" => .@"i32.load16_s",
                            .@"i32.load16_u" => .@"i32.load16_u",
                            .@"i64.load8_s" => .@"i64.load8_s",
                            .@"i64.load8_u" => .@"i64.load8_u",
                            .@"i64.load16_s" => .@"i64.load16_s",
                            .@"i64.load16_u" => .@"i64.load16_u",
                            .@"i64.load32_s" => .@"i64.load32_s",
                            .@"i64.load32_u" => .@"i64.load32_u",
                            .@"i32.store" => .@"i32.store",
                            .@"i64.store" => .@"i64.store",
                            .@"f32.store" => .@"f32.store",
                            .@"f64.store" => .@"f64.store",
                            .@"i32.store8" => .@"i32.store8",
                            .@"i32.store16" => .@"i32.store16",
                            .@"i64.store8" => .@"i64.store8",
                            .@"i64.store16" => .@"i64.store16",
                            .@"i64.store32" => .@"i64.store32",
                            else => {
                                return error.Unreachable;
                            },
                        },
                        .payload_index = memory_accessor_payloads.len,
                    });
                    try memory_accessor_payloads.append(.{
                        .@"align" = try std.leb.readULEB128(u32, reader),
                        .offset = try std.leb.readULEB128(u32, reader),
                    });
                },

                .br_table => {
                    try instructions.append(.{
                        .tag = .branch_table,
                        .payload_index = branch_table_payloads.len,
                    });
                    try branch_table_payloads.append(try readBranchTablePaylaod(allocator, reader));
                },

                .br, .br_if => |op| {
                    const label_index = try std.leb.readULEB128(LabelIndex, reader);

                    if (label_index >= label_stack.len) {
                        return error.InvalidLabelIndex;
                    }

                    try instructions.append(.{
                        .tag = switch (op) {
                            .br => .branch,
                            .br_if => .branch_if,
                            else => {
                                return error.Unreachable;
                            },
                        },
                        .payload_index = branch_payloads.len,
                    });
                    try branch_payloads.append(.{
                        .label_index = label_index,
                    });
                },

                // tuple
                inline .@"local.get", .@"local.set", .@"local.tee", .@"global.get", .@"global.set" => |code| {
                    const tag_name = comptime @tagName(code);
                    @setEvalBranchQuota(10_000);
                    const tag = comptime std.meta.stringToEnum(InstructionTag, tag_name) orelse {
                        @compileError("Missing InstructionTag: " ++ tag_name);
                    };
                    @setEvalBranchQuota(1000);
                    const payload_index: InstructionPayloadIndex = std.math.cast(InstructionPayloadIndex, instruction_arguments.items.len) orelse {
                        return error.TooManyInstructionArguments;
                    };

                    const TupleType: type = comptime ArgumentsTypeOfInstruction(tag);
                    const tuple: TupleType = try readTuple(TupleType, reader);

                    const arguments = @unionInit(InstructionArguments, tag_name, tuple);

                    try instruction_arguments.append(arguments);
                    try instructions.append(.{
                        .tag = tag,
                        .payload_index = payload_index,
                    });
                },

                else => {
                    std.log.err("UnhandledOpcode: opcode={}", .{opcode});
                    return error.UnhandledOpcode;
                },
            }
        }
    }

    if (label_stack.len != 0) {
        return error.UnfinishedBusiness;
    }

    return Expression{
        .instructions = try instructions.toOwnedSlice(),
        .instruction_arguments = try instruction_arguments.toOwnedSlice(),
        .branch_payloads = try allocAndCopySlice(allocator, BranchPayload, branch_payloads.slice()),
        .label_payloads = try allocAndCopySlice(allocator, LabelPayload, label_payloads.slice()),
        .constant_payloads = try allocAndCopySlice(allocator, ConstantPayload, constant_payloads.slice()),
        .branch_table_payloads = try allocAndCopySlice(allocator, BranchTablePayload, branch_table_payloads.slice()),
        .if_payloads = try allocAndCopySlice(allocator, IfPayload, if_payloads.slice()),
        .call_payloads = try allocAndCopySlice(allocator, CallPayload, call_payloads.slice()),
        .call_indirect_payloads = try allocAndCopySlice(allocator, CallIndirectPayload, call_indirect_payloads.slice()),
        .memory_accessor_payloads = try allocAndCopySlice(allocator, MemoryAccessorPayload, memory_accessor_payloads.slice()),
    };
}

fn allocAndCopySlice(allocator: std.mem.Allocator, comptime T: type, source: []T) ![]T {
    const result = try allocator.alloc(T, source.len);

    std.mem.copyBackwards(T, result, source);

    return result;
}

fn readFloat(comptime T: type, reader: anytype) !T {
    const i = @typeInfo(T);

    return switch (i) {
        .Float => |t| {
            switch (t.bits) {
                32 => {
                    return @as(f32, @bitCast(try reader.readInt(u32, .little)));
                },
                64 => {
                    return @as(f64, @bitCast(try reader.readInt(u64, .little)));
                },
                else => {
                    @compileError("Unsupported instruction parameters type: parameter type: " ++ @typeName(T));
                },
            }
        },
        else => {
            @compileError("Unsupported instruction parameters type: parameter type: " ++ @typeName(T));
        },
    };
}

fn readTuple(comptime T: type, reader: anytype) !T {
    const t_info = @typeInfo(T);

    if (!t_info.Struct.is_tuple) {
        @compileError("Should be a tuple");
    }

    var params: T = undefined;

    inline for (std.meta.fields(T)) |p| {
        const p_info = @typeInfo(p.type);

        switch (p_info) {
            .Int => |t| {
                if (t.signedness == .signed) {
                    @field(params, p.name) = try std.leb.readILEB128(p.type, reader);
                } else {
                    @field(params, p.name) = try std.leb.readULEB128(p.type, reader);
                }
            },
            .Float => {
                @field(params, p.name) = try readFloat(p.type, reader);
            },
            else => {
                @compileError("Unsupported instruction parameters type: parameter type: " ++ @typeName(p.type));
            },
        }
    }

    return params;
}

fn readLocals(allocator: std.mem.Allocator, reader: Reader) ![]Local {
    const count = try std.leb.readULEB128(u32, reader);

    const locals = try allocator.alloc(Local, count);

    for (locals) |*local| {
        const n = try std.leb.readULEB128(u32, reader);

        local.* = .{
            .n = n,
            .type = try readValueType(reader),
        };
    }

    return locals;
}

fn readExportSection(allocator: std.mem.Allocator, reader: Reader) ![]Export {
    const count = try std.leb.readULEB128(u32, reader);

    const entries = try allocator.alloc(Export, count);

    for (entries) |*entry| {
        entry.* = .{
            .name = try readName(allocator, reader),
            .type = (try readEnum(ExportDescritionTypeCode, reader)).toExportDescritionType(),
            .index = try std.leb.readULEB128(u32, reader),
        };
    }

    return entries;
}

fn readName(allocator: std.mem.Allocator, reader: Reader) ![]u8 {
    const count = try std.leb.readULEB128(u32, reader);
    const name = try allocator.alloc(u8, count);

    const read = try reader.readAll(name);

    if (read != name.len) {
        return error.ReadAllFail;
    }

    return name;
}

fn readFunctionSection(allocator: std.mem.Allocator, reader: Reader) ![]u32 {
    const count = try std.leb.readULEB128(u32, reader);

    const function_type_indices = try allocator.alloc(u32, count);

    for (function_type_indices) |*index| {
        index.* = try std.leb.readULEB128(u32, reader);
    }

    return function_type_indices;
}

const FunctionTypeSection = struct {
    empty_type_index: u32,
    function_types: []FunctionType,
};

fn readFunctionTypeSection(allocator: std.mem.Allocator, reader: Reader) !FunctionTypeSection {
    const count = try std.leb.readULEB128(u32, reader);

    const total_type_count = count + 1 + std.meta.fields(ValueType).len;

    const function_types = try allocator.alloc(FunctionType, total_type_count);

    for (0..count) |i| {
        const head = try reader.readByte();

        if (head != 0x60) {
            return error.InvalidTypeSectionHead;
        }

        function_types[i] = .{
            .parameters = try readResultType(allocator, reader),
            .results = try readResultType(allocator, reader),
        };
    }

    function_types[count] = FunctionType{
        .parameters = &.{},
        .results = &.{},
    };

    inline for (std.meta.fields(ValueType), 0..) |f, i| {
        function_types[count + 1 + i] = FunctionType{
            .parameters = &.{},
            .results = try allocResultTypeFromValueType(allocator, @enumFromInt(f.value)),
        };
    }

    return FunctionTypeSection{
        .empty_type_index = count,
        .function_types = function_types,
    };
}

fn readCodeSection(allocator: std.mem.Allocator, reader: Reader, function_type_section: *const FunctionTypeSection) ![]FunctionBody {
    const count = try std.leb.readULEB128(u32, reader);

    const codes = try allocator.alloc(FunctionBody, count);

    for (codes, 0..) |*code, function_index| {
        const code_size = try std.leb.readULEB128(u32, reader);

        const position_before_locals = try reader.context.getPos();

        const locals = try readLocals(allocator, reader);

        const starting_position = try reader.context.getPos();
        const locals_size = starting_position - position_before_locals;

        const expression_size = code_size - locals_size;

        code.* = .{
            .locals = locals,
            .expression = readExpression(allocator, reader, function_type_section) catch |err| {
                std.log.err("{s}: function_index={}", .{ @errorName(err), function_index });
                return err;
            },
        };

        const ending_position = try reader.context.getPos();
        const processed_size = ending_position - starting_position;

        if (processed_size != expression_size) {
            std.log.err("ExpressionReaderHasBytesLeft: expected={} given={}", .{ expression_size, processed_size });
            return error.ExpressionReaderHasBytesLeft;
        }
    }

    return codes;
}

pub fn readAllAlloc(allocator: std.mem.Allocator, reader: Reader) !Module {
    var area = std.heap.ArenaAllocator.init(allocator);
    errdefer area.deinit();

    const magic = try reader.readBoundedBytes(WASM_MAGIC.len);

    if (!std.mem.eql(u8, magic.slice(), &WASM_MAGIC)) {
        return error.InvalidMagic;
    }

    const version = try reader.readBoundedBytes(WASM_VERSION.len);

    if (!std.mem.eql(u8, version.slice(), &WASM_VERSION)) {
        return error.InvalidVersion;
    }

    const area_allocator = area.allocator();

    var exports: []Export = &.{};
    var maybe_function_type_section: ?FunctionTypeSection = null;
    var function_type_indices: []u32 = &.{};
    var function_bodies: []FunctionBody = &.{};

    while (true) {
        const secton_id_raw = reader.readByte() catch |err| {
            if (err == Reader.NoEofError.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        if (secton_id_raw > 12) {
            return error.InvalidSectionId;
        }

        const secton_id: SectionId = @enumFromInt(secton_id_raw);
        const section_size = try std.leb.readULEB128(u32, reader);

        const starting_position = try reader.context.getPos();

        switch (secton_id) {
            .type_section => {
                maybe_function_type_section = try readFunctionTypeSection(area_allocator, reader);
            },
            .function_section => {
                function_type_indices = try readFunctionSection(area_allocator, reader);
            },
            .export_section => {
                exports = try readExportSection(area_allocator, reader);
            },
            .code_section => {
                const function_type_section = maybe_function_type_section orelse {
                    return error.NoTypeSection;
                };
                function_bodies = try readCodeSection(area_allocator, reader, &function_type_section);
            },
            .table_section => {
                _ = try readTableSection(area_allocator, reader);
            },
            .memory_section => {
                _ = try readMemorySection(area_allocator, reader);
            },
            .global_section => {
                const function_type_section = maybe_function_type_section orelse {
                    return error.NoTypeSection;
                };
                _ = try readGlobalSection(area_allocator, reader, &function_type_section);
            },
            else => {
                std.log.info("Section {}: skiped", .{secton_id});
                try reader.skipBytes(section_size, .{});
            },
        }

        const ending_position = try reader.context.getPos();
        const processed_size = ending_position - starting_position;

        if (section_size != processed_size) {
            std.log.info("Section {}: expected={}, given={}", .{ secton_id, section_size, processed_size });
            return error.WrongEndingPosition;
        }
    }

    const function_type_section = maybe_function_type_section orelse {
        return error.NoTypeSection;
    };

    return .{
        .area = area,
        .exports = exports,
        .empty_type_index = function_type_section.empty_type_index,
        .function_types = function_type_section.function_types,
        .function_type_indices = function_type_indices,
        .function_bodies = function_bodies,
    };
}

const Limits = @import("./module.zig").Limits;
const ReferenceType = @import("./module.zig").ReferenceType;
const Table = @import("./module.zig").Table;

fn readLimits(reader: Reader) !Limits {
    const selector = try reader.readByte();

    switch (selector) {
        0x00 => {
            return .{
                .min = try std.leb.readULEB128(u32, reader),
            };
        },
        0x01 => {
            return .{
                .min = try std.leb.readULEB128(u32, reader),
                .max = try std.leb.readULEB128(u32, reader),
            };
        },
        else => return error.MalformedLimits,
    }
}

fn readReferenceType(reader: Reader) !ReferenceType {
    const selector = try reader.readByte();

    return switch (selector) {
        0x70 => .function,
        0x6F => .@"extern",
        else => return error.MalformedReferenceType,
    };
}

fn readTableSection(allocator: std.mem.Allocator, reader: Reader) ![]Table {
    const count = try std.leb.readULEB128(u32, reader);

    const tables = try allocator.alloc(Table, count);

    for (tables) |*table| {
        table.* = .{
            .element = try readReferenceType(reader),
            .limits = try readLimits(reader),
        };
    }

    return tables;
}

fn readMemorySection(allocator: std.mem.Allocator, reader: Reader) ![]Limits {
    const count = try std.leb.readULEB128(u32, reader);

    const entries = try allocator.alloc(Limits, count);

    for (entries) |*entry| {
        entry.* = try readLimits(reader);
    }

    return entries;
}

const GlobalDefinition = @import("./module.zig").GlobalDefinition;
const Mutability = @import("./module.zig").Mutability;

fn readGlobalSection(allocator: std.mem.Allocator, reader: Reader, section_type_section: *const FunctionTypeSection) ![]GlobalDefinition {
    const count = try std.leb.readULEB128(u32, reader);

    const entries = try allocator.alloc(GlobalDefinition, count);

    for (entries) |*entry| {
        entry.* = .{
            .type = try readValueType(reader),
            .mutability = try readMutability(reader),
            .expression = try readExpression(allocator, reader, section_type_section),
        };
    }

    return entries;
}

fn readMutability(reader: Reader) !Mutability {
    const selector = try reader.readByte();

    return switch (selector) {
        0x00 => .constant,
        0x01 => .variable,
        else => return error.MalformedLimits,
    };
}
