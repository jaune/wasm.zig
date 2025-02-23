const std = @import("std");

const readInstructionTag = @import("./read_instruction_tag.zig").readInstructionTag;
const InstructionTag = @import("./instruction_tag.zig").InstructionTag;

const Reader = std.fs.File.Reader;
const ExpressionReader = std.io.LimitedReader(Reader).Reader;

const ValueType = @import("./module.zig").ValueType;
const Instruction = @import("./module.zig").Instruction;
const Local = @import("./module.zig").Local;
const Export = @import("./module.zig").Export;
const FunctionType = @import("./module.zig").FunctionType;
const FunctionBody = @import("./module.zig").FunctionBody;
const Module = @import("./module.zig").Module;

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

fn enumFromInt(comptime E: type, raw: u8) !E {
    const enum_info = @typeInfo(E).Enum;

    if (enum_info.tag_type != u8) {
        @compileError("Enum tag type should be u8");
    }

    inline for (enum_info.fields) |field| {
        if (raw == field.value) {
            return @enumFromInt(field.value);
        }
    }

    std.log.err("InvalidEnumValue: {s} raw=0x{x}", .{ @typeName(E), raw });
    return error.InvalidEnumValue;
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

fn readResultType(allocator: std.mem.Allocator, reader: Reader) ![]ValueType {
    const count = try std.leb.readULEB128(u32, reader);

    const result_type = try allocator.alloc(ValueType, count);

    for (result_type) |*value_type| {
        value_type.* = (try readEnum(ValueTypeCode, reader)).toValueType();
    }

    return result_type;
}

const BlockType = union(enum) {
    empty,
    value_type: ValueTypeCode,
};

fn readBlockType(reader: ExpressionReader) !BlockType {
    const b = try reader.readByte();

    if (b == 0x40) {
        return .empty;
    }

    const e = enumFromInt(ValueTypeCode, b) catch {
        return error.UnsupportedBlockType;
    };

    return BlockType{ .value_type = e };
}

const BranchTable = struct {
    branches: []u32,
    fallback: u32,
};

fn readBranchTable(allocator: std.mem.Allocator, reader: ExpressionReader) !BranchTable {
    const count = try std.leb.readULEB128(u32, reader);

    const branches = try allocator.alloc(u32, count);

    for (branches) |*branch| {
        branch.* = try std.leb.readULEB128(u32, reader);
    }

    return BranchTable{
        .branches = branches,
        .fallback = try std.leb.readULEB128(u32, reader),
    };
}

fn readExpression(allocator: std.mem.Allocator, reader: ExpressionReader) ![]Instruction {
    var instructions = std.ArrayList(Instruction).init(allocator);

    var stack_height: usize = 1;

    while (true) {
        const tag = try readInstructionTag(reader);

        switch (tag) {
            .end => {
                if (stack_height == 0) {
                    return error.NothingToPop;
                }
                stack_height -= 1;
                try instructions.append(.{ .end = .{} });

                if (stack_height == 0) {
                    break;
                }
            },
            .block => {
                const bt = try readBlockType(reader);

                switch (bt) {
                    .value_type => |vt| {
                        stack_height += 1;

                        std.log.debug("block bt={}", .{vt});
                    },
                    .empty => {
                        stack_height += 1;
                        std.log.debug("block bt=<empty>", .{});
                    },
                }

                try instructions.append(.{
                    .block = .{},
                });
            },
            .loop => {
                const bt = try readBlockType(reader);

                switch (bt) {
                    .value_type => |vt| {
                        stack_height += 1;

                        std.log.debug("loop bt={}", .{vt});
                    },
                    .empty => {
                        stack_height += 1;
                        std.log.debug("loop bt=<empty>", .{});
                    },
                }

                try instructions.append(.{
                    .loop = .{},
                });
            },

            .@"if" => {
                const bt = try readBlockType(reader);

                switch (bt) {
                    .value_type => |vt| {
                        stack_height += 1;

                        std.log.debug("if bt={}", .{vt});
                    },
                    .empty => {
                        stack_height += 1;
                        std.log.debug("if bt=<empty>", .{});
                    },
                }

                try instructions.append(.{
                    .@"if" = .{},
                });
            },

            .@"else" => {
                try instructions.append(.{
                    .@"else" = .{},
                });
            },

            .br_table => {
                _ = try readBranchTable(allocator, reader);

                try instructions.append(.{
                    .br_table = .{},
                });
            },
            else => {
                const instruction = try readInstructionParameters(reader, tag);

                try instructions.append(instruction);
            },
        }
    }

    if (stack_height != 0) {
        return error.UnfinishedBusiness;
    }

    return try instructions.toOwnedSlice();
}

fn readInstructionTuple(comptime T: type, reader: anytype) !T {
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
            .Float => |t| {
                switch (t.bits) {
                    32 => {
                        @field(params, p.name) = @as(f32, @bitCast(try reader.readInt(u32, .little)));
                    },
                    64 => {
                        @field(params, p.name) = @as(f64, @bitCast(try reader.readInt(u64, .little)));
                    },
                    else => {
                        @compileError("Unsupported instruction parameters type: parameter type: " ++ @typeName(p.type));
                    },
                }
            },
            else => {
                @compileError("Unsupported instruction parameters type: parameter type: " ++ @typeName(p.type));
            },
        }
    }

    return params;
}

fn readInstructionParameters(reader: anytype, tag: InstructionTag) !Instruction {
    const enum_info = @typeInfo(InstructionTag).Enum;

    inline for (enum_info.fields, 0..) |field, field_index| {
        if (@intFromEnum(tag) == field.value) {
            const field_info: std.builtin.Type.UnionField = std.meta.fields(Instruction)[field_index];
            const tuple_type = field_info.type;
            const tuple_type_info = @typeInfo(tuple_type);

            if (tuple_type_info.Struct.is_tuple) {
                const params = try readInstructionTuple(tuple_type, reader);

                return @unionInit(
                    Instruction,
                    field.name,
                    params,
                );
            } else {
                @compileError("Unsupported instruction parameters type: tuple");
            }
        }
    }

    std.log.err("InvalidInstructionParameters: {}", .{tag});
    return error.InvalidInstructionParameters;
}

fn readLocals(allocator: std.mem.Allocator, reader: Reader) ![]Local {
    const count = try std.leb.readULEB128(u32, reader);

    const locals = try allocator.alloc(Local, count);

    for (locals) |*local| {
        const n = try std.leb.readULEB128(u32, reader);
        const t = try readEnum(ValueTypeCode, reader);

        local.* = .{
            .n = n,
            .type = t.toValueType(),
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

fn readFunctionTypeSection(allocator: std.mem.Allocator, reader: Reader) ![]FunctionType {
    const count = try std.leb.readULEB128(u32, reader);

    const function_types = try allocator.alloc(FunctionType, count);

    for (function_types) |*function_type| {
        const head = try reader.readByte();

        if (head != 0x60) {
            return error.InvalidTypeSectionHead;
        }

        function_type.* = .{
            .parameters = try readResultType(allocator, reader),
            .results = try readResultType(allocator, reader),
        };
    }

    return function_types;
}

fn readCodeSection(allocator: std.mem.Allocator, reader: Reader) ![]FunctionBody {
    const count = try std.leb.readULEB128(u32, reader);

    const codes = try allocator.alloc(FunctionBody, count);

    for (codes) |*code| {
        const code_size = try std.leb.readULEB128(u32, reader);

        const position_before_locals = try reader.context.getPos();

        const locals = try readLocals(allocator, reader);

        const locals_size = try reader.context.getPos() - position_before_locals;

        const expression_size = code_size - locals_size;

        var expression_reader = std.io.limitedReader(reader, expression_size);

        code.* = .{
            .locals = locals,
            .expression = try readExpression(allocator, expression_reader.reader()),
        };

        if (expression_reader.bytes_left != 0) {
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
    var function_types: []FunctionType = &.{};
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

        switch (secton_id) {
            .type_section => {
                function_types = try readFunctionTypeSection(area_allocator, reader);
            },
            .function_section => {
                function_type_indices = try readFunctionSection(area_allocator, reader);
            },
            .export_section => {
                exports = try readExportSection(area_allocator, reader);
            },
            .code_section => {
                function_bodies = try readCodeSection(area_allocator, reader);
            },
            else => {
                std.log.info("Section {}: skiped", .{secton_id});
                try reader.skipBytes(section_size, .{});
            },
        }
    }

    return .{
        .area = area,
        .exports = exports,
        .function_types = function_types,
        .function_type_indices = function_type_indices,
        .function_bodies = function_bodies,
    };
}
