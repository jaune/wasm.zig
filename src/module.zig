const std = @import("std");

const Reader = std.fs.File.Reader;
const ExpressionReader = std.io.LimitedReader(Reader).Reader;

const Export = struct {
    index: u32,
    name: []const u8,
    type: ExportDescritionType,
};

pub const FunctionType = struct {
    parameters: []ValueType,
    results: []ValueType,
};

pub const Module = struct {
    area: std.heap.ArenaAllocator,

    exports: []Export,
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

    pub fn readAlloc(allocator: std.mem.Allocator, reader: Reader) !Self {
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

    pub fn deinit(self: Self) void {
        self.area.deinit();
    }
};

const FunctionBody = struct {
    locals: []Local,
    expression: []Instruction,
};

pub const InstructionTag = enum(u8) {
    // Numeric Instructions
    @"i32.const" = 0x41,
    @"i64.const" = 0x42,
    @"f32.const" = 0x43,
    @"f64.const" = 0x44,

    @"i32.eqz" = 0x45,
    @"i32.eq" = 0x46,
    @"i32.ne" = 0x47,
    @"i32.lt_s" = 0x48,
    @"i32.lt_u" = 0x49,
    @"i32.gt_s" = 0x4A,
    @"i32.gt_u" = 0x4B,
    @"i32.le_s" = 0x4C,
    @"i32.le_u" = 0x4D,
    @"i32.ge_s" = 0x4E,
    @"i32.ge_u" = 0x4F,
    @"i64.eqz" = 0x50,
    @"i64.eq" = 0x51,
    @"i64.ne" = 0x52,
    @"i64.lt_s" = 0x53,
    @"i64.lt_u" = 0x54,
    @"i64.gt_s" = 0x55,
    @"i64.gt_u" = 0x56,
    @"i64.le_s" = 0x57,
    @"i64.le_u" = 0x58,
    @"i64.ge_s" = 0x59,
    @"i64.ge_u" = 0x5A,
    @"f32.eq" = 0x5B,
    @"f32.ne" = 0x5C,
    @"f32.lt" = 0x5D,
    @"f32.gt" = 0x5E,
    @"f32.le" = 0x5F,
    @"f32.ge" = 0x60,
    @"f64.eq" = 0x61,
    @"f64.ne" = 0x62,
    @"f64.lt" = 0x63,
    @"f64.gt" = 0x64,
    @"f64.le" = 0x65,
    @"f64.ge" = 0x66,
    @"i32.clz" = 0x67,
    @"i32.ctz" = 0x68,
    @"i32.popcnt" = 0x69,
    @"i32.add" = 0x6A,
    @"i32.sub" = 0x6B,
    @"i32.mul" = 0x6C,
    @"i32.div_s" = 0x6D,
    @"i32.div_u" = 0x6E,
    @"i32.rem_s" = 0x6F,
    @"i32.rem_u" = 0x70,
    @"i32.and" = 0x71,
    @"i32.or" = 0x72,
    @"i32.xor" = 0x73,
    @"i32.shl" = 0x74,
    @"i32.shr_s" = 0x75,
    @"i32.shr_u" = 0x76,
    @"i32.rotl" = 0x77,
    @"i32.rotr" = 0x78,
    @"i64.clz" = 0x79,
    @"i64.ctz" = 0x7A,
    @"i64.popcnt" = 0x7B,
    @"i64.add" = 0x7C,
    @"i64.sub" = 0x7D,
    @"i64.mul" = 0x7E,
    @"i64.div_s" = 0x7F,
    @"i64.div_u" = 0x80,
    @"i64.rem_s" = 0x81,
    @"i64.rem_u" = 0x82,
    @"i64.and" = 0x83,
    @"i64.or" = 0x84,
    @"i64.xor" = 0x85,
    @"i64.shl" = 0x86,
    @"i64.shr_s" = 0x87,
    @"i64.shr_u" = 0x88,
    @"i64.rotl" = 0x89,
    @"i64.rotr" = 0x8A,
    @"f32.abs" = 0x8B,
    @"f32.neg" = 0x8C,
    @"f32.ceil" = 0x8D,
    @"f32.floor" = 0x8E,
    @"f32.trunc" = 0x8F,
    @"f32.nearest" = 0x90,
    @"f32.sqrt" = 0x91,
    @"f32.add" = 0x92,
    @"f32.sub" = 0x93,
    @"f32.mul" = 0x94,
    @"f32.div" = 0x95,
    @"f32.min" = 0x96,
    @"f32.max" = 0x97,
    @"f32.copysign" = 0x98,
    @"f64.abs" = 0x99,
    @"f64.neg" = 0x9A,
    @"f64.ceil" = 0x9B,
    @"f64.floor" = 0x9C,
    @"f64.trunc" = 0x9D,
    @"f64.nearest" = 0x9E,
    @"f64.sqrt" = 0x9F,
    @"f64.add" = 0xA0,
    @"f64.sub" = 0xA1,
    @"f64.mul" = 0xA2,
    @"f64.div" = 0xA3,
    @"f64.min" = 0xA4,
    @"f64.max" = 0xA5,
    @"f64.copysign" = 0xA6,
    @"i32.wrap_i64" = 0xA7,
    @"i32.trunc_f32_s" = 0xA8,
    @"i32.trunc_f32_u" = 0xA9,
    @"i32.trunc_f64_s" = 0xAA,
    @"i32.trunc_f64_u" = 0xAB,
    @"i64.extend_i32_s" = 0xAC,
    @"i64.extend_i32_u" = 0xAD,
    @"i64.trunc_f32_s" = 0xAE,
    @"i64.trunc_f32_u" = 0xAF,
    @"i64.trunc_f64_s" = 0xB0,
    @"i64.trunc_f64_u" = 0xB1,
    @"f32.convert_i32_s" = 0xB2,
    @"f32.convert_i32_u" = 0xB3,
    @"f32.convert_i64_s" = 0xB4,
    @"f32.convert_i64_u" = 0xB5,
    @"f32.demote_f64" = 0xB6,
    @"f64.convert_i32_s" = 0xB7,
    @"f64.convert_i32_u" = 0xB8,
    @"f64.convert_i64_s" = 0xB9,
    @"f64.convert_i64_u" = 0xBA,
    @"f64.promote_f32" = 0xBB,
    @"i32.reinterpret_f32" = 0xBC,
    @"i64.reinterpret_f64" = 0xBD,
    @"f32.reinterpret_i32" = 0xBE,
    @"f64.reinterpret_i64" = 0xBF,
    @"i32.extend8_s" = 0xC0,
    @"i32.extend16_s" = 0xC1,
    @"i64.extend8_s" = 0xC2,
    @"i64.extend16_s" = 0xC3,
    @"i64.extend32_s" = 0xC4,

    // Variable Instructions
    @"local.get" = 0x20,
    @"local.set" = 0x21,
    @"local.tee" = 0x22,
    @"global.get" = 0x23,
    @"global.set" = 0x24,

    // End
    end = 0x0B,
};

const Tuple = std.meta.Tuple;
const EmptyTuple: type = Tuple(&[_]type{});

pub const Instruction = union(InstructionTag) {
    // Numeric Instructions
    @"i32.const": Tuple(&[_]type{i32}),
    @"i64.const": Tuple(&[_]type{i64}),
    @"f32.const": Tuple(&[_]type{f32}),
    @"f64.const": Tuple(&[_]type{f64}),

    @"i32.eqz": EmptyTuple,
    @"i32.eq": EmptyTuple,
    @"i32.ne": EmptyTuple,
    @"i32.lt_s": EmptyTuple,
    @"i32.lt_u": EmptyTuple,
    @"i32.gt_s": EmptyTuple,
    @"i32.gt_u": EmptyTuple,
    @"i32.le_s": EmptyTuple,
    @"i32.le_u": EmptyTuple,
    @"i32.ge_s": EmptyTuple,
    @"i32.ge_u": EmptyTuple,
    @"i64.eqz": EmptyTuple,
    @"i64.eq": EmptyTuple,
    @"i64.ne": EmptyTuple,
    @"i64.lt_s": EmptyTuple,
    @"i64.lt_u": EmptyTuple,
    @"i64.gt_s": EmptyTuple,
    @"i64.gt_u": EmptyTuple,
    @"i64.le_s": EmptyTuple,
    @"i64.le_u": EmptyTuple,
    @"i64.ge_s": EmptyTuple,
    @"i64.ge_u": EmptyTuple,
    @"f32.eq": EmptyTuple,
    @"f32.ne": EmptyTuple,
    @"f32.lt": EmptyTuple,
    @"f32.gt": EmptyTuple,
    @"f32.le": EmptyTuple,
    @"f32.ge": EmptyTuple,
    @"f64.eq": EmptyTuple,
    @"f64.ne": EmptyTuple,
    @"f64.lt": EmptyTuple,
    @"f64.gt": EmptyTuple,
    @"f64.le": EmptyTuple,
    @"f64.ge": EmptyTuple,
    @"i32.clz": EmptyTuple,
    @"i32.ctz": EmptyTuple,
    @"i32.popcnt": EmptyTuple,
    @"i32.add": EmptyTuple,
    @"i32.sub": EmptyTuple,
    @"i32.mul": EmptyTuple,
    @"i32.div_s": EmptyTuple,
    @"i32.div_u": EmptyTuple,
    @"i32.rem_s": EmptyTuple,
    @"i32.rem_u": EmptyTuple,
    @"i32.and": EmptyTuple,
    @"i32.or": EmptyTuple,
    @"i32.xor": EmptyTuple,
    @"i32.shl": EmptyTuple,
    @"i32.shr_s": EmptyTuple,
    @"i32.shr_u": EmptyTuple,
    @"i32.rotl": EmptyTuple,
    @"i32.rotr": EmptyTuple,
    @"i64.clz": EmptyTuple,
    @"i64.ctz": EmptyTuple,
    @"i64.popcnt": EmptyTuple,
    @"i64.add": EmptyTuple,
    @"i64.sub": EmptyTuple,
    @"i64.mul": EmptyTuple,
    @"i64.div_s": EmptyTuple,
    @"i64.div_u": EmptyTuple,
    @"i64.rem_s": EmptyTuple,
    @"i64.rem_u": EmptyTuple,
    @"i64.and": EmptyTuple,
    @"i64.or": EmptyTuple,
    @"i64.xor": EmptyTuple,
    @"i64.shl": EmptyTuple,
    @"i64.shr_s": EmptyTuple,
    @"i64.shr_u": EmptyTuple,
    @"i64.rotl": EmptyTuple,
    @"i64.rotr": EmptyTuple,
    @"f32.abs": EmptyTuple,
    @"f32.neg": EmptyTuple,
    @"f32.ceil": EmptyTuple,
    @"f32.floor": EmptyTuple,
    @"f32.trunc": EmptyTuple,
    @"f32.nearest": EmptyTuple,
    @"f32.sqrt": EmptyTuple,
    @"f32.add": EmptyTuple,
    @"f32.sub": EmptyTuple,
    @"f32.mul": EmptyTuple,
    @"f32.div": EmptyTuple,
    @"f32.min": EmptyTuple,
    @"f32.max": EmptyTuple,
    @"f32.copysign": EmptyTuple,
    @"f64.abs": EmptyTuple,
    @"f64.neg": EmptyTuple,
    @"f64.ceil": EmptyTuple,
    @"f64.floor": EmptyTuple,
    @"f64.trunc": EmptyTuple,
    @"f64.nearest": EmptyTuple,
    @"f64.sqrt": EmptyTuple,
    @"f64.add": EmptyTuple,
    @"f64.sub": EmptyTuple,
    @"f64.mul": EmptyTuple,
    @"f64.div": EmptyTuple,
    @"f64.min": EmptyTuple,
    @"f64.max": EmptyTuple,
    @"f64.copysign": EmptyTuple,
    @"i32.wrap_i64": EmptyTuple,
    @"i32.trunc_f32_s": EmptyTuple,
    @"i32.trunc_f32_u": EmptyTuple,
    @"i32.trunc_f64_s": EmptyTuple,
    @"i32.trunc_f64_u": EmptyTuple,
    @"i64.extend_i32_s": EmptyTuple,
    @"i64.extend_i32_u": EmptyTuple,
    @"i64.trunc_f32_s": EmptyTuple,
    @"i64.trunc_f32_u": EmptyTuple,
    @"i64.trunc_f64_s": EmptyTuple,
    @"i64.trunc_f64_u": EmptyTuple,
    @"f32.convert_i32_s": EmptyTuple,
    @"f32.convert_i32_u": EmptyTuple,
    @"f32.convert_i64_s": EmptyTuple,
    @"f32.convert_i64_u": EmptyTuple,
    @"f32.demote_f64": EmptyTuple,
    @"f64.convert_i32_s": EmptyTuple,
    @"f64.convert_i32_u": EmptyTuple,
    @"f64.convert_i64_s": EmptyTuple,
    @"f64.convert_i64_u": EmptyTuple,
    @"f64.promote_f32": EmptyTuple,
    @"i32.reinterpret_f32": EmptyTuple,
    @"i64.reinterpret_f64": EmptyTuple,
    @"f32.reinterpret_i32": EmptyTuple,
    @"f64.reinterpret_i64": EmptyTuple,
    @"i32.extend8_s": EmptyTuple,
    @"i32.extend16_s": EmptyTuple,
    @"i64.extend8_s": EmptyTuple,
    @"i64.extend16_s": EmptyTuple,
    @"i64.extend32_s": EmptyTuple,

    // Variable Instructions
    @"local.get": Tuple(&[_]type{u32}),
    @"local.set": Tuple(&[_]type{u32}),
    @"local.tee": Tuple(&[_]type{u32}),
    @"global.get": Tuple(&[_]type{u32}),
    @"global.set": Tuple(&[_]type{u32}),

    // End
    end: EmptyTuple,
};

const Local = struct {
    n: u32,
    t: ValueType,
};

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

fn readExpression(allocator: std.mem.Allocator, reader: ExpressionReader) ![]Instruction {
    const predicted_capacity = reader.context.bytes_left / 2;
    var instructions = try std.ArrayList(Instruction).initCapacity(allocator, predicted_capacity);

    while (true) {
        const tag = try readEnum(InstructionTag, reader);

        if (tag == .end) {
            try instructions.append(.{ .end = .{} });
            break;
        }

        const instruction = try readInstructionParameters(reader, tag);

        try instructions.append(instruction);
    }

    return try instructions.toOwnedSlice();
}

fn readInstructionParameters(reader: anytype, tag: InstructionTag) !Instruction {
    const enum_info = @typeInfo(InstructionTag).Enum;

    inline for (enum_info.fields, 0..) |field, field_index| {
        if (@intFromEnum(tag) == field.value) {
            const field_info: std.builtin.Type.UnionField = std.meta.fields(Instruction)[field_index];
            const tuple_type = field_info.type;
            const tuple_type_info = @typeInfo(tuple_type);

            if (tuple_type_info.Struct.is_tuple) {
                var params: tuple_type = undefined;

                inline for (std.meta.fields(tuple_type)) |p| {
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
        local.* = .{
            .n = try std.leb.readULEB128(u32, reader),
            .t = try readEnum(ValueType, reader),
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
            .type = try readEnum(ExportDescritionType, reader),
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

fn readResultType(allocator: std.mem.Allocator, reader: Reader) ![]ValueType {
    const count = try std.leb.readULEB128(u32, reader);

    const result_type = try allocator.alloc(ValueType, count);

    for (result_type) |*value_type| {
        value_type.* = try readEnum(ValueType, reader);
    }

    return result_type;
}

const ExportDescritionType = enum(u8) {
    function = 0x00,
    table = 0x01,
    memory = 0x02,
    global = 0x03,
};

const ValueType = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,

    f32 = 0x7D,
    f64 = 0x7C,

    v128 = 0x7B,

    function_reference = 0x70,
    extern_reference = 0x6F,
};

fn readEnum(comptime E: type, reader: anytype) !E {
    const enum_info = @typeInfo(E).Enum;

    if (enum_info.tag_type != u8) {
        @compileError("Enum tag type should be u8");
    }

    const raw = try reader.readByte();

    inline for (enum_info.fields) |field| {
        if (raw == field.value) {
            return @enumFromInt(field.value);
        }
    }

    std.log.err("InvalidEnumValue: {s} raw=0x{x}", .{ @typeName(E), raw });
    return error.InvalidEnumValue;
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
