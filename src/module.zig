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

const Tuple = std.meta.Tuple;
const EmptyTuple: type = Tuple(&[_]type{});

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
        const tag = try readInstructionTag(reader);

        if (tag == .end) {
            try instructions.append(.{ .end = .{} });
            break;
        }

        const instruction = try readInstructionParameters(reader, tag);

        try instructions.append(instruction);
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

const InstructionTag = enum {
    @"unreachable",
    nop,
    block,
    loop,
    @"if",
    @"else",
    end,
    br,
    br_if,
    br_table,
    @"return",
    call,
    call_indirect,
    drop,
    select,
    select_t,
    @"local.get",
    @"local.set",
    @"local.tee",
    @"global.get",
    @"global.set",
    @"table.get",
    @"table.set",
    @"i32.load",
    @"i64.load",
    @"f32.load",
    @"f64.load",
    @"i32.load8_s",
    @"i32.load8_u",
    @"i32.load16_s",
    @"i32.load16_u",
    @"i64.load8_s",
    @"i64.load8_u",
    @"i64.load16_s",
    @"i64.load16_u",
    @"i64.load32_s",
    @"i64.load32_u",
    @"i32.store",
    @"i64.store",
    @"f32.store",
    @"f64.store",
    @"i32.store8",
    @"i32.store16",
    @"i64.store8",
    @"i64.store16",
    @"i64.store32",
    @"memory.size",
    @"memory.grow",
    @"i32.const",
    @"i64.const",
    @"f32.const",
    @"f64.const",
    @"i32.eqz",
    @"i32.eq",
    @"i32.ne",
    @"i32.lt_s",
    @"i32.lt_u",
    @"i32.gt_s",
    @"i32.gt_u",
    @"i32.le_s",
    @"i32.le_u",
    @"i32.ge_s",
    @"i32.ge_u",
    @"i64.eqz",
    @"i64.eq",
    @"i64.ne",
    @"i64.lt_s",
    @"i64.lt_u",
    @"i64.gt_s",
    @"i64.gt_u",
    @"i64.le_s",
    @"i64.le_u",
    @"i64.ge_s",
    @"i64.ge_u",
    @"f32.eq",
    @"f32.ne",
    @"f32.lt",
    @"f32.gt",
    @"f32.le",
    @"f32.ge",
    @"f64.eq",
    @"f64.ne",
    @"f64.lt",
    @"f64.gt",
    @"f64.le",
    @"f64.ge",
    @"i32.clz",
    @"i32.ctz",
    @"i32.popcnt",
    @"i32.add",
    @"i32.sub",
    @"i32.mul",
    @"i32.div_s",
    @"i32.div_u",
    @"i32.rem_s",
    @"i32.rem_u",
    @"i32.and",
    @"i32.or",
    @"i32.xor",
    @"i32.shl",
    @"i32.shr_s",
    @"i32.shr_u",
    @"i32.rotl",
    @"i32.rotr",
    @"i64.clz",
    @"i64.ctz",
    @"i64.popcnt",
    @"i64.add",
    @"i64.sub",
    @"i64.mul",
    @"i64.div_s",
    @"i64.div_u",
    @"i64.rem_s",
    @"i64.rem_u",
    @"i64.and",
    @"i64.or",
    @"i64.xor",
    @"i64.shl",
    @"i64.shr_s",
    @"i64.shr_u",
    @"i64.rotl",
    @"i64.rotr",
    @"f32.abs",
    @"f32.neg",
    @"f32.ceil",
    @"f32.floor",
    @"f32.trunc",
    @"f32.nearest",
    @"f32.sqrt",
    @"f32.add",
    @"f32.sub",
    @"f32.mul",
    @"f32.div",
    @"f32.min",
    @"f32.max",
    @"f32.copysign",
    @"f64.abs",
    @"f64.neg",
    @"f64.ceil",
    @"f64.floor",
    @"f64.trunc",
    @"f64.nearest",
    @"f64.sqrt",
    @"f64.add",
    @"f64.sub",
    @"f64.mul",
    @"f64.div",
    @"f64.min",
    @"f64.max",
    @"f64.copysign",
    @"i32.wrap_i64",
    @"i32.trunc_f32_s",
    @"i32.trunc_f32_u",
    @"i32.trunc_f64_s",
    @"i32.trunc_f64_u",
    @"i64.extend_i32_s",
    @"i64.extend_i32_u",
    @"i64.trunc_f32_s",
    @"i64.trunc_f32_u",
    @"i64.trunc_f64_s",
    @"i64.trunc_f64_u",
    @"f32.convert_i32_s",
    @"f32.convert_i32_u",
    @"f32.convert_i64_s",
    @"f32.convert_i64_u",
    @"f32.demote_f64",
    @"f64.convert_i32_s",
    @"f64.convert_i32_u",
    @"f64.convert_i64_s",
    @"f64.convert_i64_u",
    @"f64.promote_f32",
    @"i32.reinterpret_f32",
    @"i64.reinterpret_f64",
    @"f32.reinterpret_i32",
    @"f64.reinterpret_i64",
    @"i32.extend8_s",
    @"i32.extend16_s",
    @"i64.extend8_s",
    @"i64.extend16_s",
    @"i64.extend32_s",
    @"ref.null",
    @"ref.is_null",
    @"ref.func",
    @"i32.trunc_sat_f32_s",
    @"i32.trunc_sat_f32_u",
    @"i32.trunc_sat_f64_s",
    @"i32.trunc_sat_f64_u",
    @"i64.trunc_sat_f32_s",
    @"i64.trunc_sat_f32_u",
    @"i64.trunc_sat_f64_s",
    @"i64.trunc_sat_f64_u",
    @"memory.init",
    @"data.drop",
    @"memory.copy",
    @"memory.fill",
    @"table.init",
    @"elem.drop",
    @"table.copy",
    @"table.grow",
    @"table.size",
    @"table.fill",
    @"v128.load",
    @"v128.load8x8_s",
    @"v128.load8x8_u",
    @"v128.load16x4_s",
    @"v128.load16x4_u",
    @"v128.load32x2_s",
    @"v128.load32x2_u",
    @"v128.load8_splat",
    @"v128.load16_splat",
    @"v128.load32_splat",
    @"v128.load64_splat",
    @"v128.store",
    @"v128.const",
    @"i8x16.shuffle",
    @"i8x16.swizzle",
    @"i8x16.splat",
    @"i16x8.splat",
    @"i32x4.splat",
    @"i64x2.splat",
    @"f32x4.splat",
    @"f64x2.splat",
    @"i8x16.extract_lane_s",
    @"i8x16.extract_lane_u",
    @"i8x16.replace_lane",
    @"i16x8.extract_lane_s",
    @"i16x8.extract_lane_u",
    @"i16x8.replace_lane",
    @"i32x4.extract_lane",
    @"i32x4.replace_lane",
    @"i64x2.extract_lane",
    @"i64x2.replace_lane",
    @"f32x4.extract_lane",
    @"f32x4.replace_lane",
    @"f64x2.extract_lane",
    @"f64x2.replace_lane",
    @"i8x16.eq",
    @"i8x16.ne",
    @"i8x16.lt_s",
    @"i8x16.lt_u",
    @"i8x16.gt_s",
    @"i8x16.gt_u",
    @"i8x16.le_s",
    @"i8x16.le_u",
    @"i8x16.ge_s",
    @"i8x16.ge_u",
    @"i16x8.eq",
    @"i16x8.ne",
    @"i16x8.lt_s",
    @"i16x8.lt_u",
    @"i16x8.gt_s",
    @"i16x8.gt_u",
    @"i16x8.le_s",
    @"i16x8.le_u",
    @"i16x8.ge_s",
    @"i16x8.ge_u",
    @"i32x4.eq",
    @"i32x4.ne",
    @"i32x4.lt_s",
    @"i32x4.lt_u",
    @"i32x4.gt_s",
    @"i32x4.gt_u",
    @"i32x4.le_s",
    @"i32x4.le_u",
    @"i32x4.ge_s",
    @"i32x4.ge_u",
    @"f32x4.eq",
    @"f32x4.ne",
    @"f32x4.lt",
    @"f32x4.gt",
    @"f32x4.le",
    @"f32x4.ge",
    @"f64x2.eq",
    @"f64x2.ne",
    @"f64x2.lt",
    @"f64x2.gt",
    @"f64x2.le",
    @"f64x2.ge",
    @"v128.not",
    @"v128.and",
    @"v128.andnot",
    @"v128.or",
    @"v128.xor",
    @"v128.bitselect",
    @"v128.any_true",
    @"v128.load8_lane",
    @"v128.load16_lane",
    @"v128.load32_lane",
    @"v128.load64_lane",
    @"v128.store8_lane",
    @"v128.store16_lane",
    @"v128.store32_lane",
    @"v128.store64_lane",
    @"v128.load32_zero",
    @"v128.load64_zero",
    @"f32x4.demote_f64x2_zero",
    @"f64x2.promote_low_f32x4",
    @"i8x16.abs",
    @"i8x16.neg",
    @"i8x16.popcnt",
    @"i8x16.all_true",
    @"i8x16.bitmask",
    @"i8x16.narrow_i16x8_s",
    @"i8x16.narrow_i16x8_u",
    @"f32x4.ceil",
    @"f32x4.floor",
    @"f32x4.trunc",
    @"f32x4.nearest",
    @"i8x16.shl",
    @"i8x16.shr_s",
    @"i8x16.shr_u",
    @"i8x16.add",
    @"i8x16.add_sat_s",
    @"i8x16.add_sat_u",
    @"i8x16.sub",
    @"i8x16.sub_sat_s",
    @"i8x16.sub_sat_u",
    @"f64x2.ceil",
    @"f64x2.floor",
    @"i8x16.min_s",
    @"i8x16.min_u",
    @"i8x16.max_s",
    @"i8x16.max_u",
    @"f64x2.trunc",
    @"i8x16.avgr_u",
    @"i16x8.extadd_pairwise_i8x16_s",
    @"i16x8.extadd_pairwise_i8x16_u",
    @"i32x4.extadd_pairwise_i16x8_s",
    @"i32x4.extadd_pairwise_i16x8_u",
    @"i16x8.abs",
    @"i16x8.neg",
    @"i16x8.q15mulr_sat_s",
    @"i16x8.all_true",
    @"i16x8.bitmask",
    @"i16x8.narrow_i32x4_s",
    @"i16x8.narrow_i32x4_u",
    @"i16x8.extend_low_i8x16_s",
    @"i16x8.extend_high_i8x16_s",
    @"i16x8.extend_low_i8x16_u",
    @"i16x8.extend_high_i8x16_u",
    @"i16x8.shl",
    @"i16x8.shr_s",
    @"i16x8.shr_u",
    @"i16x8.add",
    @"i16x8.add_sat_s",
    @"i16x8.add_sat_u",
    @"i16x8.sub",
    @"i16x8.sub_sat_s",
    @"i16x8.sub_sat_u",
    @"f64x2.nearest",
    @"i16x8.mul",
    @"i16x8.min_s",
    @"i16x8.min_u",
    @"i16x8.max_s",
    @"i16x8.max_u",
    @"i16x8.avgr_u",
    @"i16x8.extmul_low_i8x16_s",
    @"i16x8.extmul_high_i8x16_s",
    @"i16x8.extmul_low_i8x16_u",
    @"i16x8.extmul_high_i8x16_u",
    @"i32x4.abs",
    @"i32x4.neg",
    @"i32x4.all_true",
    @"i32x4.bitmask",
    @"i32x4.extend_low_i16x8_s",
    @"i32x4.extend_high_i16x8_s",
    @"i32x4.extend_low_i16x8_u",
    @"i32x4.extend_high_i16x8_u",
    @"i32x4.shl",
    @"i32x4.shr_s",
    @"i32x4.shr_u",
    @"i32x4.add",
    @"i32x4.sub",
    @"i32x4.mul",
    @"i32x4.min_s",
    @"i32x4.min_u",
    @"i32x4.max_s",
    @"i32x4.max_u",
    @"i32x4.dot_i16x8_s",
    @"i32x4.extmul_low_i16x8_s",
    @"i32x4.extmul_high_i16x8_s",
    @"i32x4.extmul_low_i16x8_u",
    @"i32x4.extmul_high_i16x8_u",
    @"i64x2.abs",
    @"i64x2.neg",
    @"i64x2.all_true",
    @"i64x2.bitmask",
    @"i64x2.extend_low_i32x4_s",
    @"i64x2.extend_high_i32x4_s",
    @"i64x2.extend_low_i32x4_u",
    @"i64x2.extend_high_i32x4_u",
    @"i64x2.shl",
    @"i64x2.shr_s",
    @"i64x2.shr_u",
    @"i64x2.add",
    @"i64x2.sub",
    @"i64x2.mul",
    @"i64x2.eq",
    @"i64x2.ne",
    @"i64x2.lt_s",
    @"i64x2.gt_s",
    @"i64x2.le_s",
    @"i64x2.ge_s",
    @"i64x2.extmul_low_i32x4_s",
    @"i64x2.extmul_high_i32x4_s",
    @"i64x2.extmul_low_i32x4_u",
    @"i64x2.extmul_high_i32x4_u",
    @"f32x4.abs",
    @"f32x4.neg",
    @"f32x4.sqrt",
    @"f32x4.add",
    @"f32x4.sub",
    @"f32x4.mul",
    @"f32x4.div",
    @"f32x4.min",
    @"f32x4.max",
    @"f32x4.pmin",
    @"f32x4.pmax",
    @"f64x2.abs",
    @"f64x2.neg",
    @"f64x2.sqrt",
    @"f64x2.add",
    @"f64x2.sub",
    @"f64x2.mul",
    @"f64x2.div",
    @"f64x2.min",
    @"f64x2.max",
    @"f64x2.pmin",
    @"f64x2.pmax",
    @"i32x4.trunc_sat_f32x4_s",
    @"i32x4.trunc_sat_f32x4_u",
    @"f32x4.convert_i32x4_s",
    @"f32x4.convert_i32x4_u",
    @"i32x4.trunc_sat_f64x2_s_zero",
    @"i32x4.trunc_sat_f64x2_u_zero",
    @"f64x2.convert_low_i32x4_s",
    @"f64x2.convert_low_i32x4_u",
};

fn readInstructionTag(reader: anytype) !InstructionTag {
    const op0 = try reader.readByte();
    switch (op0) {
        0x00 => {
            return .@"unreachable";
        },
        0x01 => {
            return .nop;
        },
        0x02 => {
            return .block;
        },
        0x03 => {
            return .loop;
        },
        0x04 => {
            return .@"if";
        },
        0x05 => {
            return .@"else";
        },
        0x0B => {
            return .end;
        },
        0x0C => {
            return .br;
        },
        0x0D => {
            return .br_if;
        },
        0x0E => {
            return .br_table;
        },
        0x0F => {
            return .@"return";
        },
        0x10 => {
            return .call;
        },
        0x11 => {
            return .call_indirect;
        },
        0x1A => {
            return .drop;
        },
        0x1B => {
            return .select;
        },
        0x1C => {
            return .select;
        },
        0x20 => {
            return .@"local.get";
        },
        0x21 => {
            return .@"local.set";
        },
        0x22 => {
            return .@"local.tee";
        },
        0x23 => {
            return .@"global.get";
        },
        0x24 => {
            return .@"global.set";
        },
        0x25 => {
            return .@"table.get";
        },
        0x26 => {
            return .@"table.set";
        },
        0x28 => {
            return .@"i32.load";
        },
        0x29 => {
            return .@"i64.load";
        },
        0x2A => {
            return .@"f32.load";
        },
        0x2B => {
            return .@"f64.load";
        },
        0x2C => {
            return .@"i32.load8_s";
        },
        0x2D => {
            return .@"i32.load8_u";
        },
        0x2E => {
            return .@"i32.load16_s";
        },
        0x2F => {
            return .@"i32.load16_u";
        },
        0x30 => {
            return .@"i64.load8_s";
        },
        0x31 => {
            return .@"i64.load8_u";
        },
        0x32 => {
            return .@"i64.load16_s";
        },
        0x33 => {
            return .@"i64.load16_u";
        },
        0x34 => {
            return .@"i64.load32_s";
        },
        0x35 => {
            return .@"i64.load32_u";
        },
        0x36 => {
            return .@"i32.store";
        },
        0x37 => {
            return .@"i64.store";
        },
        0x38 => {
            return .@"f32.store";
        },
        0x39 => {
            return .@"f64.store";
        },
        0x3A => {
            return .@"i32.store8";
        },
        0x3B => {
            return .@"i32.store16";
        },
        0x3C => {
            return .@"i64.store8";
        },
        0x3D => {
            return .@"i64.store16";
        },
        0x3E => {
            return .@"i64.store32";
        },
        0x3F => {
            return .@"memory.size";
        },
        0x40 => {
            return .@"memory.grow";
        },
        0x41 => {
            return .@"i32.const";
        },
        0x42 => {
            return .@"i64.const";
        },
        0x43 => {
            return .@"f32.const";
        },
        0x44 => {
            return .@"f64.const";
        },
        0x45 => {
            return .@"i32.eqz";
        },
        0x46 => {
            return .@"i32.eq";
        },
        0x47 => {
            return .@"i32.ne";
        },
        0x48 => {
            return .@"i32.lt_s";
        },
        0x49 => {
            return .@"i32.lt_u";
        },
        0x4A => {
            return .@"i32.gt_s";
        },
        0x4B => {
            return .@"i32.gt_u";
        },
        0x4C => {
            return .@"i32.le_s";
        },
        0x4D => {
            return .@"i32.le_u";
        },
        0x4E => {
            return .@"i32.ge_s";
        },
        0x4F => {
            return .@"i32.ge_u";
        },
        0x50 => {
            return .@"i64.eqz";
        },
        0x51 => {
            return .@"i64.eq";
        },
        0x52 => {
            return .@"i64.ne";
        },
        0x53 => {
            return .@"i64.lt_s";
        },
        0x54 => {
            return .@"i64.lt_u";
        },
        0x55 => {
            return .@"i64.gt_s";
        },
        0x56 => {
            return .@"i64.gt_u";
        },
        0x57 => {
            return .@"i64.le_s";
        },
        0x58 => {
            return .@"i64.le_u";
        },
        0x59 => {
            return .@"i64.ge_s";
        },
        0x5A => {
            return .@"i64.ge_u";
        },
        0x5B => {
            return .@"f32.eq";
        },
        0x5C => {
            return .@"f32.ne";
        },
        0x5D => {
            return .@"f32.lt";
        },
        0x5E => {
            return .@"f32.gt";
        },
        0x5F => {
            return .@"f32.le";
        },
        0x60 => {
            return .@"f32.ge";
        },
        0x61 => {
            return .@"f64.eq";
        },
        0x62 => {
            return .@"f64.ne";
        },
        0x63 => {
            return .@"f64.lt";
        },
        0x64 => {
            return .@"f64.gt";
        },
        0x65 => {
            return .@"f64.le";
        },
        0x66 => {
            return .@"f64.ge";
        },
        0x67 => {
            return .@"i32.clz";
        },
        0x68 => {
            return .@"i32.ctz";
        },
        0x69 => {
            return .@"i32.popcnt";
        },
        0x6A => {
            return .@"i32.add";
        },
        0x6B => {
            return .@"i32.sub";
        },
        0x6C => {
            return .@"i32.mul";
        },
        0x6D => {
            return .@"i32.div_s";
        },
        0x6E => {
            return .@"i32.div_u";
        },
        0x6F => {
            return .@"i32.rem_s";
        },
        0x70 => {
            return .@"i32.rem_u";
        },
        0x71 => {
            return .@"i32.and";
        },
        0x72 => {
            return .@"i32.or";
        },
        0x73 => {
            return .@"i32.xor";
        },
        0x74 => {
            return .@"i32.shl";
        },
        0x75 => {
            return .@"i32.shr_s";
        },
        0x76 => {
            return .@"i32.shr_u";
        },
        0x77 => {
            return .@"i32.rotl";
        },
        0x78 => {
            return .@"i32.rotr";
        },
        0x79 => {
            return .@"i64.clz";
        },
        0x7A => {
            return .@"i64.ctz";
        },
        0x7B => {
            return .@"i64.popcnt";
        },
        0x7C => {
            return .@"i64.add";
        },
        0x7D => {
            return .@"i64.sub";
        },
        0x7E => {
            return .@"i64.mul";
        },
        0x7F => {
            return .@"i64.div_s";
        },
        0x80 => {
            return .@"i64.div_u";
        },
        0x81 => {
            return .@"i64.rem_s";
        },
        0x82 => {
            return .@"i64.rem_u";
        },
        0x83 => {
            return .@"i64.and";
        },
        0x84 => {
            return .@"i64.or";
        },
        0x85 => {
            return .@"i64.xor";
        },
        0x86 => {
            return .@"i64.shl";
        },
        0x87 => {
            return .@"i64.shr_s";
        },
        0x88 => {
            return .@"i64.shr_u";
        },
        0x89 => {
            return .@"i64.rotl";
        },
        0x8A => {
            return .@"i64.rotr";
        },
        0x8B => {
            return .@"f32.abs";
        },
        0x8C => {
            return .@"f32.neg";
        },
        0x8D => {
            return .@"f32.ceil";
        },
        0x8E => {
            return .@"f32.floor";
        },
        0x8F => {
            return .@"f32.trunc";
        },
        0x90 => {
            return .@"f32.nearest";
        },
        0x91 => {
            return .@"f32.sqrt";
        },
        0x92 => {
            return .@"f32.add";
        },
        0x93 => {
            return .@"f32.sub";
        },
        0x94 => {
            return .@"f32.mul";
        },
        0x95 => {
            return .@"f32.div";
        },
        0x96 => {
            return .@"f32.min";
        },
        0x97 => {
            return .@"f32.max";
        },
        0x98 => {
            return .@"f32.copysign";
        },
        0x99 => {
            return .@"f64.abs";
        },
        0x9A => {
            return .@"f64.neg";
        },
        0x9B => {
            return .@"f64.ceil";
        },
        0x9C => {
            return .@"f64.floor";
        },
        0x9D => {
            return .@"f64.trunc";
        },
        0x9E => {
            return .@"f64.nearest";
        },
        0x9F => {
            return .@"f64.sqrt";
        },
        0xA0 => {
            return .@"f64.add";
        },
        0xA1 => {
            return .@"f64.sub";
        },
        0xA2 => {
            return .@"f64.mul";
        },
        0xA3 => {
            return .@"f64.div";
        },
        0xA4 => {
            return .@"f64.min";
        },
        0xA5 => {
            return .@"f64.max";
        },
        0xA6 => {
            return .@"f64.copysign";
        },
        0xA7 => {
            return .@"i32.wrap_i64";
        },
        0xA8 => {
            return .@"i32.trunc_f32_s";
        },
        0xA9 => {
            return .@"i32.trunc_f32_u";
        },
        0xAA => {
            return .@"i32.trunc_f64_s";
        },
        0xAB => {
            return .@"i32.trunc_f64_u";
        },
        0xAC => {
            return .@"i64.extend_i32_s";
        },
        0xAD => {
            return .@"i64.extend_i32_u";
        },
        0xAE => {
            return .@"i64.trunc_f32_s";
        },
        0xAF => {
            return .@"i64.trunc_f32_u";
        },
        0xB0 => {
            return .@"i64.trunc_f64_s";
        },
        0xB1 => {
            return .@"i64.trunc_f64_u";
        },
        0xB2 => {
            return .@"f32.convert_i32_s";
        },
        0xB3 => {
            return .@"f32.convert_i32_u";
        },
        0xB4 => {
            return .@"f32.convert_i64_s";
        },
        0xB5 => {
            return .@"f32.convert_i64_u";
        },
        0xB6 => {
            return .@"f32.demote_f64";
        },
        0xB7 => {
            return .@"f64.convert_i32_s";
        },
        0xB8 => {
            return .@"f64.convert_i32_u";
        },
        0xB9 => {
            return .@"f64.convert_i64_s";
        },
        0xBA => {
            return .@"f64.convert_i64_u";
        },
        0xBB => {
            return .@"f64.promote_f32";
        },
        0xBC => {
            return .@"i32.reinterpret_f32";
        },
        0xBD => {
            return .@"i64.reinterpret_f64";
        },
        0xBE => {
            return .@"f32.reinterpret_i32";
        },
        0xBF => {
            return .@"f64.reinterpret_i64";
        },
        0xC0 => {
            return .@"i32.extend8_s";
        },
        0xC1 => {
            return .@"i32.extend16_s";
        },
        0xC2 => {
            return .@"i64.extend8_s";
        },
        0xC3 => {
            return .@"i64.extend16_s";
        },
        0xC4 => {
            return .@"i64.extend32_s";
        },
        0xD0 => {
            return .@"ref.null";
        },
        0xD1 => {
            return .@"ref.is_null";
        },
        0xD2 => {
            return .@"ref.func";
        },
        0xFC => {
            const op1 = try reader.readByte();
            switch (op1) {
                0x00 => {
                    return .@"i32.trunc_sat_f32_s";
                },
                0x01 => {
                    return .@"i32.trunc_sat_f32_u";
                },
                0x02 => {
                    return .@"i32.trunc_sat_f64_s";
                },
                0x03 => {
                    return .@"i32.trunc_sat_f64_u";
                },
                0x04 => {
                    return .@"i64.trunc_sat_f32_s";
                },
                0x05 => {
                    return .@"i64.trunc_sat_f32_u";
                },
                0x06 => {
                    return .@"i64.trunc_sat_f64_s";
                },
                0x07 => {
                    return .@"i64.trunc_sat_f64_u";
                },
                0x08 => {
                    return .@"memory.init";
                },
                0x09 => {
                    return .@"data.drop";
                },
                0x0A => {
                    return .@"memory.copy";
                },
                0x0B => {
                    return .@"memory.fill";
                },
                0x0C => {
                    return .@"table.init";
                },
                0x0D => {
                    return .@"elem.drop";
                },
                0x0E => {
                    return .@"table.copy";
                },
                0x0F => {
                    return .@"table.grow";
                },
                0x10 => {
                    return .@"table.size";
                },
                0x11 => {
                    return .@"table.fill";
                },
                else => {
                    return error.InvalidOpcode;
                },
            }
        },
        0xFD => {
            const op1 = try reader.readByte();
            switch (op1) {
                0x00 => {
                    return .@"v128.load";
                },
                0x01 => {
                    return .@"v128.load8x8_s";
                },
                0x02 => {
                    return .@"v128.load8x8_u";
                },
                0x03 => {
                    return .@"v128.load16x4_s";
                },
                0x04 => {
                    return .@"v128.load16x4_u";
                },
                0x05 => {
                    return .@"v128.load32x2_s";
                },
                0x06 => {
                    return .@"v128.load32x2_u";
                },
                0x07 => {
                    return .@"v128.load8_splat";
                },
                0x08 => {
                    return .@"v128.load16_splat";
                },
                0x09 => {
                    return .@"v128.load32_splat";
                },
                0x0A => {
                    return .@"v128.load64_splat";
                },
                0x0B => {
                    return .@"v128.store";
                },
                0x0C => {
                    return .@"v128.const";
                },
                0x0D => {
                    return .@"i8x16.shuffle";
                },
                0x0E => {
                    return .@"i8x16.swizzle";
                },
                0x0F => {
                    return .@"i8x16.splat";
                },
                0x10 => {
                    return .@"i16x8.splat";
                },
                0x11 => {
                    return .@"i32x4.splat";
                },
                0x12 => {
                    return .@"i64x2.splat";
                },
                0x13 => {
                    return .@"f32x4.splat";
                },
                0x14 => {
                    return .@"f64x2.splat";
                },
                0x15 => {
                    return .@"i8x16.extract_lane_s";
                },
                0x16 => {
                    return .@"i8x16.extract_lane_u";
                },
                0x17 => {
                    return .@"i8x16.replace_lane";
                },
                0x18 => {
                    return .@"i16x8.extract_lane_s";
                },
                0x19 => {
                    return .@"i16x8.extract_lane_u";
                },
                0x1A => {
                    return .@"i16x8.replace_lane";
                },
                0x1B => {
                    return .@"i32x4.extract_lane";
                },
                0x1C => {
                    return .@"i32x4.replace_lane";
                },
                0x1D => {
                    return .@"i64x2.extract_lane";
                },
                0x1E => {
                    return .@"i64x2.replace_lane";
                },
                0x1F => {
                    return .@"f32x4.extract_lane";
                },
                0x20 => {
                    return .@"f32x4.replace_lane";
                },
                0x21 => {
                    return .@"f64x2.extract_lane";
                },
                0x22 => {
                    return .@"f64x2.replace_lane";
                },
                0x23 => {
                    return .@"i8x16.eq";
                },
                0x24 => {
                    return .@"i8x16.ne";
                },
                0x25 => {
                    return .@"i8x16.lt_s";
                },
                0x26 => {
                    return .@"i8x16.lt_u";
                },
                0x27 => {
                    return .@"i8x16.gt_s";
                },
                0x28 => {
                    return .@"i8x16.gt_u";
                },
                0x29 => {
                    return .@"i8x16.le_s";
                },
                0x2A => {
                    return .@"i8x16.le_u";
                },
                0x2B => {
                    return .@"i8x16.ge_s";
                },
                0x2C => {
                    return .@"i8x16.ge_u";
                },
                0x2D => {
                    return .@"i16x8.eq";
                },
                0x2E => {
                    return .@"i16x8.ne";
                },
                0x2F => {
                    return .@"i16x8.lt_s";
                },
                0x30 => {
                    return .@"i16x8.lt_u";
                },
                0x31 => {
                    return .@"i16x8.gt_s";
                },
                0x32 => {
                    return .@"i16x8.gt_u";
                },
                0x33 => {
                    return .@"i16x8.le_s";
                },
                0x34 => {
                    return .@"i16x8.le_u";
                },
                0x35 => {
                    return .@"i16x8.ge_s";
                },
                0x36 => {
                    return .@"i16x8.ge_u";
                },
                0x37 => {
                    return .@"i32x4.eq";
                },
                0x38 => {
                    return .@"i32x4.ne";
                },
                0x39 => {
                    return .@"i32x4.lt_s";
                },
                0x3A => {
                    return .@"i32x4.lt_u";
                },
                0x3B => {
                    return .@"i32x4.gt_s";
                },
                0x3C => {
                    return .@"i32x4.gt_u";
                },
                0x3D => {
                    return .@"i32x4.le_s";
                },
                0x3E => {
                    return .@"i32x4.le_u";
                },
                0x3F => {
                    return .@"i32x4.ge_s";
                },
                0x40 => {
                    return .@"i32x4.ge_u";
                },
                0x41 => {
                    return .@"f32x4.eq";
                },
                0x42 => {
                    return .@"f32x4.ne";
                },
                0x43 => {
                    return .@"f32x4.lt";
                },
                0x44 => {
                    return .@"f32x4.gt";
                },
                0x45 => {
                    return .@"f32x4.le";
                },
                0x46 => {
                    return .@"f32x4.ge";
                },
                0x47 => {
                    return .@"f64x2.eq";
                },
                0x48 => {
                    return .@"f64x2.ne";
                },
                0x49 => {
                    return .@"f64x2.lt";
                },
                0x4A => {
                    return .@"f64x2.gt";
                },
                0x4B => {
                    return .@"f64x2.le";
                },
                0x4C => {
                    return .@"f64x2.ge";
                },
                0x4D => {
                    return .@"v128.not";
                },
                0x4E => {
                    return .@"v128.and";
                },
                0x4F => {
                    return .@"v128.andnot";
                },
                0x50 => {
                    return .@"v128.or";
                },
                0x51 => {
                    return .@"v128.xor";
                },
                0x52 => {
                    return .@"v128.bitselect";
                },
                0x53 => {
                    return .@"v128.any_true";
                },
                0x54 => {
                    return .@"v128.load8_lane";
                },
                0x55 => {
                    return .@"v128.load16_lane";
                },
                0x56 => {
                    return .@"v128.load32_lane";
                },
                0x57 => {
                    return .@"v128.load64_lane";
                },
                0x58 => {
                    return .@"v128.store8_lane";
                },
                0x59 => {
                    return .@"v128.store16_lane";
                },
                0x5A => {
                    return .@"v128.store32_lane";
                },
                0x5B => {
                    return .@"v128.store64_lane";
                },
                0x5C => {
                    return .@"v128.load32_zero";
                },
                0x5D => {
                    return .@"v128.load64_zero";
                },
                0x5E => {
                    return .@"f32x4.demote_f64x2_zero";
                },
                0x5F => {
                    return .@"f64x2.promote_low_f32x4";
                },
                0x60 => {
                    return .@"i8x16.abs";
                },
                0x61 => {
                    return .@"i8x16.neg";
                },
                0x62 => {
                    return .@"i8x16.popcnt";
                },
                0x63 => {
                    return .@"i8x16.all_true";
                },
                0x64 => {
                    return .@"i8x16.bitmask";
                },
                0x65 => {
                    return .@"i8x16.narrow_i16x8_s";
                },
                0x66 => {
                    return .@"i8x16.narrow_i16x8_u";
                },
                0x67 => {
                    return .@"f32x4.ceil";
                },
                0x68 => {
                    return .@"f32x4.floor";
                },
                0x69 => {
                    return .@"f32x4.trunc";
                },
                0x6A => {
                    return .@"f32x4.nearest";
                },
                0x6B => {
                    return .@"i8x16.shl";
                },
                0x6C => {
                    return .@"i8x16.shr_s";
                },
                0x6D => {
                    return .@"i8x16.shr_u";
                },
                0x6E => {
                    return .@"i8x16.add";
                },
                0x6F => {
                    return .@"i8x16.add_sat_s";
                },
                0x70 => {
                    return .@"i8x16.add_sat_u";
                },
                0x71 => {
                    return .@"i8x16.sub";
                },
                0x72 => {
                    return .@"i8x16.sub_sat_s";
                },
                0x73 => {
                    return .@"i8x16.sub_sat_u";
                },
                0x74 => {
                    return .@"f64x2.ceil";
                },
                0x75 => {
                    return .@"f64x2.floor";
                },
                0x76 => {
                    return .@"i8x16.min_s";
                },
                0x77 => {
                    return .@"i8x16.min_u";
                },
                0x78 => {
                    return .@"i8x16.max_s";
                },
                0x79 => {
                    return .@"i8x16.max_u";
                },
                0x7A => {
                    return .@"f64x2.trunc";
                },
                0x7B => {
                    return .@"i8x16.avgr_u";
                },
                0x7C => {
                    return .@"i16x8.extadd_pairwise_i8x16_s";
                },
                0x7D => {
                    return .@"i16x8.extadd_pairwise_i8x16_u";
                },
                0x7E => {
                    return .@"i32x4.extadd_pairwise_i16x8_s";
                },
                0x7F => {
                    return .@"i32x4.extadd_pairwise_i16x8_u";
                },
                0x80 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.abs";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x81 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.neg";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x82 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.q15mulr_sat_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x83 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.all_true";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x84 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.bitmask";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x85 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.narrow_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x86 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.narrow_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x87 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extend_low_i8x16_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x88 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extend_high_i8x16_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x89 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extend_low_i8x16_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x8A => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extend_high_i8x16_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x8B => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.shl";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x8C => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.shr_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x8D => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.shr_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x8E => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.add";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x8F => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.add_sat_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x90 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.add_sat_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x91 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.sub";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x92 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.sub_sat_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x93 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.sub_sat_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x94 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.nearest";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x95 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.mul";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x96 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.min_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x97 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.min_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x98 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.max_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x99 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.max_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x9B => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.avgr_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x9C => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extmul_low_i8x16_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x9D => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extmul_high_i8x16_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x9E => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extmul_low_i8x16_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0x9F => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i16x8.extmul_high_i8x16_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA0 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.abs";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA1 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.neg";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA3 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.all_true";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA4 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.bitmask";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA7 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extend_low_i16x8_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA8 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extend_high_i16x8_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xA9 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extend_low_i16x8_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xAA => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extend_high_i16x8_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xAB => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.shl";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xAC => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.shr_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xAD => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.shr_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xAE => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.add";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xB1 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.sub";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xB5 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.mul";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xB6 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.min_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xB7 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.min_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xB8 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.max_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xB9 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.max_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xBA => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.dot_i16x8_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xBC => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extmul_low_i16x8_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xBD => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extmul_high_i16x8_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xBE => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extmul_low_i16x8_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xBF => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.extmul_high_i16x8_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC0 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.abs";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC1 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.neg";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC3 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.all_true";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC4 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.bitmask";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC7 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extend_low_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC8 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extend_high_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xC9 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extend_low_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xCA => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extend_high_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xCB => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.shl";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xCC => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.shr_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xCD => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.shr_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xCE => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.add";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xD1 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.sub";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xD5 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.mul";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xD6 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.eq";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xD7 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.ne";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xD8 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.lt_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xD9 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.gt_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xDA => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.le_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xDB => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.ge_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xDC => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extmul_low_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xDD => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extmul_high_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xDE => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extmul_low_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xDF => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i64x2.extmul_high_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE0 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.abs";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE1 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.neg";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE3 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.sqrt";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE4 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.add";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE5 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.sub";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE6 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.mul";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE7 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.div";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE8 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.min";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xE9 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.max";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xEA => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.pmin";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xEB => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.pmax";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xEC => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.abs";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xED => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.neg";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xEF => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.sqrt";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF0 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.add";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF1 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.sub";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF2 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.mul";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF3 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.div";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF4 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.min";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF5 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.max";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF6 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.pmin";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF7 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.pmax";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF8 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.trunc_sat_f32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xF9 => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.trunc_sat_f32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xFA => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.convert_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xFB => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f32x4.convert_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xFC => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.trunc_sat_f64x2_s_zero";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xFD => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"i32x4.trunc_sat_f64x2_u_zero";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xFE => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.convert_low_i32x4_s";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                0xFF => {
                    const op2 = try reader.readByte();
                    switch (op2) {
                        0x01 => {
                            return .@"f64x2.convert_low_i32x4_u";
                        },
                        else => {
                            return error.InvalidOpcode;
                        },
                    }
                },
                else => {
                    return error.InvalidOpcode;
                },
            }
        },
        else => {
            return error.InvalidOpcode;
        },
    }
}

pub const Instruction = union(InstructionTag) {
    @"unreachable": EmptyTuple,
    nop: EmptyTuple,
    block: EmptyTuple,
    loop: EmptyTuple,
    @"if": EmptyTuple,
    @"else": EmptyTuple,
    end: EmptyTuple,
    br: EmptyTuple,
    br_if: EmptyTuple,
    br_table: EmptyTuple,
    @"return": EmptyTuple,
    call: EmptyTuple,
    call_indirect: EmptyTuple,
    drop: EmptyTuple,
    select: EmptyTuple,
    select_t: EmptyTuple,
    @"local.get": Tuple(&[_]type{u32}),
    @"local.set": Tuple(&[_]type{u32}),
    @"local.tee": Tuple(&[_]type{u32}),
    @"global.get": Tuple(&[_]type{u32}),
    @"global.set": Tuple(&[_]type{u32}),
    @"table.get": EmptyTuple,
    @"table.set": EmptyTuple,
    @"i32.load": EmptyTuple,
    @"i64.load": EmptyTuple,
    @"f32.load": EmptyTuple,
    @"f64.load": EmptyTuple,
    @"i32.load8_s": EmptyTuple,
    @"i32.load8_u": EmptyTuple,
    @"i32.load16_s": EmptyTuple,
    @"i32.load16_u": EmptyTuple,
    @"i64.load8_s": EmptyTuple,
    @"i64.load8_u": EmptyTuple,
    @"i64.load16_s": EmptyTuple,
    @"i64.load16_u": EmptyTuple,
    @"i64.load32_s": EmptyTuple,
    @"i64.load32_u": EmptyTuple,
    @"i32.store": EmptyTuple,
    @"i64.store": EmptyTuple,
    @"f32.store": EmptyTuple,
    @"f64.store": EmptyTuple,
    @"i32.store8": EmptyTuple,
    @"i32.store16": EmptyTuple,
    @"i64.store8": EmptyTuple,
    @"i64.store16": EmptyTuple,
    @"i64.store32": EmptyTuple,
    @"memory.size": EmptyTuple,
    @"memory.grow": EmptyTuple,
    @"i32.const": EmptyTuple,
    @"i64.const": EmptyTuple,
    @"f32.const": EmptyTuple,
    @"f64.const": EmptyTuple,
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
    @"ref.null": EmptyTuple,
    @"ref.is_null": EmptyTuple,
    @"ref.func": EmptyTuple,
    @"i32.trunc_sat_f32_s": EmptyTuple,
    @"i32.trunc_sat_f32_u": EmptyTuple,
    @"i32.trunc_sat_f64_s": EmptyTuple,
    @"i32.trunc_sat_f64_u": EmptyTuple,
    @"i64.trunc_sat_f32_s": EmptyTuple,
    @"i64.trunc_sat_f32_u": EmptyTuple,
    @"i64.trunc_sat_f64_s": EmptyTuple,
    @"i64.trunc_sat_f64_u": EmptyTuple,
    @"memory.init": EmptyTuple,
    @"data.drop": EmptyTuple,
    @"memory.copy": EmptyTuple,
    @"memory.fill": EmptyTuple,
    @"table.init": EmptyTuple,
    @"elem.drop": EmptyTuple,
    @"table.copy": EmptyTuple,
    @"table.grow": EmptyTuple,
    @"table.size": EmptyTuple,
    @"table.fill": EmptyTuple,
    @"v128.load": EmptyTuple,
    @"v128.load8x8_s": EmptyTuple,
    @"v128.load8x8_u": EmptyTuple,
    @"v128.load16x4_s": EmptyTuple,
    @"v128.load16x4_u": EmptyTuple,
    @"v128.load32x2_s": EmptyTuple,
    @"v128.load32x2_u": EmptyTuple,
    @"v128.load8_splat": EmptyTuple,
    @"v128.load16_splat": EmptyTuple,
    @"v128.load32_splat": EmptyTuple,
    @"v128.load64_splat": EmptyTuple,
    @"v128.store": EmptyTuple,
    @"v128.const": EmptyTuple,
    @"i8x16.shuffle": EmptyTuple,
    @"i8x16.swizzle": EmptyTuple,
    @"i8x16.splat": EmptyTuple,
    @"i16x8.splat": EmptyTuple,
    @"i32x4.splat": EmptyTuple,
    @"i64x2.splat": EmptyTuple,
    @"f32x4.splat": EmptyTuple,
    @"f64x2.splat": EmptyTuple,
    @"i8x16.extract_lane_s": EmptyTuple,
    @"i8x16.extract_lane_u": EmptyTuple,
    @"i8x16.replace_lane": EmptyTuple,
    @"i16x8.extract_lane_s": EmptyTuple,
    @"i16x8.extract_lane_u": EmptyTuple,
    @"i16x8.replace_lane": EmptyTuple,
    @"i32x4.extract_lane": EmptyTuple,
    @"i32x4.replace_lane": EmptyTuple,
    @"i64x2.extract_lane": EmptyTuple,
    @"i64x2.replace_lane": EmptyTuple,
    @"f32x4.extract_lane": EmptyTuple,
    @"f32x4.replace_lane": EmptyTuple,
    @"f64x2.extract_lane": EmptyTuple,
    @"f64x2.replace_lane": EmptyTuple,
    @"i8x16.eq": EmptyTuple,
    @"i8x16.ne": EmptyTuple,
    @"i8x16.lt_s": EmptyTuple,
    @"i8x16.lt_u": EmptyTuple,
    @"i8x16.gt_s": EmptyTuple,
    @"i8x16.gt_u": EmptyTuple,
    @"i8x16.le_s": EmptyTuple,
    @"i8x16.le_u": EmptyTuple,
    @"i8x16.ge_s": EmptyTuple,
    @"i8x16.ge_u": EmptyTuple,
    @"i16x8.eq": EmptyTuple,
    @"i16x8.ne": EmptyTuple,
    @"i16x8.lt_s": EmptyTuple,
    @"i16x8.lt_u": EmptyTuple,
    @"i16x8.gt_s": EmptyTuple,
    @"i16x8.gt_u": EmptyTuple,
    @"i16x8.le_s": EmptyTuple,
    @"i16x8.le_u": EmptyTuple,
    @"i16x8.ge_s": EmptyTuple,
    @"i16x8.ge_u": EmptyTuple,
    @"i32x4.eq": EmptyTuple,
    @"i32x4.ne": EmptyTuple,
    @"i32x4.lt_s": EmptyTuple,
    @"i32x4.lt_u": EmptyTuple,
    @"i32x4.gt_s": EmptyTuple,
    @"i32x4.gt_u": EmptyTuple,
    @"i32x4.le_s": EmptyTuple,
    @"i32x4.le_u": EmptyTuple,
    @"i32x4.ge_s": EmptyTuple,
    @"i32x4.ge_u": EmptyTuple,
    @"f32x4.eq": EmptyTuple,
    @"f32x4.ne": EmptyTuple,
    @"f32x4.lt": EmptyTuple,
    @"f32x4.gt": EmptyTuple,
    @"f32x4.le": EmptyTuple,
    @"f32x4.ge": EmptyTuple,
    @"f64x2.eq": EmptyTuple,
    @"f64x2.ne": EmptyTuple,
    @"f64x2.lt": EmptyTuple,
    @"f64x2.gt": EmptyTuple,
    @"f64x2.le": EmptyTuple,
    @"f64x2.ge": EmptyTuple,
    @"v128.not": EmptyTuple,
    @"v128.and": EmptyTuple,
    @"v128.andnot": EmptyTuple,
    @"v128.or": EmptyTuple,
    @"v128.xor": EmptyTuple,
    @"v128.bitselect": EmptyTuple,
    @"v128.any_true": EmptyTuple,
    @"v128.load8_lane": EmptyTuple,
    @"v128.load16_lane": EmptyTuple,
    @"v128.load32_lane": EmptyTuple,
    @"v128.load64_lane": EmptyTuple,
    @"v128.store8_lane": EmptyTuple,
    @"v128.store16_lane": EmptyTuple,
    @"v128.store32_lane": EmptyTuple,
    @"v128.store64_lane": EmptyTuple,
    @"v128.load32_zero": EmptyTuple,
    @"v128.load64_zero": EmptyTuple,
    @"f32x4.demote_f64x2_zero": EmptyTuple,
    @"f64x2.promote_low_f32x4": EmptyTuple,
    @"i8x16.abs": EmptyTuple,
    @"i8x16.neg": EmptyTuple,
    @"i8x16.popcnt": EmptyTuple,
    @"i8x16.all_true": EmptyTuple,
    @"i8x16.bitmask": EmptyTuple,
    @"i8x16.narrow_i16x8_s": EmptyTuple,
    @"i8x16.narrow_i16x8_u": EmptyTuple,
    @"f32x4.ceil": EmptyTuple,
    @"f32x4.floor": EmptyTuple,
    @"f32x4.trunc": EmptyTuple,
    @"f32x4.nearest": EmptyTuple,
    @"i8x16.shl": EmptyTuple,
    @"i8x16.shr_s": EmptyTuple,
    @"i8x16.shr_u": EmptyTuple,
    @"i8x16.add": EmptyTuple,
    @"i8x16.add_sat_s": EmptyTuple,
    @"i8x16.add_sat_u": EmptyTuple,
    @"i8x16.sub": EmptyTuple,
    @"i8x16.sub_sat_s": EmptyTuple,
    @"i8x16.sub_sat_u": EmptyTuple,
    @"f64x2.ceil": EmptyTuple,
    @"f64x2.floor": EmptyTuple,
    @"i8x16.min_s": EmptyTuple,
    @"i8x16.min_u": EmptyTuple,
    @"i8x16.max_s": EmptyTuple,
    @"i8x16.max_u": EmptyTuple,
    @"f64x2.trunc": EmptyTuple,
    @"i8x16.avgr_u": EmptyTuple,
    @"i16x8.extadd_pairwise_i8x16_s": EmptyTuple,
    @"i16x8.extadd_pairwise_i8x16_u": EmptyTuple,
    @"i32x4.extadd_pairwise_i16x8_s": EmptyTuple,
    @"i32x4.extadd_pairwise_i16x8_u": EmptyTuple,
    @"i16x8.abs": EmptyTuple,
    @"i16x8.neg": EmptyTuple,
    @"i16x8.q15mulr_sat_s": EmptyTuple,
    @"i16x8.all_true": EmptyTuple,
    @"i16x8.bitmask": EmptyTuple,
    @"i16x8.narrow_i32x4_s": EmptyTuple,
    @"i16x8.narrow_i32x4_u": EmptyTuple,
    @"i16x8.extend_low_i8x16_s": EmptyTuple,
    @"i16x8.extend_high_i8x16_s": EmptyTuple,
    @"i16x8.extend_low_i8x16_u": EmptyTuple,
    @"i16x8.extend_high_i8x16_u": EmptyTuple,
    @"i16x8.shl": EmptyTuple,
    @"i16x8.shr_s": EmptyTuple,
    @"i16x8.shr_u": EmptyTuple,
    @"i16x8.add": EmptyTuple,
    @"i16x8.add_sat_s": EmptyTuple,
    @"i16x8.add_sat_u": EmptyTuple,
    @"i16x8.sub": EmptyTuple,
    @"i16x8.sub_sat_s": EmptyTuple,
    @"i16x8.sub_sat_u": EmptyTuple,
    @"f64x2.nearest": EmptyTuple,
    @"i16x8.mul": EmptyTuple,
    @"i16x8.min_s": EmptyTuple,
    @"i16x8.min_u": EmptyTuple,
    @"i16x8.max_s": EmptyTuple,
    @"i16x8.max_u": EmptyTuple,
    @"i16x8.avgr_u": EmptyTuple,
    @"i16x8.extmul_low_i8x16_s": EmptyTuple,
    @"i16x8.extmul_high_i8x16_s": EmptyTuple,
    @"i16x8.extmul_low_i8x16_u": EmptyTuple,
    @"i16x8.extmul_high_i8x16_u": EmptyTuple,
    @"i32x4.abs": EmptyTuple,
    @"i32x4.neg": EmptyTuple,
    @"i32x4.all_true": EmptyTuple,
    @"i32x4.bitmask": EmptyTuple,
    @"i32x4.extend_low_i16x8_s": EmptyTuple,
    @"i32x4.extend_high_i16x8_s": EmptyTuple,
    @"i32x4.extend_low_i16x8_u": EmptyTuple,
    @"i32x4.extend_high_i16x8_u": EmptyTuple,
    @"i32x4.shl": EmptyTuple,
    @"i32x4.shr_s": EmptyTuple,
    @"i32x4.shr_u": EmptyTuple,
    @"i32x4.add": EmptyTuple,
    @"i32x4.sub": EmptyTuple,
    @"i32x4.mul": EmptyTuple,
    @"i32x4.min_s": EmptyTuple,
    @"i32x4.min_u": EmptyTuple,
    @"i32x4.max_s": EmptyTuple,
    @"i32x4.max_u": EmptyTuple,
    @"i32x4.dot_i16x8_s": EmptyTuple,
    @"i32x4.extmul_low_i16x8_s": EmptyTuple,
    @"i32x4.extmul_high_i16x8_s": EmptyTuple,
    @"i32x4.extmul_low_i16x8_u": EmptyTuple,
    @"i32x4.extmul_high_i16x8_u": EmptyTuple,
    @"i64x2.abs": EmptyTuple,
    @"i64x2.neg": EmptyTuple,
    @"i64x2.all_true": EmptyTuple,
    @"i64x2.bitmask": EmptyTuple,
    @"i64x2.extend_low_i32x4_s": EmptyTuple,
    @"i64x2.extend_high_i32x4_s": EmptyTuple,
    @"i64x2.extend_low_i32x4_u": EmptyTuple,
    @"i64x2.extend_high_i32x4_u": EmptyTuple,
    @"i64x2.shl": EmptyTuple,
    @"i64x2.shr_s": EmptyTuple,
    @"i64x2.shr_u": EmptyTuple,
    @"i64x2.add": EmptyTuple,
    @"i64x2.sub": EmptyTuple,
    @"i64x2.mul": EmptyTuple,
    @"i64x2.eq": EmptyTuple,
    @"i64x2.ne": EmptyTuple,
    @"i64x2.lt_s": EmptyTuple,
    @"i64x2.gt_s": EmptyTuple,
    @"i64x2.le_s": EmptyTuple,
    @"i64x2.ge_s": EmptyTuple,
    @"i64x2.extmul_low_i32x4_s": EmptyTuple,
    @"i64x2.extmul_high_i32x4_s": EmptyTuple,
    @"i64x2.extmul_low_i32x4_u": EmptyTuple,
    @"i64x2.extmul_high_i32x4_u": EmptyTuple,
    @"f32x4.abs": EmptyTuple,
    @"f32x4.neg": EmptyTuple,
    @"f32x4.sqrt": EmptyTuple,
    @"f32x4.add": EmptyTuple,
    @"f32x4.sub": EmptyTuple,
    @"f32x4.mul": EmptyTuple,
    @"f32x4.div": EmptyTuple,
    @"f32x4.min": EmptyTuple,
    @"f32x4.max": EmptyTuple,
    @"f32x4.pmin": EmptyTuple,
    @"f32x4.pmax": EmptyTuple,
    @"f64x2.abs": EmptyTuple,
    @"f64x2.neg": EmptyTuple,
    @"f64x2.sqrt": EmptyTuple,
    @"f64x2.add": EmptyTuple,
    @"f64x2.sub": EmptyTuple,
    @"f64x2.mul": EmptyTuple,
    @"f64x2.div": EmptyTuple,
    @"f64x2.min": EmptyTuple,
    @"f64x2.max": EmptyTuple,
    @"f64x2.pmin": EmptyTuple,
    @"f64x2.pmax": EmptyTuple,
    @"i32x4.trunc_sat_f32x4_s": EmptyTuple,
    @"i32x4.trunc_sat_f32x4_u": EmptyTuple,
    @"f32x4.convert_i32x4_s": EmptyTuple,
    @"f32x4.convert_i32x4_u": EmptyTuple,
    @"i32x4.trunc_sat_f64x2_s_zero": EmptyTuple,
    @"i32x4.trunc_sat_f64x2_u_zero": EmptyTuple,
    @"f64x2.convert_low_i32x4_s": EmptyTuple,
    @"f64x2.convert_low_i32x4_u": EmptyTuple,
};
