const std = @import("std");

const readInstructionTag = @import("./read_instruction_tag.zig").readInstructionTag;
const InstructionTag = @import("./instruction_tag.zig").InstructionTag;

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
    type: ValueType,
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
            .type = valueTypeFromValueTypeCode(t),
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

fn valueTypeFromValueTypeCode(v: ValueTypeCode) ValueType {
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

fn readResultType(allocator: std.mem.Allocator, reader: Reader) ![]ValueType {
    const count = try std.leb.readULEB128(u32, reader);

    const result_type = try allocator.alloc(ValueType, count);

    for (result_type) |*value_type| {
        value_type.* = valueTypeFromValueTypeCode(try readEnum(ValueTypeCode, reader));
    }

    return result_type;
}

const ExportDescritionType = enum(u8) {
    function = 0x00,
    table = 0x01,
    memory = 0x02,
    global = 0x03,
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

const ValueTypeCode = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,

    f32 = 0x7D,
    f64 = 0x7C,

    v128 = 0x7B,

    function_reference = 0x70,
    extern_reference = 0x6F,
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

pub const Instruction = union(InstructionTag) {
    @"unreachable": EmptyTuple,
    nop: EmptyTuple,
    block: EmptyTuple,
    loop: EmptyTuple,
    @"if": EmptyTuple,
    @"else": EmptyTuple,
    end: EmptyTuple,
    br: Tuple(&[_]type{u32}),
    br_if: Tuple(&[_]type{u32}),
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
