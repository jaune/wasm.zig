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

const ExpressionBuilder = @import("./expression_builder.zig").ExpressionBuilder;

fn readExpression(allocator: std.mem.Allocator, reader: FileReader, function_type_section: *const FunctionTypeSection) !Expression {
    var builder: ExpressionBuilder = ExpressionBuilder.init(allocator);
    defer builder.deinit();

    while (!builder.is_expression_completed) {
        const opcode = try readOpcode(reader);

        const maybe_simple_tag = instructionTagFromOpcode(opcode);

        if (maybe_simple_tag) |tag| {
            try builder.appendTag(tag);
        } else {
            switch (opcode) {
                .end => {
                    try builder.appendEnd();
                },

                .@"else" => {
                    try builder.appendElse();
                },

                .@"if" => {
                    const bt = try readBlockType(reader);

                    try builder.appendIf(bt.toFunctionTypeIndex(function_type_section));
                },

                .block, .loop => |op| {
                    const bt = try readBlockType(reader);

                    try builder.appendBlock(
                        switch (op) {
                            .block => .block,
                            .loop => .loop,
                            else => {
                                return error.Unreachable;
                            },
                        },
                        bt.toFunctionTypeIndex(function_type_section),
                    );
                },

                .@"i32.const", .@"i64.const", .@"f32.const", .@"f64.const" => |op| {
                    const value = switch (op) {
                        .@"i32.const" => try readValue(i32, reader),
                        .@"i64.const" => try readValue(i64, reader),
                        .@"f32.const" => try readValue(f32, reader),
                        .@"f64.const" => try readValue(f64, reader),
                        else => return error.Unreachable,
                    };
                    try builder.appendConst(value);
                },

                .@"v128.const" => {
                    return error.UnhandledOpcode;
                },

                .call => {
                    try builder.appendCall(.{
                        .function_index = try std.leb.readULEB128(u32, reader),
                    });
                },

                .call_indirect => {
                    try builder.appendCallIndirect(.{
                        .function_type_index = try std.leb.readULEB128(u32, reader),
                        .table_index = try std.leb.readULEB128(u32, reader),
                    });
                },

                .@"i32.load", .@"i64.load", .@"f32.load", .@"f64.load", .@"i32.load8_s", .@"i32.load8_u", .@"i32.load16_s", .@"i32.load16_u", .@"i64.load8_s", .@"i64.load8_u", .@"i64.load16_s", .@"i64.load16_u", .@"i64.load32_s", .@"i64.load32_u", .@"i32.store", .@"i64.store", .@"f32.store", .@"f64.store", .@"i32.store8", .@"i32.store16", .@"i64.store8", .@"i64.store16", .@"i64.store32" => |op| {
                    const tag: InstructionTag = switch (op) {
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
                    };
                    try builder.appendMemoryAccessor(tag, .{
                        .@"align" = try std.leb.readULEB128(u32, reader),
                        .offset = try std.leb.readULEB128(u32, reader),
                    });
                },

                .br_table => {
                    try builder.appendBranchTable(try readBranchTablePaylaod(allocator, reader));
                },

                .br, .br_if => |op| {
                    const label_index = try std.leb.readULEB128(LabelIndex, reader);

                    try builder.appendBranch(switch (op) {
                        .br => .branch,
                        .br_if => .branch_if,
                        else => {
                            return error.Unreachable;
                        },
                    }, label_index);
                },

                .@"local.get", .@"local.set", .@"local.tee" => |op| {
                    const local_index = try std.leb.readULEB128(u32, reader);

                    try builder.appendLocalAccessor(switch (op) {
                        .@"local.get" => .@"local.get",
                        .@"local.set" => .@"local.set",
                        .@"local.tee" => .@"local.tee",
                        else => {
                            return error.Unreachable;
                        },
                    }, local_index);
                },

                .@"global.get", .@"global.set" => |op| {
                    const global_index = try std.leb.readULEB128(u32, reader);

                    try builder.appendGlobalAccessor(switch (op) {
                        .@"global.get" => .@"global.get",
                        .@"global.set" => .@"global.set",
                        else => {
                            return error.Unreachable;
                        },
                    }, global_index);
                },

                .@"ref.null" => {
                    const t = try readReferenceType(reader);

                    try builder.appendTag(switch (t) {
                        .function => .@"ref.null function",
                        .@"extern" => .@"ref.null extern",
                    });
                },

                else => {
                    std.log.err("UnhandledOpcode: opcode={}", .{opcode});
                    return error.UnhandledOpcode;
                },
            }
        }
    }

    return try builder.build(allocator);
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

const TableIndex = @import("./module.zig").TableIndex;
const FunctionIndex = @import("./module.zig").FunctionIndex;
const MemoryIndex = @import("./module.zig").MemoryIndex;
const GlobalIndex = @import("./module.zig").GlobalIndex;

fn readExportSection(allocator: std.mem.Allocator, reader: Reader) ![]Export {
    const count = try std.leb.readULEB128(u32, reader);

    const entries = try allocator.alloc(Export, count);

    for (entries) |*entry| {
        const name = try readName(allocator, reader);
        const descrition = try readEnum(ExportDescritionTypeCode, reader);
        const index = try std.leb.readULEB128(u32, reader);

        entry.* = .{
            .name = name,
            .index = switch (descrition) {
                .function => .{
                    .function = std.math.cast(FunctionIndex, index) orelse {
                        return error.CastingError;
                    },
                },
                .table => .{
                    .table = std.math.cast(TableIndex, index) orelse {
                        return error.CastingError;
                    },
                },
                .memory => .{
                    .memory = std.math.cast(MemoryIndex, index) orelse {
                        return error.CastingError;
                    },
                },
                .global => .{
                    .global = std.math.cast(GlobalIndex, index) orelse {
                        return error.CastingError;
                    },
                },
            },
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

    inline for (std.meta.fields(ValueType), 1..) |f, i| {
        function_types[count + i] = FunctionType{
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
    var tables: []Table = &.{};
    var memory_limits: []Limits = &.{};
    var element_segments: []ElementSegment = &.{};
    var global_definitions: []GlobalDefinition = &.{};

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

        const secton_id = try enumFromInt(SectionId, secton_id_raw);
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
                tables = try readTableSection(area_allocator, reader);
            },
            .memory_section => {
                memory_limits = try readMemorySection(area_allocator, reader);
            },
            .global_section => {
                const function_type_section = maybe_function_type_section orelse {
                    return error.NoTypeSection;
                };
                global_definitions = try readGlobalSection(area_allocator, reader, &function_type_section);
            },
            .element_section => {
                const function_type_section = maybe_function_type_section orelse {
                    return error.NoTypeSection;
                };
                element_segments = try readElementSection(area_allocator, reader, &function_type_section);
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
        .tables = tables,
        .element_segments = element_segments,
        .memory_limits = memory_limits,
        .global_definitions = global_definitions,
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
            const min = try std.leb.readULEB128(u32, reader);
            const max = try std.leb.readULEB128(u32, reader);

            if (max < min) {
                return error.MalformedLimits;
            }

            return .{
                .min = min,
                .max = max,
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
            .type = try readReferenceType(reader),
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

const ElementSegment = @import("./module.zig").ElementSegment;

fn createReferenceFunctionExpression(allocator: std.mem.Allocator, fn_idx: FunctionIndex) !Expression {
    var builder: ExpressionBuilder = ExpressionBuilder.init(allocator);
    defer builder.deinit();

    try builder.appendReferenceFunction(fn_idx);
    try builder.appendEnd();

    return try builder.build(allocator);
}

fn readElementSection(allocator: std.mem.Allocator, reader: Reader, section_type_section: *const FunctionTypeSection) ![]ElementSegment {
    const count = try std.leb.readULEB128(u32, reader);

    const segments = try allocator.alloc(ElementSegment, count);

    for (segments) |*seg| {
        const selector = try std.leb.readULEB128(u32, reader);

        switch (selector) {
            0 => {
                const offset = try readExpression(allocator, reader, section_type_section);

                const fn_count = try std.leb.readULEB128(u32, reader);
                const init = try allocator.alloc(Expression, fn_count);

                for (init) |*e| {
                    const fn_idx = try std.leb.readULEB128(u32, reader);

                    e.* = try createReferenceFunctionExpression(allocator, fn_idx);
                }

                seg.* = .{
                    .type = .function,
                    .init = init,
                    .mode = .{
                        .active = .{
                            .table = 0,
                            .offset = offset,
                        },
                    },
                };
            },
            else => |e| {
                std.log.err("MalformedElementSegment: {}", .{e});
                return error.MalformedElementSegment;
            },
        }
    }

    return segments;
}
