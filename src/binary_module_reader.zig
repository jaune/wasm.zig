const std = @import("std");

const instructionTagFromOpcode = @import("./instruction_tag_from_opcode.zig").instructionTagFromOpcode;
const readOpcode = @import("./read_opcode.zig").readOpcode;
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
const Expression = @import("./module.zig").Expression;
const InstructionArguments = @import("./module.zig").InstructionArguments;
const ArgumentsTypeOfInstruction = @import("./module.zig").ArgumentsTypeOfInstruction;
const InstructionIndex = @import("./module.zig").InstructionIndex;

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

fn allocResultTypeFromValueType(allocator: std.mem.Allocator, vt: ValueType) ![]ValueType {
    const result_type = try allocator.alloc(ValueType, 1);

    result_type[0] = vt;

    return result_type;
}

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
    value_type: ValueType,

    fn toFunctionTypeIndex(self: BlockType, section: *const FunctionTypeSection) u32 {
        return switch (self) {
            .value_type => |vt| section.empty_type_index + 1 + @intFromEnum(vt),
            .empty => section.empty_type_index,
        };
    }
};

fn readBlockType(reader: ExpressionReader) !BlockType {
    const b = try reader.readByte();

    if (b == 0x40) {
        return .empty;
    }

    const e = enumFromInt(ValueTypeCode, b) catch {
        return error.UnsupportedBlockType;
    };

    return BlockType{ .value_type = e.toValueType() };
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

const LabelKind = enum {
    loop,
    block,
    @"if",
};

const LabelStackEntry = struct {
    kind: LabelKind,
};

fn readExpression(allocator: std.mem.Allocator, reader: ExpressionReader, function_type_section: *const FunctionTypeSection) !Expression {
    var instructions = std.ArrayList(Instruction).init(allocator);
    var instruction_arguments = std.ArrayList(InstructionArguments).init(allocator);
    var label_stack = try std.BoundedArray(LabelStackEntry, 1024).init(0);

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
                    const entry = label_stack.pop();

                    switch (entry.kind) {
                        .block => {
                            try instructions.append(.{ .tag = .block_end });
                        },
                        .loop => {
                            try instructions.append(.{ .tag = .loop_end });
                        },
                        .@"if" => {
                            try instructions.append(.{ .tag = .if_end });
                        },
                    }
                },
                .block => {
                    const bt = try readBlockType(reader);

                    const arguments_index: u32 = std.math.cast(u32, instruction_arguments.items.len) orelse {
                        return error.TooManyInstructionArguments;
                    };

                    const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, instructions.items.len) orelse {
                        return error.TooManyInstruction;
                    };

                    try instruction_arguments.append(.{
                        .block = .{
                            .start = instruction_index,
                            .end = instruction_index,
                            .function_type_index = bt.toFunctionTypeIndex(function_type_section),
                        },
                    });

                    try instructions.append(.{
                        .tag = .block,
                        .arguments_index = arguments_index,
                    });

                    try label_stack.append(.{
                        .kind = .block,
                    });
                },
                .loop => {
                    const bt = try readBlockType(reader);

                    const arguments_index: u32 = std.math.cast(u32, instruction_arguments.items.len) orelse {
                        return error.TooManyInstructionArguments;
                    };
                    const instruction_index: InstructionIndex = std.math.cast(InstructionIndex, instructions.items.len) orelse {
                        return error.TooManyInstruction;
                    };

                    try instruction_arguments.append(.{
                        .loop = .{
                            .start = instruction_index,
                            .end = instruction_index,
                            .function_type_index = bt.toFunctionTypeIndex(function_type_section),
                        },
                    });

                    try instructions.append(.{
                        .tag = .loop,
                        .arguments_index = arguments_index,
                    });

                    try label_stack.append(.{
                        .kind = .loop,
                    });
                },

                .@"if" => {
                    const bt = try readBlockType(reader);

                    switch (bt) {
                        .value_type => |vt| {
                            std.log.debug("if bt={}", .{vt});
                        },
                        .empty => {
                            std.log.debug("if bt=<empty>", .{});
                        },
                    }

                    try label_stack.append(.{
                        .kind = .@"if",
                    });

                    try instructions.append(.{ .tag = .@"if" });
                },

                .@"else" => {
                    try instructions.append(.{ .tag = .@"else" });
                },

                .br_table => {
                    _ = try readBranchTable(allocator, reader);

                    try instructions.append(.{ .tag = .br_table });
                },

                // tuple
                inline .br, .br_if, .@"local.get", .@"local.set", .@"local.tee", .@"global.get", .@"global.set", .@"i32.const", .@"i64.const", .@"f32.const", .@"f64.const" => |code| {
                    const tag_name = comptime @tagName(code);
                    @setEvalBranchQuota(10_000);
                    const tag = comptime std.meta.stringToEnum(InstructionTag, tag_name) orelse {
                        @compileError("Missing InstructionTag: " ++ tag_name);
                    };
                    @setEvalBranchQuota(1000);
                    const arguments_index: u32 = std.math.cast(u32, instruction_arguments.items.len) orelse {
                        return error.TooManyInstructionArguments;
                    };

                    const TupleType: type = comptime ArgumentsTypeOfInstruction(tag);
                    const tuple: TupleType = try readTuple(TupleType, reader);

                    const arguments = @unionInit(InstructionArguments, tag_name, tuple);

                    try instruction_arguments.append(arguments);
                    try instructions.append(.{
                        .tag = tag,
                        .arguments_index = arguments_index,
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

    for (codes) |*code| {
        const code_size = try std.leb.readULEB128(u32, reader);

        const position_before_locals = try reader.context.getPos();

        const locals = try readLocals(allocator, reader);
        const locals_size = try reader.context.getPos() - position_before_locals;

        const expression_size = code_size - locals_size;
        var expression_reader = std.io.limitedReader(reader, expression_size);

        code.* = .{
            .locals = locals,
            .expression = try readExpression(allocator, expression_reader.reader(), function_type_section),
        };

        if (expression_reader.bytes_left != 0) {
            std.log.err("ExpressionReaderHasBytesLeft: bytes_left={}", .{expression_reader.bytes_left});

            var buf: [32]u8 = undefined;

            if (expression_reader.bytes_left < buf.len) {
                const r = try reader.read(buf[0..expression_reader.bytes_left]);

                for (buf[0..r]) |b| {
                    std.log.err("  0x{X:0>2}", .{b});
                }
            }

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
            else => {
                std.log.info("Section {}: skiped", .{secton_id});
                try reader.skipBytes(section_size, .{});
            },
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
