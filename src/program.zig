const std = @import("std");

const ReferenceType = @import("./module.zig").ReferenceType;
const Limits = @import("./module.zig").Limits;
const Module = @import("./module.zig").Module;
const FunctionIndex = @import("./module.zig").FunctionIndex;
const FunctionTypeIndex = @import("./module.zig").FunctionTypeIndex;
const Expression = @import("./module.zig").Expression;
const FunctionReference = @import("./module.zig").FunctionReference;
const FunctionBody = @import("./module.zig").FunctionBody;
const ModuleInstanceIndex = @import("./module.zig").ModuleInstanceIndex;
const AnyReference = @import("./module.zig").AnyReference;
const MemoryInstanceIndex = @import("./module.zig").MemoryInstanceIndex;
const MemoryPageIndex = @import("./module.zig").MemoryPageIndex;
const Value = @import("./module.zig").Value;
const GlobalIndex = @import("./module.zig").GlobalIndex;

const max_module_instances = @import("./module.zig").max_module_instances;
const max_memory_pages = @import("./module.zig").max_memory_pages;

const logRuntime = @import("./runtime.zig").logRuntime;

const ExpressionBuilder = @import("./expression_builder.zig").ExpressionBuilder;
const FunctionType = @import("./module.zig").FunctionType;

const Runtime = @import("./runtime.zig").Runtime;

const executeExpression = @import("./runtime.zig").executeExpression;

const MemoryAddress = u32;

const TableInstance = struct {
    type: ReferenceType,
    limits: Limits,
    elements: []AnyReference,

    const Self = @This();

    pub fn getAnyElement(self: *const Self, i: usize) !AnyReference {
        if (i >= self.elements.len) {
            return error.UndefinedElement;
        }

        return self.elements[i];
    }

    pub fn setAnyElement(self: *const Self, i: usize, value: AnyReference) !void {
        if (i >= self.elements.len) {
            return error.OutOfBounds;
        }

        self.elements[i] = value;
    }
};

const memory_page_size = 64 * 1024;

const MemoryInstance = struct {
    limits: Limits,
    data: []u8,
};

const ModuleInstance = struct {
    module: *const Module,

    tables: []TableInstance,
    memories: []MemoryInstance,
    globals: []Value,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, module: *const Module) !Self {
        const tables = try allocator.alloc(TableInstance, module.tables.len);
        errdefer allocator.free(tables);

        for (module.tables, tables) |t, *ti| {
            const max = t.limits.max orelse {
                return error.UnsupportedTableWithoutMax;
            };
            if (max > 100) {
                return error.TableTooBig;
            }
            if (max == 0) {
                return error.TableEmpty;
            }

            ti.* = .{
                .type = t.type,
                .limits = t.limits,
                .elements = try allocator.alloc(AnyReference, max),
            };
        }

        const memory_instances = try allocator.alloc(MemoryInstance, module.memory_limits.len);
        errdefer allocator.free(memory_instances);

        for (memory_instances, module.memory_limits) |*m, limits| {
            m.* = .{
                .limits = limits,
                .data = try allocator.alloc(u8, limits.min * memory_page_size),
            };
        }

        const globals = try allocator.alloc(Value, module.global_definitions.len);
        errdefer allocator.free(globals);

        return .{
            .module = module,
            .tables = tables,
            .memories = memory_instances,
            .globals = globals,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.tables) |i| {
            allocator.free(i.elements);
        }
        allocator.free(self.tables);

        for (self.memories) |m| {
            allocator.free(m.data);
        }
        allocator.free(self.memories);

        allocator.free(self.globals);

        self.tables = &.{};
    }

    pub fn getAnyElement(self: *const Self, table_index: usize, element_index: usize) !AnyReference {
        if (table_index >= self.tables.len) {
            return error.OutOfBounds;
        }

        return self.tables[table_index].getAnyElement(element_index);
    }

    pub fn setAnyElement(self: *const Self, table_index: usize, element_index: usize, value: AnyReference) !void {
        if (table_index >= self.tables.len) {
            return error.OutOfBounds;
        }

        try self.tables[table_index].setAnyElement(element_index, value);
    }
};

pub const ModuleInstanceBoundedArray = std.BoundedArray(ModuleInstance, max_module_instances);

const FunctionInstance = struct {
    function_index: FunctionIndex,
    module_instance_index: ModuleInstanceIndex,
};

pub const Program = struct {
    allocator: std.mem.Allocator,

    module_instances: ModuleInstanceBoundedArray,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .module_instances = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.module_instances.slice()) |*i| {
            i.deinit(self.allocator);
        }
        self.module_instances.len = 0;
    }

    pub fn functionTypeFromIndex(self: *Self, module_instance_index: ModuleInstanceIndex, function_type_index: FunctionTypeIndex) !FunctionType {
        return self.module_instances.buffer[module_instance_index].module.function_types[function_type_index];
    }

    pub fn @"global.set"(self: *Self, module_instance_index: ModuleInstanceIndex, global_index: GlobalIndex, value: Value) !void {
        self.module_instances.buffer[module_instance_index].globals[global_index] = value;
    }

    pub fn @"global.get"(self: *Self, module_instance_index: ModuleInstanceIndex, global_index: GlobalIndex) !Value {
        return self.module_instances.buffer[module_instance_index].globals[global_index];
    }

    pub fn @"memory.size"(self: *Self, module_instance_index: ModuleInstanceIndex, memory_instance_index: MemoryInstanceIndex) !MemoryPageIndex {
        const data = self.module_instances.buffer[module_instance_index].memories[memory_instance_index].data;

        return std.math.cast(MemoryPageIndex, data.len / memory_page_size) orelse {
            return error.CastingError;
        };
    }

    pub fn @"memory.grow"(self: *Self, module_instance_index: ModuleInstanceIndex, memory_instance_index: MemoryInstanceIndex, n_page: MemoryPageIndex) !MemoryPageIndex {
        const old_data = self.module_instances.buffer[module_instance_index].memories[memory_instance_index].data;

        const old_size = std.math.cast(MemoryPageIndex, old_data.len / memory_page_size) orelse {
            return error.CastingError;
        };
        const new_size = old_data.len + (@as(usize, @intCast(n_page)) * memory_page_size);

        const new_data = try self.allocator.realloc(old_data, new_size);

        self.module_instances.buffer[module_instance_index].memories[memory_instance_index].data = new_data;

        return old_size;
    }

    pub fn @"i.store"(self: *Self, comptime T: type, module_instance_index: ModuleInstanceIndex, memory_instance_index: MemoryInstanceIndex, addr: u32, value: T) !void {
        const data = self.module_instances.buffer[module_instance_index].memories[memory_instance_index].data;

        switch (@typeInfo(T)) {
            .Int => {
                std.mem.writePackedIntNative(T, data, addr * 8, value);
            },
            .Float => |F| {
                const U = std.meta.Int(.unsigned, F.bits);

                std.mem.writePackedIntNative(U, data, addr * 8, @bitCast(value));
            },
            else => @compileError("Unsupported type"),
        }
    }

    pub fn @"i.load"(self: *Self, comptime T: type, module_instance_index: ModuleInstanceIndex, memory_instance_index: MemoryInstanceIndex, addr: MemoryAddress) !T {
        const data = self.module_instances.buffer[module_instance_index].memories[memory_instance_index].data;

        switch (@typeInfo(T)) {
            .Int => {
                return std.mem.readPackedIntNative(T, data, addr * 8);
            },
            .Float => |F| {
                const U = std.meta.Int(.unsigned, F.bits);

                return @bitCast(std.mem.readPackedIntNative(U, data, addr * 8));
            },
            else => @compileError("Unsupported type"),
        }
    }

    pub fn instantiateModule(self: *Self, module: *const Module) !ModuleInstanceIndex {
        const i = try ModuleInstance.init(self.allocator, module);

        const idx = self.module_instances.len;
        try self.module_instances.append(i);

        for (module.element_segments) |seg| {
            switch (seg.mode) {
                .active => |m| {
                    for (seg.init, 0..) |e, el_i| {
                        var rt = Runtime.init(self);

                        try executeExpression(&rt, idx, &m.offset);
                        try executeExpression(&rt, idx, &e);

                        const ref = try rt.popAnyReferenceValue();

                        if (std.meta.activeTag(ref) != seg.type) {
                            return error.WrongSegmentType;
                        }

                        try i.setAnyElement(m.table, el_i, ref);
                    }
                },
                else => {
                    return error.UnsupportedElementSegment;
                },
            }
        }

        return idx;
    }
};

// fn createTableInitExpression(allocator: std.mem.Allocator, fn_idx: FunctionIndex) !Expression {
//     var builder: ExpressionBuilder = ExpressionBuilder.init(allocator);
//     defer builder.deinit();

//     try builder.appendReferenceFunction(fn_idx);
//     try builder.appendEnd();

//     return try builder.build(allocator);
// }
