const std = @import("std");

const ReferenceType = @import("./module.zig").ReferenceType;
const Limits = @import("./module.zig").Limits;
const Module = @import("./module.zig").Module;
const FunctionIndex = @import("./module.zig").FunctionIndex;
const FunctionTypeIndex = @import("./module.zig").FunctionTypeIndex;
const Expression = @import("./module.zig").Expression;
const ExpressionBuilder = @import("./expression_builder.zig").ExpressionBuilder;
const FunctionType = @import("./expression_builder.zig").ExpressionBuilder;

const logExpression = @import("./module.zig").logExpression;
const logRuntime = @import("./runtime.zig").logRuntime;

const Runtime = @import("./runtime.zig").Runtime;
const executeExpression = @import("./runtime.zig").executeExpression;

const TableInstance = struct {
    type: ReferenceType,
    limits: Limits,
    elements: []u32,

    const Self = @This();

    pub fn getElement(self: *const Self, i: usize) !u32 {
        if (i >= self.elements.len) {
            return error.OutOfBounds;
        }

        return self.elements[i];
    }

    pub fn setElement(self: *const Self, i: usize, value: u32) !void {
        if (i >= self.elements.len) {
            return error.OutOfBounds;
        }

        self.elements[i] = value;
    }
};

const ModuleInstance = struct {
    module: *const Module,

    tables: []TableInstance,

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
                .elements = try allocator.alloc(u32, max),
            };
        }

        return .{
            .module = module,
            .tables = tables,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.tables) |i| {
            allocator.free(i.elements);
        }
        allocator.free(self.tables);
        self.tables = &.{};
    }

    pub fn getFunctionIndexFromTable(self: *const Self, table_index: usize, element_index: usize, function_type_index: FunctionTypeIndex) !FunctionIndex {
        const function_index = try self.getElement(.function, table_index, element_index);

        std.log.debug("function_index= {x}", .{function_index});

        const fti = try self.module.getFunctionTypeIndex(function_index);

        if (fti != function_type_index) {
            return error.InvalidFunctionTypeIndex;
        }

        return function_index;
    }

    pub fn getElement(self: *const Self, rt: ReferenceType, table_index: usize, element_index: usize) !u32 {
        if (table_index >= self.tables.len) {
            return error.OutOfBounds;
        }

        const t = &self.tables[table_index];

        if (t.type != rt) {
            return error.WrongTableType;
        }

        return t.getElement(element_index);
    }

    pub fn setElement(self: *const Self, rt: ReferenceType, table_index: usize, element_index: usize, value: u32) !void {
        if (table_index >= self.tables.len) {
            return error.OutOfBounds;
        }

        const t = &self.tables[table_index];

        if (t.type != rt) {
            return error.WrongTableType;
        }

        try self.tables[table_index].setElement(element_index, value);
    }
};

const max_module_instances_capacity = 10;
pub const ModuleInstanceIndex: type = std.math.IntFittingRange(0, max_module_instances_capacity - 1);
pub const ModuleInstanceBoundedArray = std.BoundedArray(ModuleInstance, max_module_instances_capacity);

const FunctionReference = @import("./module.zig").FunctionReference;
const FunctionBody = @import("./module.zig").FunctionBody;

const FunctionInstance = struct {
    function_index: FunctionIndex,
    module_instance_index: ModuleInstanceIndex,
};

pub const Program = struct {
    module_instances: ModuleInstanceBoundedArray,
    allocator: std.mem.Allocator,

    // functions: []FunctionInstance,
    // tables: []TableInstance,

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

    pub fn getFunctionReference(self: *const Self, m_idx: ModuleInstanceIndex, f_idx: FunctionIndex) !FunctionReference {
        _ = self;
        _ = m_idx;
        _ = f_idx;

        return 0xF0F;
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

                        const ref = try rt.popValueFunctionReference();

                        try i.setElement(seg.type, m.table, el_i, ref);
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
