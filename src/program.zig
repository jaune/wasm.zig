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

const max_module_instances = @import("./module.zig").max_module_instances;

const logExpression = @import("./module.zig").logExpression;
const logRuntime = @import("./runtime.zig").logRuntime;

const ExpressionBuilder = @import("./expression_builder.zig").ExpressionBuilder;
const FunctionType = @import("./expression_builder.zig").ExpressionBuilder;

const Runtime = @import("./runtime.zig").Runtime;

const executeExpression = @import("./runtime.zig").executeExpression;

const TableInstance = struct {
    type: ReferenceType,
    limits: Limits,
    elements: []AnyReference,

    const Self = @This();

    pub fn getAnyElement(self: *const Self, i: usize) !AnyReference {
        if (i >= self.elements.len) {
            return error.OutOfBounds;
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
                .elements = try allocator.alloc(AnyReference, max),
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
