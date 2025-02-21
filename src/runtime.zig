const std = @import("std");

const Module = @import("./module.zig").Module;
const Instruction = @import("./module.zig").Instruction;
const InstructionTag = @import("./module.zig").InstructionTag;
const FunctionType = @import("./module.zig").FunctionType;

pub const Value = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,

    pub fn eql(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return false;
        }

        return switch (a) {
            .i32 => a.i32 == b.i32,
            .i64 => a.i64 == b.i64,
            .f32 => a.f32 == b.f32,
            .f64 => a.f64 == b.f64,
        };
    }
};

pub const Runtime = struct {
    const Len: type = std.math.IntFittingRange(0, 1024);
    const ValueStackType = std.BoundedArray(Value, 1024);

    const CallStackEntry = struct {
        locals_start: Len,
        locals_length: Len,
    };
    const CallStackType = std.BoundedArray(CallStackEntry, 1024);

    const Self = @This();

    call_stack: CallStackType,
    value_stack: ValueStackType,

    fn popValue(self: *Self, comptime T: type) T {
        switch (T) {
            i32 => {
                return self.value_stack.pop().i32;
            },
            i64 => {
                return self.value_stack.pop().i64;
            },
            f32 => {
                return self.value_stack.pop().f32;
            },
            f64 => {
                return self.value_stack.pop().f64;
            },
            else => {
                @compileError("Unsupported value type");
            },
        }
    }

    fn pushValue(self: *Self, comptime T: type, value: T) !void {
        switch (T) {
            i32 => {
                try self.value_stack.append(.{ .i32 = value });
            },
            i64 => {
                try self.value_stack.append(.{ .i64 = value });
            },
            f32 => {
                try self.value_stack.append(.{ .f32 = value });
            },
            f64 => {
                try self.value_stack.append(.{ .f64 = value });
            },
            else => {
                @compileError("Unsupported value type");
            },
        }
    }

    pub fn init() Self {
        return .{
            .call_stack = CallStackType{},
            .value_stack = ValueStackType{},
        };
    }
};

fn executeInstruction(runtime: *Runtime, instruction: Instruction) !void {
    switch (instruction) {
        inline else => |parameters, tag| {
            const tag_name = comptime @tagName(tag);
            const func = comptime @field(InstructionFunctions, tag_name);

            const ArgTuple: type = comptime std.meta.ArgsTuple(@TypeOf(func));
            var args: ArgTuple = undefined;

            args.@"0" = runtime;

            inline for (std.meta.fields(@TypeOf(parameters)), 1..) |p, i| {
                @field(args, std.meta.fields(ArgTuple)[i].name) = @field(parameters, p.name);
            }

            _ = try @call(.auto, func, args);
        },
    }
}

pub fn invokeFunction(m: *const Module, runtime: *Runtime, func_idx: u32, parameters: []const Value, results: []Value) !void {
    const b = m.function_bodies[func_idx];
    const function_type: FunctionType = m.function_types[m.function_type_indices[func_idx]];

    if (function_type.parameters.len != parameters.len) {
        return error.WrongNumberOfParameters;
    }

    if (function_type.results.len != results.len) {
        return error.WrongNumberOfResults;
    }

    var frame = try runtime.call_stack.addOne();

    frame.locals_start = runtime.value_stack.len;

    for (function_type.parameters, 0..) |p, i| {
        switch (p) {
            .i32 => {
                const value = switch (parameters[i]) {
                    .i32 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.pushValue(i32, value);
            },
            .i64 => {
                const value = switch (parameters[i]) {
                    .i64 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.pushValue(i64, value);
            },
            .f32 => {
                const value = switch (parameters[i]) {
                    .f32 => |v| v,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
                try runtime.value_stack.append(.{ .f32 = value });
            },
            else => {
                return error.UnsupportedParameterType;
            },
        }
    }

    const locals_end = runtime.value_stack.len;

    frame.locals_length = locals_end - frame.locals_start;

    for (b.expression) |instruction| {
        try executeInstruction(runtime, instruction);
    }

    const expected_length = locals_end + function_type.results.len;

    if (expected_length != runtime.value_stack.len) {
        return error.WrongNumberOfResults;
    }

    for (function_type.results, 0..) |p, i| {
        const value = runtime.value_stack.pop();

        switch (p) {
            .i32 => {
                results[i] = switch (value) {
                    .i32 => value,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
            },
            .f32 => {
                results[i] = switch (value) {
                    .f32 => value,
                    else => {
                        return error.InvalidParameterType;
                    },
                };
            },
            else => {
                return error.UnsupportedResultType;
            },
        }
    }

    runtime.value_stack.len -= frame.locals_length;
    runtime.call_stack.len -= 1;

    if (runtime.value_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
    if (runtime.call_stack.len != 0) {
        return error.UnfinishedBusiness;
    }
}

const InstructionFunctions = struct {
    pub fn @"local.get"(runtime: *Runtime, localidx: u32) !void {
        const frame = runtime.call_stack.get(runtime.call_stack.len - 1);

        if (localidx >= frame.locals_length) {
            return error.@"local.get: invalid local index";
        }

        const value = runtime.value_stack.get(frame.locals_start + localidx);

        try runtime.value_stack.append(value);
    }

    pub fn @"local.set"(runtime: *Runtime, localidx: u32) !void {
        _ = runtime;
        _ = localidx;
    }

    pub fn @"local.tee"(runtime: *Runtime, localidx: u32) !void {
        _ = runtime;
        _ = localidx;
    }

    pub fn @"global.get"(runtime: *Runtime, globalidx: u32) !void {
        _ = runtime;
        _ = globalidx;
    }

    pub fn @"global.set"(runtime: *Runtime, globalidx: u32) !void {
        _ = runtime;
        _ = globalidx;
    }

    pub fn @"i32.const"(runtime: *Runtime, value: i32) !void {
        _ = runtime;
        _ = value;
    }

    pub fn @"i64.const"(runtime: *Runtime, value: i64) !void {
        _ = runtime;
        _ = value;
    }

    pub fn @"f32.const"(runtime: *Runtime, value: f32) !void {
        _ = runtime;
        _ = value;
    }

    pub fn @"f64.const"(runtime: *Runtime, value: f64) !void {
        _ = runtime;
        _ = value;
    }

    fn @"i32.eqz"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);

        try runtime.pushValue(i32, if (a == 0) 1 else 0);
    }
    fn @"i32.eq"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (a == b) 1 else 0);
    }
    fn @"i32.ne"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (a != b) 1 else 0);
    }
    fn @"i32.lt_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (b < a) 1 else 0);
    }
    fn @"i32.lt_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (@as(u32, @bitCast(b)) < @as(u32, @bitCast(a))) 1 else 0);
    }
    fn @"i32.gt_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (b > a) 1 else 0);
    }
    fn @"i32.gt_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (@as(u32, @bitCast(b)) > @as(u32, @bitCast(a))) 1 else 0);
    }
    fn @"i32.le_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (b <= a) 1 else 0);
    }
    fn @"i32.le_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (@as(u32, @bitCast(b)) <= @as(u32, @bitCast(a))) 1 else 0);
    }
    fn @"i32.ge_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (b >= a) 1 else 0);
    }
    fn @"i32.ge_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, if (@as(u32, @bitCast(b)) >= @as(u32, @bitCast(a))) 1 else 0);
    }
    fn @"i64.eqz"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.eqz", .{});
    }
    fn @"i64.eq"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.eq", .{});
    }
    fn @"i64.ne"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.ne", .{});
    }
    fn @"i64.lt_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.lt_s", .{});
    }
    fn @"i64.lt_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.lt_u", .{});
    }
    fn @"i64.gt_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.gt_s", .{});
    }
    fn @"i64.gt_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.gt_u", .{});
    }
    fn @"i64.le_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.le_s", .{});
    }
    fn @"i64.le_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.le_u", .{});
    }
    fn @"i64.ge_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.ge_s", .{});
    }
    fn @"i64.ge_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.ge_u", .{});
    }
    fn @"f32.eq"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.eq", .{});
    }
    fn @"f32.ne"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.ne", .{});
    }
    fn @"f32.lt"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.lt", .{});
    }
    fn @"f32.gt"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.gt", .{});
    }
    fn @"f32.le"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.le", .{});
    }
    fn @"f32.ge"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.ge", .{});
    }
    fn @"f64.eq"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.eq", .{});
    }
    fn @"f64.ne"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.ne", .{});
    }
    fn @"f64.lt"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.lt", .{});
    }
    fn @"f64.gt"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.gt", .{});
    }
    fn @"f64.le"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.le", .{});
    }
    fn @"f64.ge"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.ge", .{});
    }
    fn @"i32.clz"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);

        try runtime.pushValue(i32, @clz(a));
    }
    fn @"i32.ctz"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);

        try runtime.pushValue(i32, @ctz(a));
    }
    fn @"i32.popcnt"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);

        try runtime.pushValue(i32, @popCount(a));
    }
    fn @"i32.add"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, a +% b);
    }
    fn @"i32.sub"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, b -% a);
    }
    fn @"i32.mul"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, a *% b);
    }
    fn @"i32.div_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, try std.math.divTrunc(i32, b, a));
    }

    fn @"i32.div_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, @bitCast(try std.math.divTrunc(u32, @bitCast(b), @bitCast(a))));
    }

    fn @"i32.rem_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, try remS(i32, b, a));
    }

    fn @"i32.rem_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, @bitCast(try remS(u32, @bitCast(b), @bitCast(a))));
    }
    fn @"i32.and"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, a & b);
    }
    fn @"i32.or"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, a | b);
    }
    fn @"i32.xor"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, b ^ a);
    }
    fn @"i32.shl"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, try intShl(i32, b, a));
    }
    fn @"i32.shr_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, try intShr(i32, b, a));
    }
    fn @"i32.shr_u"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, @bitCast(try intShr(u32, @bitCast(b), @bitCast(a))));
    }
    fn @"i32.rotl"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, try intRotl(i32, b, a));
    }
    fn @"i32.rotr"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);
        const b = runtime.popValue(i32);

        try runtime.pushValue(i32, try intRotr(i32, b, a));
    }
    fn @"i64.clz"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.clz", .{});
    }
    fn @"i64.ctz"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.ctz", .{});
    }
    fn @"i64.popcnt"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.popcnt", .{});
    }
    fn @"i64.add"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.add", .{});
    }
    fn @"i64.sub"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.sub", .{});
    }
    fn @"i64.mul"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.mul", .{});
    }
    fn @"i64.div_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.div_s", .{});
    }
    fn @"i64.div_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.div_u", .{});
    }
    fn @"i64.rem_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.rem_s", .{});
    }
    fn @"i64.rem_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.rem_u", .{});
    }
    fn @"i64.and"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.and", .{});
    }
    fn @"i64.or"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.or", .{});
    }
    fn @"i64.xor"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.xor", .{});
    }
    fn @"i64.shl"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.shl", .{});
    }
    fn @"i64.shr_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.shr_s", .{});
    }
    fn @"i64.shr_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.shr_u", .{});
    }
    fn @"i64.rotl"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.rotl", .{});
    }
    fn @"i64.rotr"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.rotr", .{});
    }
    fn @"f32.abs"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.abs", .{});
    }
    fn @"f32.neg"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.neg", .{});
    }
    fn @"f32.ceil"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = std.math.ceil(a) });
    }
    fn @"f32.floor"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = std.math.floor(a) });
    }
    fn @"f32.trunc"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = std.math.trunc(a) });
    }
    fn @"f32.nearest"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = try floatNearest(f32, a) });
    }
    fn @"f32.sqrt"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = std.math.sqrt(a) });
    }
    fn @"f32.add"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;
        const b = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = a + b });
    }
    fn @"f32.sub"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;
        const b = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = b - a });
    }
    fn @"f32.mul"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;
        const b = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = a * b });
    }
    fn @"f32.div"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;
        const b = runtime.value_stack.pop().f32;

        try runtime.value_stack.append(.{ .f32 = b / a });
    }
    fn @"f32.min"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;
        const b = runtime.value_stack.pop().f32;

        if (std.math.isNan(a)) {
            try runtime.value_stack.append(.{
                .f32 = std.math.nan(f32),
            });
            return;
        }
        if (std.math.isNan(b)) {
            try runtime.value_stack.append(.{
                .f32 = std.math.nan(f32),
            });
            return;
        }

        try runtime.value_stack.append(.{
            .f32 = if (a > b) b else a,
        });
    }
    fn @"f32.max"(runtime: *Runtime) !void {
        const a = runtime.value_stack.pop().f32;
        const b = runtime.value_stack.pop().f32;

        if (std.math.isNan(a)) {
            try runtime.value_stack.append(.{
                .f32 = std.math.nan(f32),
            });
            return;
        }
        if (std.math.isNan(b)) {
            try runtime.value_stack.append(.{
                .f32 = std.math.nan(f32),
            });
            return;
        }

        try runtime.value_stack.append(.{
            .f32 = if (a > b) a else b,
        });
    }
    fn @"f32.copysign"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.copysign", .{});
    }
    fn @"f64.abs"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.abs", .{});
    }
    fn @"f64.neg"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.neg", .{});
    }
    fn @"f64.ceil"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.ceil", .{});
    }
    fn @"f64.floor"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.floor", .{});
    }
    fn @"f64.trunc"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.trunc", .{});
    }
    fn @"f64.nearest"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.nearest", .{});
    }
    fn @"f64.sqrt"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.sqrt", .{});
    }
    fn @"f64.add"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.add", .{});
    }
    fn @"f64.sub"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.sub", .{});
    }
    fn @"f64.mul"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.mul", .{});
    }
    fn @"f64.div"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.div", .{});
    }
    fn @"f64.min"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.min", .{});
    }
    fn @"f64.max"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.max", .{});
    }
    fn @"f64.copysign"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.copysign", .{});
    }
    fn @"i32.wrap_i64"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i32.wrap_i64", .{});
    }
    fn @"i32.trunc_f32_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i32.trunc_f32_s", .{});
    }
    fn @"i32.trunc_f32_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i32.trunc_f32_u", .{});
    }
    fn @"i32.trunc_f64_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i32.trunc_f64_s", .{});
    }
    fn @"i32.trunc_f64_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i32.trunc_f64_u", .{});
    }
    fn @"i64.extend_i32_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.extend_i32_s", .{});
    }
    fn @"i64.extend_i32_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.extend_i32_u", .{});
    }
    fn @"i64.trunc_f32_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.trunc_f32_s", .{});
    }
    fn @"i64.trunc_f32_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.trunc_f32_u", .{});
    }
    fn @"i64.trunc_f64_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.trunc_f64_s", .{});
    }
    fn @"i64.trunc_f64_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.trunc_f64_u", .{});
    }
    fn @"f32.convert_i32_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.convert_i32_s", .{});
    }
    fn @"f32.convert_i32_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.convert_i32_u", .{});
    }
    fn @"f32.convert_i64_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.convert_i64_s", .{});
    }
    fn @"f32.convert_i64_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.convert_i64_u", .{});
    }
    fn @"f32.demote_f64"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.demote_f64", .{});
    }
    fn @"f64.convert_i32_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.convert_i32_s", .{});
    }
    fn @"f64.convert_i32_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.convert_i32_u", .{});
    }
    fn @"f64.convert_i64_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.convert_i64_s", .{});
    }
    fn @"f64.convert_i64_u"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.convert_i64_u", .{});
    }
    fn @"f64.promote_f32"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.promote_f32", .{});
    }
    fn @"i32.reinterpret_f32"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i32.reinterpret_f32", .{});
    }
    fn @"i64.reinterpret_f64"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.reinterpret_f64", .{});
    }
    fn @"f32.reinterpret_i32"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f32.reinterpret_i32", .{});
    }
    fn @"f64.reinterpret_i64"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside f64.reinterpret_i64", .{});
    }
    fn @"i32.extend8_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);

        try runtime.pushValue(i32, @as(i8, @truncate(a)));
    }
    fn @"i32.extend16_s"(runtime: *Runtime) !void {
        const a = runtime.popValue(i32);

        try runtime.pushValue(i32, @as(i16, @truncate(a)));
    }
    fn @"i64.extend8_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.extend8_s", .{});
    }
    fn @"i64.extend16_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.extend16_s", .{});
    }
    fn @"i64.extend32_s"(runtime: *Runtime) !void {
        _ = runtime;
        std.log.info("inside i64.extend32_s", .{});
    }

    fn end(runtime: *Runtime) !void {
        _ = runtime;
    }
};

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
fn intShl(comptime T: type, lhs: T, rhs: T) !T {
    const Log2T: type = comptime std.meta.Int(.unsigned, std.math.log2(@typeInfo(T).Int.bits));

    const shift = try std.math.mod(T, rhs, @bitSizeOf(T));

    const casted = std.math.cast(Log2T, shift) orelse {
        return error.TooBig;
    };

    return lhs << casted;
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
fn intShr(comptime T: type, lhs: T, rhs: T) !T {
    const Log2T: type = comptime std.meta.Int(.unsigned, std.math.log2(@typeInfo(T).Int.bits));

    const shift = try std.math.mod(T, rhs, @bitSizeOf(T));

    const casted = std.math.cast(Log2T, shift) orelse {
        return error.TooBig;
    };

    return lhs >> casted;
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
fn remS(comptime T: type, numerator: T, denominator: T) !T {
    if (denominator < 0) {
        return try std.math.rem(T, numerator, denominator * -1);
    }
    return try std.math.rem(T, numerator, denominator);
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
fn intRotl(comptime T: type, lhs: T, rhs: T) !T {
    const UnsignedType = std.meta.Int(.unsigned, @bitSizeOf(T));
    const num: UnsignedType = @bitCast(lhs);
    const res = std.math.rotl(UnsignedType, num, rhs);
    return @bitCast(res);
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
fn intRotr(comptime T: type, lhs: T, rhs: T) !T {
    const UnsignedType = std.meta.Int(.unsigned, @bitSizeOf(T));
    const num: UnsignedType = @bitCast(lhs);
    const res = std.math.rotr(UnsignedType, num, rhs);
    return @bitCast(res);
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
fn floatNearest(comptime T: type, value: T) !T {
    if (std.math.isInf(value))
        return value;

    const val: T = @trunc(value);
    if (value == val)
        return value;

    if (val == 0 and 0 < value and value <= 0.5)
        return 0.0;

    if (val == 0 and -0.5 <= value and value < -0.0)
        return -0.0;

    const q = value - val;
    if (q == 0.5 and try std.math.mod(T, val, 2.0) != 0.0)
        return val + 1;
    if (q == -0.5 and try std.math.mod(T, val, 2.0) != 0.0)
        return val - 1;

    return val;
}
