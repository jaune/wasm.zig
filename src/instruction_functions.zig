const Runtime = @import("./runtime.zig").Runtime;
const Value = @import("./runtime.zig").Value;

const std = @import("std");

const math = @import("./math.zig");

pub fn @"local.get"(runtime: *Runtime, localidx: u32) !void {
    const frame = try runtime.peekCurrentCall();

    if (localidx >= frame.locals_length) {
        return error.InvalidLocalIndex;
    }

    const value = runtime.value_stack.get(frame.locals_start + localidx);

    try runtime.value_stack.append(value);
}

pub fn @"local.set"(runtime: *Runtime, localidx: u32) !void {
    if (runtime.call_stack.len == 0) {
        return error.EmptyCallStack;
    }

    const frame = &runtime.call_stack.slice()[runtime.call_stack.len - 1];

    if (localidx >= frame.locals_length) {
        return error.InvalidLocalIndex;
    }

    runtime.value_stack.slice()[frame.locals_start + localidx] = try runtime.popAnyValue();
}

pub fn @"local.tee"(runtime: *Runtime, localidx: u32) !void {
    _ = runtime;
    _ = localidx;

    return error.NotImplementedYet;
}

pub fn @"global.get"(runtime: *Runtime, globalidx: u32) !void {
    _ = runtime;
    _ = globalidx;

    return error.NotImplementedYet;
}

pub fn @"global.set"(runtime: *Runtime, globalidx: u32) !void {
    _ = runtime;
    _ = globalidx;

    return error.NotImplementedYet;
}

pub fn @"i32.const"(runtime: *Runtime, value: i32) !void {
    try runtime.pushValue(i32, value);

    return error.NotImplementedYet;
}

pub fn @"i64.const"(runtime: *Runtime, value: i64) !void {
    try runtime.pushValue(i64, value);

    return error.NotImplementedYet;
}

pub fn @"f32.const"(runtime: *Runtime, value: f32) !void {
    try runtime.pushValue(f32, value);

    return error.NotImplementedYet;
}

pub fn @"f64.const"(runtime: *Runtime, value: f64) !void {
    try runtime.pushValue(f64, value);

    return error.NotImplementedYet;
}

fn @"n.eq"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(i32, if (a == b) 1 else 0);
}
fn @"n.ne"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(i32, if (a != b) 1 else 0);
}

fn @"n.lt"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(i32, if (b < a) 1 else 0);
}

fn @"n.gt"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(i32, if (b > a) 1 else 0);
}

fn @"n.le"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(i32, if (b <= a) 1 else 0);
}

fn @"n.ge"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(i32, if (b >= a) 1 else 0);
}

fn @"i.eqz"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(i32, if (a == 0) 1 else 0);
}
fn @"i.lt_s"(comptime T: type, runtime: *Runtime) !void {
    try @"n.lt"(T, runtime);
}
fn @"i.lt_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(i32, if (@as(UnsignedType, @bitCast(b)) < @as(UnsignedType, @bitCast(a))) 1 else 0);
}
fn @"i.gt_s"(comptime T: type, runtime: *Runtime) !void {
    try @"n.gt"(T, runtime);
}
fn @"i.gt_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(i32, if (@as(UnsignedType, @bitCast(b)) > @as(UnsignedType, @bitCast(a))) 1 else 0);
}
fn @"i.le_s"(comptime T: type, runtime: *Runtime) !void {
    try @"n.le"(T, runtime);
}
fn @"i.le_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(i32, if (@as(UnsignedType, @bitCast(b)) <= @as(UnsignedType, @bitCast(a))) 1 else 0);
}
fn @"i.ge_s"(comptime T: type, runtime: *Runtime) !void {
    try @"n.ge"(T, runtime);
}
fn @"i.ge_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(i32, if (@as(UnsignedType, @bitCast(b)) >= @as(UnsignedType, @bitCast(a))) 1 else 0);
}

fn @"i.and"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, a & b);
}
fn @"i.or"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, a | b);
}
fn @"i.xor"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, b ^ a);
}
fn @"i.shl"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, try math.intShl(T, b, a));
}
fn @"i.shr_s"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, try math.intShr(T, b, a));
}
fn @"i.shr_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(T, @bitCast(try math.intShr(UnsignedType, @bitCast(b), @bitCast(a))));
}
fn @"i.rotl"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, try math.intRotl(T, b, a));
}
fn @"i.rotr"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, try math.intRotr(T, b, a));
}

fn @"i.clz"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, @clz(a));
}

fn @"i.ctz"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, @ctz(a));
}
fn @"i.popcnt"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, @popCount(a));
}
fn @"i.add"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, a +% b);
}
fn @"i.sub"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, b -% a);
}
fn @"i.mul"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, a *% b);
}
fn @"i.div_s"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, try std.math.divTrunc(T, b, a));
}

fn @"i.div_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(T, @bitCast(try std.math.divTrunc(UnsignedType, @bitCast(b), @bitCast(a))));
}

fn @"i.rem_s"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, try math.remS(T, b, a));
}

fn @"i.rem_u"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(T, @bitCast(try math.remS(UnsignedType, @bitCast(b), @bitCast(a))));
}

pub fn @"i32.eqz"(runtime: *Runtime) !void {
    try @"i.eqz"(i32, runtime);
}
pub fn @"i32.eq"(runtime: *Runtime) !void {
    try @"n.eq"(i32, runtime);
}
pub fn @"i32.ne"(runtime: *Runtime) !void {
    try @"n.ne"(i32, runtime);
}
pub fn @"i32.lt_s"(runtime: *Runtime) !void {
    try @"i.lt_s"(i32, runtime);
}
pub fn @"i32.lt_u"(runtime: *Runtime) !void {
    try @"i.lt_u"(i32, runtime);
}
pub fn @"i32.gt_s"(runtime: *Runtime) !void {
    try @"i.gt_s"(i32, runtime);
}
pub fn @"i32.gt_u"(runtime: *Runtime) !void {
    try @"i.gt_u"(i32, runtime);
}
pub fn @"i32.le_s"(runtime: *Runtime) !void {
    try @"i.le_s"(i32, runtime);
}
pub fn @"i32.le_u"(runtime: *Runtime) !void {
    try @"i.le_u"(i32, runtime);
}
pub fn @"i32.ge_s"(runtime: *Runtime) !void {
    try @"i.ge_s"(i32, runtime);
}
pub fn @"i32.ge_u"(runtime: *Runtime) !void {
    try @"i.ge_u"(i32, runtime);
}
pub fn @"i32.and"(runtime: *Runtime) !void {
    try @"i.and"(i32, runtime);
}
pub fn @"i32.or"(runtime: *Runtime) !void {
    try @"i.or"(i32, runtime);
}
pub fn @"i32.xor"(runtime: *Runtime) !void {
    try @"i.xor"(i32, runtime);
}
pub fn @"i32.shl"(runtime: *Runtime) !void {
    try @"i.shl"(i32, runtime);
}
pub fn @"i32.shr_s"(runtime: *Runtime) !void {
    try @"i.shr_s"(i32, runtime);
}
pub fn @"i32.shr_u"(runtime: *Runtime) !void {
    try @"i.shr_u"(i32, runtime);
}
pub fn @"i32.rotl"(runtime: *Runtime) !void {
    try @"i.rotl"(i32, runtime);
}
pub fn @"i32.rotr"(runtime: *Runtime) !void {
    try @"i.rotr"(i32, runtime);
}
pub fn @"i32.clz"(runtime: *Runtime) !void {
    try @"i.clz"(i32, runtime);
}
pub fn @"i32.ctz"(runtime: *Runtime) !void {
    try @"i.ctz"(i32, runtime);
}
pub fn @"i32.popcnt"(runtime: *Runtime) !void {
    try @"i.popcnt"(i32, runtime);
}
pub fn @"i32.add"(runtime: *Runtime) !void {
    try @"i.add"(i32, runtime);
}
pub fn @"i32.sub"(runtime: *Runtime) !void {
    try @"i.sub"(i32, runtime);
}
pub fn @"i32.mul"(runtime: *Runtime) !void {
    try @"i.mul"(i32, runtime);
}
pub fn @"i32.div_s"(runtime: *Runtime) !void {
    try @"i.div_s"(i32, runtime);
}

pub fn @"i32.div_u"(runtime: *Runtime) !void {
    try @"i.div_u"(i32, runtime);
}

pub fn @"i32.rem_s"(runtime: *Runtime) !void {
    try @"i.rem_s"(i32, runtime);
}

pub fn @"i32.rem_u"(runtime: *Runtime) !void {
    try @"i.rem_u"(i32, runtime);
}

pub fn @"i64.eqz"(runtime: *Runtime) !void {
    try @"i.eqz"(i64, runtime);
}
pub fn @"i64.eq"(runtime: *Runtime) !void {
    try @"n.eq"(i64, runtime);
}
pub fn @"i64.ne"(runtime: *Runtime) !void {
    try @"n.ne"(i64, runtime);
}
pub fn @"i64.lt_s"(runtime: *Runtime) !void {
    try @"i.lt_s"(i64, runtime);
}
pub fn @"i64.lt_u"(runtime: *Runtime) !void {
    try @"i.lt_u"(i64, runtime);
}
pub fn @"i64.gt_s"(runtime: *Runtime) !void {
    try @"i.gt_s"(i64, runtime);
}
pub fn @"i64.gt_u"(runtime: *Runtime) !void {
    try @"i.gt_u"(i64, runtime);
}
pub fn @"i64.le_s"(runtime: *Runtime) !void {
    try @"i.le_s"(i64, runtime);
}
pub fn @"i64.le_u"(runtime: *Runtime) !void {
    try @"i.le_u"(i64, runtime);
}
pub fn @"i64.ge_s"(runtime: *Runtime) !void {
    try @"i.ge_s"(i64, runtime);
}
pub fn @"i64.ge_u"(runtime: *Runtime) !void {
    try @"i.ge_u"(i64, runtime);
}
pub fn @"i64.and"(runtime: *Runtime) !void {
    try @"i.and"(i64, runtime);
}
pub fn @"i64.or"(runtime: *Runtime) !void {
    try @"i.or"(i64, runtime);
}
pub fn @"i64.xor"(runtime: *Runtime) !void {
    try @"i.xor"(i64, runtime);
}
pub fn @"i64.shl"(runtime: *Runtime) !void {
    try @"i.shl"(i64, runtime);
}
pub fn @"i64.shr_s"(runtime: *Runtime) !void {
    try @"i.shr_s"(i64, runtime);
}
pub fn @"i64.shr_u"(runtime: *Runtime) !void {
    try @"i.shr_u"(i64, runtime);
}
pub fn @"i64.rotl"(runtime: *Runtime) !void {
    try @"i.rotl"(i64, runtime);
}
pub fn @"i64.rotr"(runtime: *Runtime) !void {
    try @"i.rotr"(i64, runtime);
}
pub fn @"i64.clz"(runtime: *Runtime) !void {
    try @"i.clz"(i64, runtime);
}
pub fn @"i64.ctz"(runtime: *Runtime) !void {
    try @"i.ctz"(i64, runtime);
}
pub fn @"i64.popcnt"(runtime: *Runtime) !void {
    try @"i.popcnt"(i64, runtime);
}
pub fn @"i64.add"(runtime: *Runtime) !void {
    try @"i.add"(i64, runtime);
}
pub fn @"i64.sub"(runtime: *Runtime) !void {
    try @"i.sub"(i64, runtime);
}
pub fn @"i64.mul"(runtime: *Runtime) !void {
    try @"i.mul"(i64, runtime);
}
pub fn @"i64.div_s"(runtime: *Runtime) !void {
    try @"i.div_s"(i64, runtime);
}

pub fn @"i64.div_u"(runtime: *Runtime) !void {
    try @"i.div_u"(i64, runtime);
}

pub fn @"i64.rem_s"(runtime: *Runtime) !void {
    try @"i.rem_s"(i64, runtime);
}

pub fn @"i64.rem_u"(runtime: *Runtime) !void {
    try @"i.rem_u"(i64, runtime);
}

pub fn @"i.extend_s"(comptime T: type, comptime E: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, @as(E, @truncate(a)));
}

pub fn @"i.extend_i_s"(comptime T: type, comptime E: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(E);

    try runtime.pushValue(T, @as(E, @truncate(a)));
}

pub fn @"i.extend_i_u"(comptime T: type, comptime E: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(E);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(E));

    try runtime.pushValue(T, @as(UnsignedType, @truncate(@as(UnsignedType, @bitCast(a)))));
}

pub fn @"i32.extend8_s"(runtime: *Runtime) !void {
    try @"i.extend_s"(i32, i8, runtime);
}

pub fn @"i32.extend16_s"(runtime: *Runtime) !void {
    try @"i.extend_s"(i32, i16, runtime);
}

pub fn @"i64.extend8_s"(runtime: *Runtime) !void {
    try @"i.extend_s"(i64, i8, runtime);
}
pub fn @"i64.extend16_s"(runtime: *Runtime) !void {
    try @"i.extend_s"(i64, i16, runtime);
}
pub fn @"i64.extend32_s"(runtime: *Runtime) !void {
    try @"i.extend_s"(i64, i32, runtime);
}

pub fn @"f32.eq"(runtime: *Runtime) !void {
    try @"n.eq"(f32, runtime);
}
pub fn @"f32.ne"(runtime: *Runtime) !void {
    try @"n.ne"(f32, runtime);
}
pub fn @"f32.lt"(runtime: *Runtime) !void {
    try @"n.lt"(f32, runtime);
}
pub fn @"f32.gt"(runtime: *Runtime) !void {
    try @"n.gt"(f32, runtime);
}
pub fn @"f32.le"(runtime: *Runtime) !void {
    try @"n.le"(f32, runtime);
}
pub fn @"f32.ge"(runtime: *Runtime) !void {
    try @"n.ge"(f32, runtime);
}
pub fn @"f64.eq"(runtime: *Runtime) !void {
    try @"n.eq"(f64, runtime);
}
pub fn @"f64.ne"(runtime: *Runtime) !void {
    try @"n.ne"(f64, runtime);
}
pub fn @"f64.lt"(runtime: *Runtime) !void {
    try @"n.lt"(f64, runtime);
}
pub fn @"f64.gt"(runtime: *Runtime) !void {
    try @"n.gt"(f64, runtime);
}
pub fn @"f64.le"(runtime: *Runtime) !void {
    try @"n.le"(f64, runtime);
}
pub fn @"f64.ge"(runtime: *Runtime) !void {
    try @"n.ge"(f64, runtime);
}

pub fn @"f32.abs"(runtime: *Runtime) !void {
    try @"f.abs"(f32, runtime);
}
pub fn @"f32.neg"(runtime: *Runtime) !void {
    try @"f.neg"(f32, runtime);
}

pub fn @"f.abs"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, @abs(a));
}

pub fn @"f.neg"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, -a);
}

pub fn @"f.ceil"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, std.math.ceil(a));
}
pub fn @"f.floor"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, std.math.floor(a));
}
pub fn @"f.trunc"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, std.math.trunc(a));
}
pub fn @"f.nearest"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, try math.floatNearest(T, a));
}
pub fn @"f.sqrt"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);

    try runtime.pushValue(T, std.math.sqrt(a));
}
pub fn @"f.add"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, a + b);
}
pub fn @"f.sub"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, b - a);
}
pub fn @"f.mul"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, a * b);
}
pub fn @"f.div"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, b / a);
}
pub fn @"f.min"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    if (std.math.isNan(a)) {
        try runtime.pushValue(T, std.math.nan(T));
        return;
    }
    if (std.math.isNan(b)) {
        try runtime.pushValue(T, std.math.nan(T));
        return;
    }

    try runtime.pushValue(T, if (a > b) b else a);
}
pub fn @"f.max"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    if (std.math.isNan(a)) {
        try runtime.pushValue(T, std.math.nan(T));
        return;
    }
    if (std.math.isNan(b)) {
        try runtime.pushValue(T, std.math.nan(T));
        return;
    }

    try runtime.pushValue(T, if (a > b) a else b);
}

pub fn @"f.copysign"(comptime T: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(T);
    const b = try runtime.popValue(T);

    try runtime.pushValue(T, std.math.copysign(b, a));
}

pub fn @"f32.ceil"(runtime: *Runtime) !void {
    try @"f.ceil"(f32, runtime);
}
pub fn @"f32.floor"(runtime: *Runtime) !void {
    try @"f.floor"(f32, runtime);
}
pub fn @"f32.trunc"(runtime: *Runtime) !void {
    try @"f.trunc"(f32, runtime);
}
pub fn @"f32.nearest"(runtime: *Runtime) !void {
    try @"f.nearest"(f32, runtime);
}
pub fn @"f32.sqrt"(runtime: *Runtime) !void {
    try @"f.sqrt"(f32, runtime);
}
pub fn @"f32.add"(runtime: *Runtime) !void {
    try @"f.add"(f32, runtime);
}
pub fn @"f32.sub"(runtime: *Runtime) !void {
    try @"f.sub"(f32, runtime);
}
pub fn @"f32.mul"(runtime: *Runtime) !void {
    try @"f.mul"(f32, runtime);
}
pub fn @"f32.div"(runtime: *Runtime) !void {
    try @"f.div"(f32, runtime);
}
pub fn @"f32.min"(runtime: *Runtime) !void {
    try @"f.min"(f32, runtime);
}
pub fn @"f32.max"(runtime: *Runtime) !void {
    try @"f.max"(f32, runtime);
}
pub fn @"f32.copysign"(runtime: *Runtime) !void {
    try @"f.copysign"(f32, runtime);
}
pub fn @"f64.abs"(runtime: *Runtime) !void {
    try @"f.abs"(f64, runtime);
}
pub fn @"f64.neg"(runtime: *Runtime) !void {
    try @"f.neg"(f64, runtime);
}
pub fn @"f64.ceil"(runtime: *Runtime) !void {
    try @"f.ceil"(f64, runtime);
}
pub fn @"f64.floor"(runtime: *Runtime) !void {
    try @"f.floor"(f64, runtime);
}
pub fn @"f64.trunc"(runtime: *Runtime) !void {
    try @"f.trunc"(f64, runtime);
}
pub fn @"f64.nearest"(runtime: *Runtime) !void {
    try @"f.nearest"(f64, runtime);
}
pub fn @"f64.sqrt"(runtime: *Runtime) !void {
    try @"f.sqrt"(f64, runtime);
}
pub fn @"f64.add"(runtime: *Runtime) !void {
    try @"f.add"(f64, runtime);
}
pub fn @"f64.sub"(runtime: *Runtime) !void {
    try @"f.sub"(f64, runtime);
}
pub fn @"f64.mul"(runtime: *Runtime) !void {
    try @"f.mul"(f64, runtime);
}
pub fn @"f64.div"(runtime: *Runtime) !void {
    try @"f.div"(f64, runtime);
}
pub fn @"f64.min"(runtime: *Runtime) !void {
    try @"f.min"(f64, runtime);
}
pub fn @"f64.max"(runtime: *Runtime) !void {
    try @"f.max"(f64, runtime);
}

pub fn @"f64.copysign"(runtime: *Runtime) !void {
    try @"f.copysign"(f64, runtime);
}

pub fn call_indirect(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}

pub fn select(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn select_t(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.get"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.set"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.load"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32.load"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64.load"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.load8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.load8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.load16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.load16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load32_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.load32_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.store"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.store"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32.store"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64.store"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.store8"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.store16"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.store8"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.store16"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64.store32"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"memory.size"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"memory.grow"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32.wrap_i64"(runtime: *Runtime) !void {
    return @"i.wrap_i"(i32, i64, runtime);
}

fn @"i.wrap_i"(comptime T: type, comptime R: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(R);

    try runtime.pushValue(T, @truncate(a));
}

fn @"i.trunc_f_s"(comptime T: type, comptime F: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(F);

    try runtime.pushValue(T, try math.trunc(T, F, a));
}

pub fn @"i.trunc_f_u"(comptime T: type, comptime F: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(F);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(T, @bitCast(try math.trunc(UnsignedType, F, a)));
}

pub fn @"i32.trunc_f32_s"(runtime: *Runtime) !void {
    try @"i.trunc_f_s"(i32, f32, runtime);
}

pub fn @"i32.trunc_f32_u"(runtime: *Runtime) !void {
    try @"i.trunc_f_u"(i32, f32, runtime);
}
pub fn @"i32.trunc_f64_s"(runtime: *Runtime) !void {
    try @"i.trunc_f_s"(i32, f64, runtime);
}
pub fn @"i32.trunc_f64_u"(runtime: *Runtime) !void {
    try @"i.trunc_f_u"(i32, f64, runtime);
}
pub fn @"i64.extend_i32_s"(runtime: *Runtime) !void {
    try @"i.extend_i_s"(i64, i32, runtime);
}
pub fn @"i64.extend_i32_u"(runtime: *Runtime) !void {
    try @"i.extend_i_u"(i64, i32, runtime);
}
pub fn @"i64.trunc_f32_s"(runtime: *Runtime) !void {
    try @"i.trunc_f_s"(i64, f32, runtime);
}
pub fn @"i64.trunc_f32_u"(runtime: *Runtime) !void {
    try @"i.trunc_f_u"(i64, f32, runtime);
}
pub fn @"i64.trunc_f64_s"(runtime: *Runtime) !void {
    try @"i.trunc_f_s"(i64, f64, runtime);
}
pub fn @"i64.trunc_f64_u"(runtime: *Runtime) !void {
    try @"i.trunc_f_u"(i64, f64, runtime);
}

pub fn @"f32.convert_i32_s"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i32);

    try runtime.pushValue(f32, @floatFromInt(a));
}
pub fn @"f32.convert_i32_u"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i32);

    try runtime.pushValue(f32, @floatFromInt(@as(u32, @bitCast(a))));
}
pub fn @"f32.convert_i64_s"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i64);

    try runtime.pushValue(f32, @floatFromInt(a));
}
pub fn @"f32.convert_i64_u"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i64);

    try runtime.pushValue(f32, @floatFromInt(@as(u64, @bitCast(a))));
}

pub fn @"f32.demote_f64"(runtime: *Runtime) !void {
    const a = try runtime.popValue(f64);

    try runtime.pushValue(f32, @floatCast(a));
}
pub fn @"f64.convert_i32_s"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i32);

    try runtime.pushValue(f64, @floatFromInt(a));
}
pub fn @"f64.convert_i32_u"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i32);

    try runtime.pushValue(f64, @floatFromInt(@as(u32, @bitCast(a))));
}
pub fn @"f64.convert_i64_s"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i64);

    try runtime.pushValue(f64, @floatFromInt(a));
}
pub fn @"f64.convert_i64_u"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i64);

    try runtime.pushValue(f64, @floatFromInt(@as(u64, @bitCast(a))));
}
pub fn @"f64.promote_f32"(runtime: *Runtime) !void {
    const a = try runtime.popValue(f32);

    try runtime.pushValue(f64, a);
}

pub fn @"i32.reinterpret_f32"(runtime: *Runtime) !void {
    const a = try runtime.popValue(f32);

    try runtime.pushValue(i32, @bitCast(a));
}
pub fn @"i64.reinterpret_f64"(runtime: *Runtime) !void {
    const a = try runtime.popValue(f64);

    try runtime.pushValue(i64, @bitCast(a));
}
pub fn @"f32.reinterpret_i32"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i32);

    try runtime.pushValue(f32, @bitCast(a));
}
pub fn @"f64.reinterpret_i64"(runtime: *Runtime) !void {
    const a = try runtime.popValue(i64);

    try runtime.pushValue(f64, @bitCast(a));
}

pub fn @"ref.null"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"ref.is_null"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"ref.func"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}

pub fn @"i.trunc_sat_f_s"(comptime T: type, comptime F: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(F);

    try runtime.pushValue(T, math.truncSat(T, F, a));
}
pub fn @"i.trunc_sat_f_u"(comptime T: type, comptime F: type, runtime: *Runtime) !void {
    const a = try runtime.popValue(F);
    const UnsignedType: type = std.meta.Int(.unsigned, @bitSizeOf(T));

    try runtime.pushValue(T, @bitCast(math.truncSat(UnsignedType, F, a)));
}

pub fn @"i32.trunc_sat_f32_s"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_s"(i32, f32, runtime);
}
pub fn @"i32.trunc_sat_f32_u"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_u"(i32, f32, runtime);
}

pub fn @"i32.trunc_sat_f64_s"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_s"(i32, f64, runtime);
}
pub fn @"i32.trunc_sat_f64_u"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_u"(i32, f64, runtime);
}

pub fn @"i64.trunc_sat_f32_s"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_s"(i64, f32, runtime);
}
pub fn @"i64.trunc_sat_f32_u"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_u"(i64, f32, runtime);
}

pub fn @"i64.trunc_sat_f64_s"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_s"(i64, f64, runtime);
}
pub fn @"i64.trunc_sat_f64_u"(runtime: *Runtime) !void {
    return @"i.trunc_sat_f_u"(i64, f64, runtime);
}

pub fn @"memory.init"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"data.drop"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"memory.copy"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"memory.fill"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.init"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"elem.drop"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.copy"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.grow"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.size"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"table.fill"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load8x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load8x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load16x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load16x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load32x2_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load32x2_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load8_splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load16_splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load32_splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load64_splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.store"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.const"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.shuffle"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.swizzle"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.splat"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.extract_lane_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.extract_lane_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.replace_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extract_lane_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extract_lane_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.replace_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extract_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.replace_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extract_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.replace_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.extract_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.replace_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.extract_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.replace_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.eq"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.ne"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.lt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.lt_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.gt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.gt_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.le_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.le_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.ge_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.ge_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.eq"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.ne"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.lt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.lt_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.gt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.gt_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.le_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.le_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.ge_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.ge_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.eq"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.ne"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.lt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.lt_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.gt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.gt_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.le_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.le_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.ge_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.ge_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.eq"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.ne"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.lt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.gt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.le"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.ge"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.eq"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.ne"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.lt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.gt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.le"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.ge"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.not"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.and"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.andnot"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.or"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.xor"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.bitselect"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.any_true"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load8_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load16_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load32_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load64_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.store8_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.store16_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.store32_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.store64_lane"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load32_zero"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"v128.load64_zero"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.demote_f64x2_zero"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.promote_low_f32x4"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.abs"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.neg"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.popcnt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.all_true"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.bitmask"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.narrow_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.narrow_i16x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.ceil"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.floor"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.trunc"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.nearest"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.shl"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.shr_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.shr_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.add"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.add_sat_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.add_sat_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.sub"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.sub_sat_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.sub_sat_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.ceil"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.floor"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.min_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.min_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.max_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.max_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.trunc"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i8x16.avgr_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extadd_pairwise_i8x16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extadd_pairwise_i8x16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extadd_pairwise_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extadd_pairwise_i16x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.abs"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.neg"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.q15mulr_sat_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.all_true"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.bitmask"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.narrow_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.narrow_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extend_low_i8x16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extend_high_i8x16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extend_low_i8x16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extend_high_i8x16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.shl"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.shr_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.shr_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.add"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.add_sat_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.add_sat_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.sub"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.sub_sat_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.sub_sat_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.nearest"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.mul"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.min_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.min_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.max_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.max_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.avgr_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extmul_low_i8x16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extmul_high_i8x16_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extmul_low_i8x16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i16x8.extmul_high_i8x16_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.abs"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.neg"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.all_true"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.bitmask"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extend_low_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extend_high_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extend_low_i16x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extend_high_i16x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.shl"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.shr_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.shr_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.add"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.sub"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.mul"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.min_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.min_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.max_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.max_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.dot_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extmul_low_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extmul_high_i16x8_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extmul_low_i16x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.extmul_high_i16x8_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.abs"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.neg"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.all_true"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.bitmask"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extend_low_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extend_high_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extend_low_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extend_high_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.shl"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.shr_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.shr_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.add"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.sub"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.mul"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.eq"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.ne"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.lt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.gt_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.le_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.ge_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extmul_low_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extmul_high_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extmul_low_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i64x2.extmul_high_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.abs"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.neg"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.sqrt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.add"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.sub"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.mul"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.div"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.min"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.max"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.pmin"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.pmax"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.abs"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.neg"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.sqrt"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.add"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.sub"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.mul"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.div"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.min"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.max"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.pmin"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.pmax"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.trunc_sat_f32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.trunc_sat_f32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.convert_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f32x4.convert_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.trunc_sat_f64x2_s_zero"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"i32x4.trunc_sat_f64x2_u_zero"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.convert_low_i32x4_s"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
pub fn @"f64x2.convert_low_i32x4_u"(runtime: *Runtime) !void {
    _ = runtime;
    return error.NotImplementedYet;
}
