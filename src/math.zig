const std = @import("std");

pub fn trunc(comptime R: type, comptime T: type, value: T) !R {
    if (R != i32 and R != u32 and R != i64 and R != u64 and T != f32 and T != f64) {
        @compileError("Invalid Number Type");
    }

    if (std.math.isNan(value)) {
        return error.InvalidCastToInt;
    }

    if (std.math.isNegativeInf(value)) {
        return error.Overflow;
    }

    if (std.math.isPositiveInf(value)) {
        return error.Overflow;
    }

    const tval = @trunc(value);

    if (tval > std.math.maxInt(R)) {
        return error.Overflow;
    }

    if (tval == std.math.maxInt(R)) {
        return @as(R, @intFromFloat(std.math.maxInt(R)));
    }

    if (tval < std.math.minInt(R)) {
        return error.Overflow;
    }

    return @as(R, @intFromFloat(tval));
}

pub fn truncSat(comptime R: type, comptime T: type, value: T) R {
    if (R != i32 and R != u32 and R != i64 and R != u64 and T != f32 and T != f64) {
        @compileError("Invalid Number Type");
    }

    const max = std.math.maxInt(R);
    const min = std.math.minInt(R);

    if (std.math.isNan(value))
        return 0;
    if (std.math.isNegativeInf(value))
        return min;
    if (std.math.isPositiveInf(value))
        return max;

    const tval = @trunc(value);
    if (tval >= max)
        return max;
    if (tval <= min)
        return min;

    return @as(R, @intFromFloat(tval));
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
pub fn remS(comptime T: type, numerator: T, denominator: T) !T {
    if (denominator < 0) {
        return try std.math.rem(T, numerator, denominator * -1);
    }
    return try std.math.rem(T, numerator, denominator);
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
pub fn intRotl(comptime T: type, lhs: T, rhs: T) !T {
    const UnsignedType = std.meta.Int(.unsigned, @bitSizeOf(T));
    const num: UnsignedType = @bitCast(lhs);
    const res = std.math.rotl(UnsignedType, num, rhs);

    return @bitCast(res);
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
pub fn intRotr(comptime T: type, lhs: T, rhs: T) !T {
    const UnsignedType = std.meta.Int(.unsigned, @bitSizeOf(T));
    const num: UnsignedType = @bitCast(lhs);
    const res = std.math.rotr(UnsignedType, num, rhs);

    return @bitCast(res);
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
pub fn floatNearest(comptime T: type, value: T) !T {
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

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
pub fn intShl(comptime T: type, lhs: T, rhs: T) !T {
    const Log2T: type = comptime std.meta.Int(.unsigned, std.math.log2(@typeInfo(T).Int.bits));

    const shift = try std.math.mod(T, rhs, @bitSizeOf(T));

    const casted = std.math.cast(Log2T, shift) orelse {
        return error.TooBig;
    };

    return lhs << casted;
}

// CREDIT: https://github.com/safx/zig-tiny-wasm-runtime
pub fn intShr(comptime T: type, lhs: T, rhs: T) !T {
    const Log2T: type = comptime std.meta.Int(.unsigned, std.math.log2(@typeInfo(T).Int.bits));

    const shift = try std.math.mod(T, rhs, @bitSizeOf(T));

    const casted = std.math.cast(Log2T, shift) orelse {
        return error.TooBig;
    };

    return lhs >> casted;
}
