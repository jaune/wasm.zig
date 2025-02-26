const std = @import("std");

const Opcode = @import("./opcode.zig").Opcode;

pub fn readOpcode(reader: anytype) !Opcode {
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
            return .select_t;
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
            const op1 = try reader.readByte();
            switch (op1) {
                0x00 => {
                    return .@"memory.size";
                },
                else => {
                    return error.InvalidOpcode;
                },
            }
        },
        0x40 => {
            const op1 = try reader.readByte();
            switch (op1) {
                0x00 => {
                    return .@"memory.grow";
                },
                else => {
                    return error.InvalidOpcode;
                },
            }
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
            std.log.err("op0: 0x{x}", .{op0});
            return error.InvalidOpcode;
        },
    }
}
