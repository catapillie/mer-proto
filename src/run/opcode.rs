use std::io::{self, Cursor, Write};

use byteorder::WriteBytesExt;

opcodes! {
    0x00 nop
    0x01 pop
    0x02 dup
    0x03 dbg

    0xc0 jmp(u32)
    0xc1 jmp_if(u32)
    0xc2 ret
    0xc3 call(u32)

    0xd0 ld_loc(u8)
    0xd1 st_loc(u8)

    0xf0 entry_point(u32)
    0xf1 function(String, u8, u8)

    0xfe ld_unit

    0x10 ld_u8(u8)
    0x11 add_u8
    0x12 sub_u8
    0x13 mul_u8
    0x14 div_u8
    0x15 rem_u8
    0x16 eq_u8
    0x17 ne_u8
    0x18 le_u8
    0x19 lt_u8
    0x1a ge_u8
    0x1b gt_u8

    0x20 ld_u16(u16)
    0x21 add_u16
    0x22 sub_u16
    0x23 mul_u16
    0x24 div_u16
    0x25 rem_u16
    0x26 eq_u16
    0x27 ne_u16
    0x28 le_u16
    0x29 lt_u16
    0x2a ge_u16
    0x2b gt_u16

    0x30 ld_u32(u32)
    0x31 add_u32
    0x32 sub_u32
    0x33 mul_u32
    0x34 div_u32
    0x35 rem_u32
    0x36 eq_u32
    0x37 ne_u32
    0x38 le_u32
    0x39 lt_u32
    0x3a ge_u32
    0x3b gt_u32

    0x40 ld_u64(u64)
    0x41 add_u64
    0x42 sub_u64
    0x43 mul_u64
    0x44 div_u64
    0x45 rem_u64
    0x46 eq_u64
    0x47 ne_u64
    0x48 le_u64
    0x49 lt_u64
    0x4a ge_u64
    0x4b gt_u64

    0x50 ld_i8(i8)
    0x51 add_i8
    0x52 sub_i8
    0x53 mul_i8
    0x54 div_i8
    0x55 rem_i8
    0x56 eq_i8
    0x57 ne_i8
    0x58 le_i8
    0x59 lt_i8
    0x5a ge_i8
    0x5b gt_i8
    0x5c neg_i8

    0x60 ld_i16(i16)
    0x61 add_i16
    0x62 sub_i16
    0x63 mul_i16
    0x64 div_i16
    0x65 rem_i16
    0x66 eq_i16
    0x67 ne_i16
    0x68 le_i16
    0x69 lt_i16
    0x6a ge_i16
    0x6b gt_i16
    0x6c neg_i16

    0x70 ld_i32(i32)
    0x71 add_i32
    0x72 sub_i32
    0x73 mul_i32
    0x74 div_i32
    0x75 rem_i32
    0x76 eq_i32
    0x77 ne_i32
    0x78 le_i32
    0x79 lt_i32
    0x7a ge_i32
    0x7b gt_i32
    0x7c neg_i32

    0x80 ld_i64(i64)
    0x81 add_i64
    0x82 sub_i64
    0x83 mul_i64
    0x84 div_i64
    0x85 rem_i64
    0x86 eq_i64
    0x87 ne_i64
    0x88 le_i64
    0x89 lt_i64
    0x8a ge_i64
    0x8b gt_i64
    0x8c neg_i64

    0x90 ld_f32(f32)
    0x91 add_f32
    0x92 sub_f32
    0x93 mul_f32
    0x94 div_f32
    0x95 rem_f32
    0x96 eq_f32
    0x97 ne_f32
    0x98 le_f32
    0x99 lt_f32
    0x9a ge_f32
    0x9b gt_f32
    0x9c neg_f32

    0xa0 ld_f64(f64)
    0xa1 add_f64
    0xa2 sub_f64
    0xa3 mul_f64
    0xa4 div_f64
    0xa5 rem_f64
    0xa6 eq_f64
    0xa7 ne_f64
    0xa8 le_f64
    0xa9 lt_f64
    0xaa ge_f64
    0xab gt_f64
    0xac neg_f64

    0xb0 ld_bool_false
    0xb1 ld_bool_true
    0xb2 eq_bool
    0xb3 ne_bool
    0xb4 and_bool
    0xb5 or_bool
    0xb6 xor_bool
    0xb7 not_bool
}

impl Opcode {
    #[allow(dead_code)]
    pub fn write_bytes(&self, cursor: &mut Cursor<Vec<u8>>) -> io::Result<()> {
        cursor.write_u8(self.byte())?;
        match self {
            Opcode::ld_u8(val) => cursor.write_u8(*val)?,
            Opcode::ld_u16(val) => cursor.write_u16::<byteorder::LE>(*val)?,
            Opcode::ld_u32(val) => cursor.write_u32::<byteorder::LE>(*val)?,
            Opcode::ld_u64(val) => cursor.write_u64::<byteorder::LE>(*val)?,
            Opcode::ld_i8(val) => cursor.write_i8(*val)?,
            Opcode::ld_i16(val) => cursor.write_i16::<byteorder::LE>(*val)?,
            Opcode::ld_i32(val) => cursor.write_i32::<byteorder::LE>(*val)?,
            Opcode::ld_i64(val) => cursor.write_i64::<byteorder::LE>(*val)?,
            Opcode::ld_f32(val) => cursor.write_f32::<byteorder::LE>(*val)?,
            Opcode::ld_f64(val) => cursor.write_f64::<byteorder::LE>(*val)?,
            Opcode::jmp(to) => cursor.write_u32::<byteorder::LE>(*to)?,
            Opcode::jmp_if(to) => cursor.write_u32::<byteorder::LE>(*to)?,
            Opcode::call(fp) => cursor.write_u32::<byteorder::LE>(*fp)?,
            Opcode::ld_loc(loc) => cursor.write_u8(*loc)?,
            Opcode::st_loc(loc) => cursor.write_u8(*loc)?,
            Opcode::entry_point(fp) => cursor.write_u32::<byteorder::LE>(*fp)?,
            Opcode::function(name, param_count, local_count) => {
                let len = name.len() as u16;
                let bytes = name.as_bytes();

                cursor.write_u16::<byteorder::LE>(len)?;
                cursor.write_all(bytes)?;
                cursor.write_u8(*param_count)?;
                cursor.write_u8(*local_count)?;
            }
            _ => {}
        }

        Ok(())
    }
}

macro_rules! to_underscore {
    ($_:tt) => {
        _
    };
}

macro_rules! opcodes {
    (
        $(
            $byte:literal $name:ident
            $(
                ( $($type:ty),* )
            )?
        )*
    ) => {
        $(
            #[allow(non_upper_case_globals)]
            #[allow(dead_code)]
            pub const $name: u8 = $byte;
        )*

        pub fn name(opcode: u8) -> Option<&'static str> {
            match opcode {
                $(
                    $byte => Some(stringify!($name)),
                )*
                _ => None,
            }
        }

        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[derive(Debug)]
        pub enum Opcode {
            $(
                $name $(($($type),*))?,
            )*
        }

        impl Opcode {
            pub fn byte(&self) -> u8 {
                match self {
                    $(
                        Self::$name $(($(to_underscore!($type)),*))? => $byte,
                    )*
                }
            }

            #[cfg(test)]
            pub fn all_opcodes() -> Vec<u8> {
                vec![$($byte),*]
            }
        }
    };
}

use {opcodes, to_underscore};
