use byteorder::WriteBytesExt;
use std::io::{self, Cursor, Write};

use super::native_type::NativeType;

opcodes! {
    0x00 nop
    0x01 pop
    0x02 dup
    0x03 dbg(NativeType)

    0xc0 jmp(u32)
    0xc1 jmp_if(u32)
    0xc2 ret
    0xc3 call(u32)

    0xd0 ld_loc(u8)
    0xd1 st_loc(u8)

    0xe0 alloc
    0xe1 ld_heap
    0xe2 st_heap

    0xf0 entry_point(u32)
    0xf1 function(String, u8, u8)

    0x10 ld_u8(u8)
    0x11 ld_u16(u16)
    0x12 ld_u32(u32)
    0x13 ld_u64(u64)
    0x14 ld_i8(i8)
    0x15 ld_i16(i16)
    0x16 ld_i32(i32)
    0x17 ld_i64(i64)
    0x18 ld_f32(f32)
    0x19 ld_f64(f64)
    0x1a ld_unit

    0x21 add(NativeType)
    0x22 sub(NativeType)
    0x23 mul(NativeType)
    0x24 div(NativeType)
    0x25 rem(NativeType)
    0x26 eq(NativeType)
    0x27 ne(NativeType)
    0x28 le(NativeType)
    0x29 lt(NativeType)
    0x2a ge(NativeType)
    0x2b gt(NativeType)
    0x2c bitand(NativeType)
    0x2d bitor(NativeType)
    0x2e bitxor(NativeType)
    0x2f neg(NativeType)

    0xfe todo
    0xfd unreachable
}

impl Opcode {
    #[allow(dead_code)]
    #[rustfmt::skip]
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
            Opcode::dbg(ty) => cursor.write_u8((*ty).into())?,
            Opcode::add(ty) => cursor.write_u8((*ty).into())?,
            Opcode::sub(ty) => cursor.write_u8((*ty).into())?,
            Opcode::mul(ty) => cursor.write_u8((*ty).into())?,
            Opcode::div(ty) => cursor.write_u8((*ty).into())?,
            Opcode::rem(ty) => cursor.write_u8((*ty).into())?,
            Opcode::eq(ty) => cursor.write_u8((*ty).into())?,
            Opcode::ne(ty) => cursor.write_u8((*ty).into())?,
            Opcode::le(ty) => cursor.write_u8((*ty).into())?,
            Opcode::lt(ty) => cursor.write_u8((*ty).into())?,
            Opcode::ge(ty) => cursor.write_u8((*ty).into())?,
            Opcode::gt(ty) => cursor.write_u8((*ty).into())?,
            Opcode::bitand(ty) => cursor.write_u8((*ty).into())?,
            Opcode::bitor(ty) => cursor.write_u8((*ty).into())?,
            Opcode::bitxor(ty) => cursor.write_u8((*ty).into())?,
            Opcode::neg(ty) => cursor.write_u8((*ty).into())?,
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

        pub fn name_of(opcode: u8) -> Option<&'static str> {
            match opcode {
                $(
                    $byte => Some(stringify!($name)),
                )*
                _ => None,
            }
        }

        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[derive(Debug, PartialEq, Clone)]
        #[repr(u8)]
        pub enum Opcode {
            $(
                $name $(($($type),*))? = $byte,
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

            pub fn name(&self) -> &'static str {
                match self {
                    $(
                        Self::$name $(($(to_underscore!($type)),*))? => stringify!($name),
                    )*
                }
            }
        }

        #[cfg(test)]
        pub fn all_opcode_bytes() -> Vec<u8> {
            vec![$($byte),*]
        }
    };
}

use {opcodes, to_underscore};
