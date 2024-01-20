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
    0xc4 call_addr

    0xd0 ld_loc(u8)
    0xd1 st_loc(u8)

    0xe0 alloc
    0xe1 ld_heap
    0xe2 st_heap
    0xe3 realloc_loc(u8)

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
