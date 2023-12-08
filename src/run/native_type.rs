use std::fmt::Display;

pub struct InvalidNativeTypeByte;

types! {
    0x00 unit
    0x01 bool
    0x02 u8
    0x03 u16
    0x04 u32
    0x05 u64
    0x06 i8
    0x07 i16
    0x08 i32
    0x09 i64
    0x0a f32
    0x0b f64
}

macro_rules! types {
    (
        $($byte:literal $name:ident)*
    ) => {
        $(
            #[allow(non_upper_case_globals)]
            pub const $name: u8 = $byte;
        )*

        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[derive(Debug, Copy, Clone)]
        #[repr(u8)]
        pub enum NativeType {
            $(
                $name = $byte
            ),*
        }

        impl Display for NativeType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$name => write!(f, stringify!($name)),
                    )*
                }
            }
        }

        impl From<NativeType> for u8 {
            fn from(ty: NativeType) -> Self {
                match ty {
                    $(
                        NativeType::$name => $byte,
                    )*
                }
            }
        }


        impl TryFrom<u8> for NativeType {
            type Error = InvalidNativeTypeByte;

            fn try_from(byte: u8) -> Result<Self, Self::Error> {
                match byte {
                    $(
                        $byte => Ok(Self::$name),
                    )*
                    _ => Err(InvalidNativeTypeByte),
                }
            }
        }
    };
}

use types;
