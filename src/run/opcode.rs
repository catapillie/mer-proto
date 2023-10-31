opcodes! {
    000 nop,

    001 ld_num_const,
    002 ld_true_const,
    003 ld_false_const,

    004 op_add,
    005 op_sub,
    006 op_mul,
    007 op_div,
    008 op_mod,
    009 op_eq,
    010 op_ne,
    011 op_le,
    012 op_lt,
    013 op_ge,
    014 op_gt,
    015 op_amp,
    016 op_bar,
    017 op_car,

    255 halt,
}

macro_rules! opcodes {
    (
        $($byte:literal $name:ident),* $(,)?
    ) => {
        #[repr(u8)]
        #[allow(non_camel_case_types)]
        #[derive(Debug)]
        pub enum Opcode {
            $(
                #[allow(clippy::zero_prefixed_literal)]
                $name = $byte
            ),*
        }

        impl TryFrom<u8> for Opcode {
            type Error = ();

            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value {
                    $(
                        $byte => Ok(Self::$name),
                    )*
                    _ => Err(()),
                }
            }
        }
    };
}

use opcodes;
