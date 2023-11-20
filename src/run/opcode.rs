pub type Opcode = u8;

// unstable opcode values, may change (a lot)
opcodes! {
    0x00 nop,
    0x11 ld_num_const, // f64
    0x12 ld_true_const,
    0x13 ld_false_const,
    0x20 op_add,
    0x21 op_sub,
    0x22 op_mul,
    0x23 op_div,
    0x24 op_mod,
    0x25 op_eq,
    0x26 op_ne,
    0x27 op_le,
    0x28 op_lt,
    0x29 op_ge,
    0x2a op_gt,
    0x2b op_amp,
    0x2c op_bar,
    0x2d op_car,
    0x30 op_neg,
    0x31 op_not,
    0x40 ld_loc, // u8
    0x41 st_loc, // u8
    0x42 pop,
    0x43 dup,
    0x50 jmp, // u32
    0x51 jmp_if, // u32
    0x60 ret,
    0x61 ret_val,
    0x90 dbg,
    0xa0 function, // n: u16, name: [n]u8, param_count: u8, local_count: u8
    0xa1 call, // fp: u32
    0xf0 entry_point, // ip: u32
}

macro_rules! opcodes {
    (
        $($byte:literal $name:ident),* $(,)?
    ) => {
        $(
            #[allow(non_upper_case_globals)]
            pub const $name: u8 = $byte;
        )*

        pub fn name(opcode: u8) -> Option<&'static str> {
            match opcode {
                $(
                    $byte => Some(stringify!($name)),
                )*
                _ => None
            }
        }
    };
}

use opcodes;
