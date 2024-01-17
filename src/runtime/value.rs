#[derive(Copy, Clone)]
pub union Value {
    unit: (),
    bool: bool,
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    usize: usize,
}

impl Value {
    init_fn! { make_unit(unit: ()) }
    init_fn! { make_bool(bool: bool) }
    init_fn! { make_u8(u8: u8) }
    init_fn! { make_u16(u16: u16) }
    init_fn! { make_u32(u32: u32) }
    init_fn! { make_u64(u64: u64) }
    init_fn! { make_i8(i8: i8) }
    init_fn! { make_i16(i16: i16) }
    init_fn! { make_i32(i32: i32) }
    init_fn! { make_i64(i64: i64) }
    init_fn! { make_f32(f32: f32) }
    init_fn! { make_f64(f64: f64) }
    init_fn! { make_usize(usize: usize) }

    getter_fn! { get_unit(unit: ()) }
    getter_fn! { get_bool(bool: bool) }
    getter_fn! { get_u8(u8: u8) }
    getter_fn! { get_u16(u16: u16) }
    getter_fn! { get_u32(u32: u32) }
    getter_fn! { get_u64(u64: u64) }
    getter_fn! { get_i8(i8: i8) }
    getter_fn! { get_i16(i16: i16) }
    getter_fn! { get_i32(i32: i32) }
    getter_fn! { get_i64(i64: i64) }
    getter_fn! { get_f32(f32: f32) }
    getter_fn! { get_f64(f64: f64) }
    getter_fn! { get_usize(usize: usize) }
}

impl_from! {
    () => get_unit
    bool => get_bool
    u8 => get_u8
    u16 => get_u16
    u32 => get_u32
    u64 => get_u64
    i8 => get_i8
    i16 => get_i16
    i32 => get_i32
    i64 => get_i64
    f32 => get_f32
    f64 => get_f64
}

macro_rules! init_fn {
    ($fn_name:ident($name:ident: $type:ty)) => {
        pub fn $fn_name($name: $type) -> Self {
            Self { $name }
        }
    };
}

macro_rules! getter_fn {
    ($fn_name:ident($name:ident: $type:ty)) => {
        pub fn $fn_name(&self) -> $type {
            unsafe { self.$name }
        }
    };
}

macro_rules! impl_from {
    ($($t:ty => $get_fn:ident)*) => {
        $(
            impl From<Value> for $t {
                fn from(value: Value) -> Self {
                    value.$get_fn()
                }
            }
        )*
    };
}

use {getter_fn, impl_from, init_fn};
