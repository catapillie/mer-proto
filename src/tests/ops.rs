use crate::com;

#[test]
fn can_parse_all_operations() {
    let success = com::analyse("<source>", SOURCE.to_string()).is_some();
    assert!(success);
}

const SOURCE: &str = "
func add_u8(x: u8, y: u8) -> u8 = x + y
func sub_u8(x: u8, y: u8) -> u8 = x - y
func mul_u8(x: u8, y: u8) -> u8 = x * y
func div_u8(x: u8, y: u8) -> u8 = x / y
func rem_u8(x: u8, y: u8) -> u8 = x % y
func bitand_u8(x: u8, y: u8) -> u8 = x & y
func bitxor_u8(x: u8, y: u8) -> u8 = x ^ y
func bitor_u8(x: u8, y: u8) -> u8 = x | y
func eq_u8(x: u8, y: u8) -> bool = x == y
func ne_u8(x: u8, y: u8) -> bool = x != y
func ge_u8(x: u8, y: u8) -> bool = x >= y
func gt_u8(x: u8, y: u8) -> bool = x > y
func le_u8(x: u8, y: u8) -> bool = x <= y
func lt_u8(x: u8, y: u8) -> bool = x < y
func pos_u8(x: u8) -> u8 = +x

func add_u16(x: u16, y: u16) -> u16 = x + y
func sub_u16(x: u16, y: u16) -> u16 = x - y
func mul_u16(x: u16, y: u16) -> u16 = x * y
func div_u16(x: u16, y: u16) -> u16 = x / y
func rem_u16(x: u16, y: u16) -> u16 = x % y
func bitand_u16(x: u16, y: u16) -> u16 = x & y
func bitxor_u16(x: u16, y: u16) -> u16 = x ^ y
func bitor_u16(x: u16, y: u16) -> u16 = x | y
func eq_u16(x: u16, y: u16) -> bool = x == y
func ne_u16(x: u16, y: u16) -> bool = x != y
func ge_u16(x: u16, y: u16) -> bool = x >= y
func gt_u16(x: u16, y: u16) -> bool = x > y
func le_u16(x: u16, y: u16) -> bool = x <= y
func lt_u16(x: u16, y: u16) -> bool = x < y
func pos_u16(x: i16) -> i16 = +x

func add_u32(x: u32, y: u32) -> u32 = x + y
func sub_u32(x: u32, y: u32) -> u32 = x - y
func mul_u32(x: u32, y: u32) -> u32 = x * y
func div_u32(x: u32, y: u32) -> u32 = x / y
func rem_u32(x: u32, y: u32) -> u32 = x % y
func bitand_u32(x: u32, y: u32) -> u32 = x & y
func bitxor_u32(x: u32, y: u32) -> u32 = x ^ y
func bitor_u32(x: u32, y: u32) -> u32 = x | y
func eq_u32(x: u32, y: u32) -> bool = x == y
func ne_u32(x: u32, y: u32) -> bool = x != y
func ge_u32(x: u32, y: u32) -> bool = x >= y
func gt_u32(x: u32, y: u32) -> bool = x > y
func le_u32(x: u32, y: u32) -> bool = x <= y
func lt_u32(x: u32, y: u32) -> bool = x < y
func pos_u32(x: u32) -> u32 = +x

func add_u64(x: u64, y: u64) -> u64 = x + y
func sub_u64(x: u64, y: u64) -> u64 = x - y
func mul_u64(x: u64, y: u64) -> u64 = x * y
func div_u64(x: u64, y: u64) -> u64 = x / y
func rem_u64(x: u64, y: u64) -> u64 = x % y
func bitand_u64(x: u64, y: u64) -> u64 = x & y
func bitxor_u64(x: u64, y: u64) -> u64 = x ^ y
func bitor_u64(x: u64, y: u64) -> u64 = x | y
func eq_u64(x: u64, y: u64) -> bool = x == y
func ne_u64(x: u64, y: u64) -> bool = x != y
func ge_u64(x: u64, y: u64) -> bool = x >= y
func gt_u64(x: u64, y: u64) -> bool = x > y
func le_u64(x: u64, y: u64) -> bool = x <= y
func lt_u64(x: u64, y: u64) -> bool = x < y
func pos_u64(x: u64) -> u64 = +x

func add_i8(x: i8, y: i8) -> i8 = x + y
func sub_i8(x: i8, y: i8) -> i8 = x - y
func mul_i8(x: i8, y: i8) -> i8 = x * y
func div_i8(x: i8, y: i8) -> i8 = x / y
func rem_i8(x: i8, y: i8) -> i8 = x % y
func bitand_i8(x: i8, y: i8) -> i8 = x & y
func bitxor_i8(x: i8, y: i8) -> i8 = x ^ y
func bitor_i8(x: i8, y: i8) -> i8 = x | y
func eq_i8(x: i8, y: i8) -> bool = x == y
func ne_i8(x: i8, y: i8) -> bool = x != y
func ge_i8(x: i8, y: i8) -> bool = x >= y
func gt_i8(x: i8, y: i8) -> bool = x > y
func le_i8(x: i8, y: i8) -> bool = x <= y
func lt_i8(x: i8, y: i8) -> bool = x < y
func pos_i8(x: i8) -> i8 = +x
func neg_i8(x: i8) -> i8 = -x

func add_i16(x: i16, y: i16) -> i16 = x + y
func sub_i16(x: i16, y: i16) -> i16 = x - y
func mul_i16(x: i16, y: i16) -> i16 = x * y
func div_i16(x: i16, y: i16) -> i16 = x / y
func rem_i16(x: i16, y: i16) -> i16 = x % y
func bitand_i16(x: i16, y: i16) -> i16 = x & y
func bitxor_i16(x: i16, y: i16) -> i16 = x ^ y
func bitor_i16(x: i16, y: i16) -> i16 = x | y
func eq_i16(x: i16, y: i16) -> bool = x == y
func ne_i16(x: i16, y: i16) -> bool = x != y
func ge_i16(x: i16, y: i16) -> bool = x >= y
func gt_i16(x: i16, y: i16) -> bool = x > y
func le_i16(x: i16, y: i16) -> bool = x <= y
func lt_i16(x: i16, y: i16) -> bool = x < y
func pos_i16(x: i16) -> i16 = +x
func neg_i16(x: i16) -> i16 = -x

func add_i32(x: i32, y: i32) -> i32 = x + y
func sub_i32(x: i32, y: i32) -> i32 = x - y
func mul_i32(x: i32, y: i32) -> i32 = x * y
func div_i32(x: i32, y: i32) -> i32 = x / y
func rem_i32(x: i32, y: i32) -> i32 = x % y
func bitand_i32(x: i32, y: i32) -> i32 = x & y
func bitxor_i32(x: i32, y: i32) -> i32 = x ^ y
func bitor_i32(x: i32, y: i32) -> i32 = x | y
func eq_i32(x: i32, y: i32) -> bool = x == y
func ne_i32(x: i32, y: i32) -> bool = x != y
func ge_i32(x: i32, y: i32) -> bool = x >= y
func gt_i32(x: i32, y: i32) -> bool = x > y
func le_i32(x: i32, y: i32) -> bool = x <= y
func lt_i32(x: i32, y: i32) -> bool = x < y
func pos_i32(x: i32) -> i32 = +x
func neg_i32(x: i32) -> i32 = -x

func add_i64(x: i64, y: i64) -> i64 = x + y
func sub_i64(x: i64, y: i64) -> i64 = x - y
func mul_i64(x: i64, y: i64) -> i64 = x * y
func div_i64(x: i64, y: i64) -> i64 = x / y
func rem_i64(x: i64, y: i64) -> i64 = x % y
func bitand_i64(x: i64, y: i64) -> i64 = x & y
func bitxor_i64(x: i64, y: i64) -> i64 = x ^ y
func bitor_i64(x: i64, y: i64) -> i64 = x | y
func eq_i64(x: i64, y: i64) -> bool = x == y
func ne_i64(x: i64, y: i64) -> bool = x != y
func ge_i64(x: i64, y: i64) -> bool = x >= y
func gt_i64(x: i64, y: i64) -> bool = x > y
func le_i64(x: i64, y: i64) -> bool = x <= y
func lt_i64(x: i64, y: i64) -> bool = x < y
func pos_i64(x: i64) -> i64 = +x
func neg_i64(x: i64) -> i64 = -x

func add_f32(x: f32, y: f32) -> f32 = x + y
func sub_f32(x: f32, y: f32) -> f32 = x - y
func mul_f32(x: f32, y: f32) -> f32 = x * y
func div_f32(x: f32, y: f32) -> f32 = x / y
func rem_f32(x: f32, y: f32) -> f32 = x % y
func eq_f32(x: f32, y: f32) -> bool = x == y
func ne_f32(x: f32, y: f32) -> bool = x != y
func ge_f32(x: f32, y: f32) -> bool = x >= y
func gt_f32(x: f32, y: f32) -> bool = x > y
func le_f32(x: f32, y: f32) -> bool = x <= y
func lt_f32(x: f32, y: f32) -> bool = x < y
func pos_f32(x: f32) -> f32 = +x
func neg_f32(x: f32) -> f32 = -x

func add_f64(x: f64, y: f64) -> f64 = x + y
func sub_f64(x: f64, y: f64) -> f64 = x - y
func mul_f64(x: f64, y: f64) -> f64 = x * y
func div_f64(x: f64, y: f64) -> f64 = x / y
func rem_f64(x: f64, y: f64) -> f64 = x % y
func eq_f64(x: f64, y: f64) -> bool = x == y
func ne_f64(x: f64, y: f64) -> bool = x != y
func ge_f64(x: f64, y: f64) -> bool = x >= y
func gt_f64(x: f64, y: f64) -> bool = x > y
func le_f64(x: f64, y: f64) -> bool = x <= y
func lt_f64(x: f64, y: f64) -> bool = x < y
func pos_f64(x: f64) -> f64 = +x
func neg_f64(x: f64) -> f64 = -x

func eq_bool(a: bool, b: bool) -> bool = a == b
func ne_bool(a: bool, b: bool) -> bool = a != b
func bitand_bool(a: bool, b: bool) -> bool = a & b
func bitor_bool(a: bool, b: bool) -> bool = a | b
func bitxor_bool(a: bool, b: bool) -> bool = a ^ b
func and_bool(a: bool, b: bool) -> bool = a and b
func or_bool(a: bool, b: bool) -> bool = a or b
func xor_bool(a: bool, b: bool) -> bool = a xor b
func not_bool(b: bool) -> bool = not b

return
";
