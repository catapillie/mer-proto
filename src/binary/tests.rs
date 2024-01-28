#![cfg(test)]

use std::{collections::HashSet, io::Cursor};

use colored::Colorize;

use super::{read_opcode, write_opcode};
use crate::runtime::{
    native_type::NativeType,
    opcode::{all_opcode_bytes, name_of, Opcode},
};

// this test will fail if not all opcode variants are handled at least once
#[test]
fn write_then_read_match() {
    let opcodes = [
        Opcode::nop,
        Opcode::pop,
        Opcode::pop_n(1),
        Opcode::pop_n(20),
        Opcode::pop_n(82),
        Opcode::dup,
        Opcode::dup_n(1),
        Opcode::dup_n(20),
        Opcode::dup_n(82),
        Opcode::dbg(NativeType::bool),
        Opcode::dbg(NativeType::u16),
        Opcode::dbg(NativeType::i32),
        Opcode::dbg(NativeType::f64),
        Opcode::dbg(NativeType::unit),
        Opcode::jmp(0),
        Opcode::jmp(654),
        Opcode::jmp_if(12),
        Opcode::jmp_if(99999),
        Opcode::ret,
        Opcode::call(5),
        Opcode::call(18),
        Opcode::call_addr,
        Opcode::ld_loc(0),
        Opcode::ld_loc(1),
        Opcode::ld_loc(2),
        Opcode::ld_loc_n(0, 1),
        Opcode::ld_loc_n(1, 2),
        Opcode::ld_loc_n(2, 3),
        Opcode::st_loc(0),
        Opcode::st_loc(1),
        Opcode::st_loc(2),
        Opcode::st_loc_n(0, 1),
        Opcode::st_loc_n(1, 2),
        Opcode::st_loc_n(2, 3),
        Opcode::alloc,
        Opcode::alloc_n(1),
        Opcode::alloc_n(8),
        Opcode::alloc_n(128),
        Opcode::ld_heap,
        Opcode::ld_heap_n(1),
        Opcode::ld_heap_n(8),
        Opcode::ld_heap_n(128),
        Opcode::st_heap,
        Opcode::st_heap_n(1),
        Opcode::st_heap_n(8),
        Opcode::st_heap_n(128),
        Opcode::realloc_loc(0),
        Opcode::realloc_loc(255),
        Opcode::realloc_loc(19),
        Opcode::entry_point(5),
        Opcode::entry_point(4581),
        Opcode::function("hello_world".to_string(), 2, 2),
        Opcode::function("hello_world".to_string(), 10, 15),
        Opcode::function("hello_world".to_string(), 0, 3),
        Opcode::ld_u8(50),
        Opcode::ld_u16(50),
        Opcode::ld_u32(50),
        Opcode::ld_u64(50),
        Opcode::ld_i8(50),
        Opcode::ld_i16(50),
        Opcode::ld_i32(50),
        Opcode::ld_i64(50),
        Opcode::ld_f32(-64.0),
        Opcode::ld_f64(4885.0054),
        Opcode::ld_unit,
        Opcode::add(NativeType::i16),
        Opcode::add(NativeType::bool),
        Opcode::sub(NativeType::i16),
        Opcode::sub(NativeType::bool),
        Opcode::mul(NativeType::i16),
        Opcode::mul(NativeType::bool),
        Opcode::div(NativeType::i16),
        Opcode::div(NativeType::bool),
        Opcode::rem(NativeType::i16),
        Opcode::rem(NativeType::bool),
        Opcode::eq(NativeType::i16),
        Opcode::eq(NativeType::bool),
        Opcode::ne(NativeType::i16),
        Opcode::ne(NativeType::bool),
        Opcode::le(NativeType::i16),
        Opcode::le(NativeType::bool),
        Opcode::lt(NativeType::i16),
        Opcode::lt(NativeType::bool),
        Opcode::ge(NativeType::i16),
        Opcode::ge(NativeType::bool),
        Opcode::gt(NativeType::i16),
        Opcode::gt(NativeType::bool),
        Opcode::bitand(NativeType::i16),
        Opcode::bitand(NativeType::bool),
        Opcode::bitor(NativeType::i16),
        Opcode::bitor(NativeType::bool),
        Opcode::bitxor(NativeType::i16),
        Opcode::bitxor(NativeType::bool),
        Opcode::neg(NativeType::i16),
        Opcode::neg(NativeType::bool),
        Opcode::todo,
        Opcode::unreachable,
    ];

    let all_opcode_bytes = HashSet::<_>::from_iter(all_opcode_bytes());
    let mut tested_opcode_bytes = HashSet::new();

    for opcode_in in opcodes {
        let mut cursor = Cursor::new(vec![]);
        write_opcode(&mut cursor, &opcode_in).expect("error when writing");

        let buf = cursor.into_inner();
        let mut cursor = Cursor::new(buf.as_slice());
        let opcode_out = read_opcode(&mut cursor).expect("error when reading");

        assert_eq!(
            opcode_in, opcode_out,
            "writing then reading yielded a different opcode"
        );
        tested_opcode_bytes.insert(opcode_in.byte());
    }

    // ensure all variants were tested at least once
    for byte in all_opcode_bytes {
        assert!(
            tested_opcode_bytes.contains(&byte),
            "missing test case for opcode::{}",
            name_of(byte).unwrap().bold().underline()
        );
    }
}
