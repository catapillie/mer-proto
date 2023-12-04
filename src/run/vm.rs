use std::{
    cmp,
    io::{Cursor, Seek, SeekFrom},
    ops::{self},
    process::{self},
};

use super::{opcode, value::Value};
use crate::msg;
use byteorder::ReadBytesExt;

const INITIAL_STACK_CAPACITY: usize = 512;
const INITIAL_CALLSTACK_CAPACITY: usize = 64;

struct Frame {
    back: Option<u64>,
    local_count: u8,
    local_offset: usize,
}

pub struct VM<'a> {
    cursor: Cursor<&'a [u8]>,
    stack: Vec<Value>,
    frames: Vec<Frame>,
    done: bool,
}

impl<'a> VM<'a> {
    pub fn new(program: &'a [u8]) -> Self {
        Self {
            cursor: Cursor::new(program),
            stack: Vec::with_capacity(INITIAL_STACK_CAPACITY),
            frames: Vec::with_capacity(INITIAL_CALLSTACK_CAPACITY),
            done: false,
        }
    }

    fn create_frame(&mut self, back: Option<u64>, param_count: u8, local_count: u8) {
        let local_offset = self.stack.len() - param_count as usize;
        self.frames.push(Frame {
            back,
            local_count,
            local_offset,
        });
    }

    fn destroy_frame(&mut self) {
        let frame = self.frames.pop().unwrap();
        for _ in 0..frame.local_count {
            self.pop();
        }

        let Some(ip) = frame.back else {
            self.halt();
            return;
        };

        self.cursor.set_position(ip);
    }

    pub fn run(&mut self) -> Value {
        let first = self.next_opcode();
        let entry_point = self.read_u32() as u64;

        if !matches!(first, opcode::entry_point) {
            msg::error("no entry point defined");
            process::exit(1);
        }

        self.call_fn(entry_point, None);

        while !self.done {
            match self.next_opcode() {
                opcode::nop => continue,
                opcode::pop => _ = self.pop(),
                opcode::dup => self.dup(),
                opcode::dbg => todo!(),

                opcode::jmp => self.jmp(),
                opcode::jmp_if => self.jmp_if(),
                opcode::ret => self.ret(),
                opcode::call => self.call(),

                opcode::ld_loc => self.ld_loc(),
                opcode::st_loc => self.st_loc(),

                opcode::ld_unit => self.push(Value::make_unit(())),

                opcode::ld_u8 => push_value!(self => read_u8, make_u8),
                opcode::add_u8 => binary_op!(self => get_u8, make_u8, ops::Add::add),
                opcode::sub_u8 => binary_op!(self => get_u8, make_u8, ops::Sub::sub),
                opcode::mul_u8 => binary_op!(self => get_u8, make_u8, ops::Mul::mul),
                opcode::div_u8 => binary_op!(self => get_u8, make_u8, ops::Div::div),
                opcode::rem_u8 => binary_op!(self => get_u8, make_u8, ops::Rem::rem),
                opcode::eq_u8 => binary_op!(self => get_u8, make_bool, cmp::PartialEq::eq),
                opcode::ne_u8 => binary_op!(self => get_u8, make_bool, cmp::PartialEq::ne),
                opcode::le_u8 => binary_op!(self => get_u8, make_bool, cmp::PartialOrd::le),
                opcode::lt_u8 => binary_op!(self => get_u8, make_bool, cmp::PartialOrd::lt),
                opcode::ge_u8 => binary_op!(self => get_u8, make_bool, cmp::PartialOrd::ge),
                opcode::gt_u8 => binary_op!(self => get_u8, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_u8 => binary_op!(self => get_u8, make_u8, ops::BitAnd::bitand),
                opcode::bitor_u8 => binary_op!(self => get_u8, make_u8, ops::BitOr::bitor),
                opcode::bitxor_u8 => binary_op!(self => get_u8, make_u8, ops::BitXor::bitxor),

                opcode::ld_u16 => push_value!(self => read_u16, make_u16),
                opcode::add_u16 => binary_op!(self => get_u16, make_u16, ops::Add::add),
                opcode::sub_u16 => binary_op!(self => get_u16, make_u16, ops::Sub::sub),
                opcode::mul_u16 => binary_op!(self => get_u16, make_u16, ops::Mul::mul),
                opcode::div_u16 => binary_op!(self => get_u16, make_u16, ops::Div::div),
                opcode::rem_u16 => binary_op!(self => get_u16, make_u16, ops::Rem::rem),
                opcode::eq_u16 => binary_op!(self => get_u16, make_bool, cmp::PartialEq::eq),
                opcode::ne_u16 => binary_op!(self => get_u16, make_bool, cmp::PartialEq::ne),
                opcode::le_u16 => binary_op!(self => get_u16, make_bool, cmp::PartialOrd::le),
                opcode::lt_u16 => binary_op!(self => get_u16, make_bool, cmp::PartialOrd::lt),
                opcode::ge_u16 => binary_op!(self => get_u16, make_bool, cmp::PartialOrd::ge),
                opcode::gt_u16 => binary_op!(self => get_u16, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_u16 => binary_op!(self => get_u16, make_u16, ops::BitAnd::bitand),
                opcode::bitor_u16 => binary_op!(self => get_u16, make_u16, ops::BitOr::bitor),
                opcode::bitxor_u16 => binary_op!(self => get_u16, make_u16, ops::BitXor::bitxor),

                opcode::ld_u32 => push_value!(self => read_u32, make_u32),
                opcode::add_u32 => binary_op!(self => get_u32, make_u32, ops::Add::add),
                opcode::sub_u32 => binary_op!(self => get_u32, make_u32, ops::Sub::sub),
                opcode::mul_u32 => binary_op!(self => get_u32, make_u32, ops::Mul::mul),
                opcode::div_u32 => binary_op!(self => get_u32, make_u32, ops::Div::div),
                opcode::rem_u32 => binary_op!(self => get_u32, make_u32, ops::Rem::rem),
                opcode::eq_u32 => binary_op!(self => get_u32, make_bool, cmp::PartialEq::eq),
                opcode::ne_u32 => binary_op!(self => get_u32, make_bool, cmp::PartialEq::ne),
                opcode::le_u32 => binary_op!(self => get_u32, make_bool, cmp::PartialOrd::le),
                opcode::lt_u32 => binary_op!(self => get_u32, make_bool, cmp::PartialOrd::lt),
                opcode::ge_u32 => binary_op!(self => get_u32, make_bool, cmp::PartialOrd::ge),
                opcode::gt_u32 => binary_op!(self => get_u32, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_u32 => binary_op!(self => get_u32, make_u32, ops::BitAnd::bitand),
                opcode::bitor_u32 => binary_op!(self => get_u32, make_u32, ops::BitOr::bitor),
                opcode::bitxor_u32 => binary_op!(self => get_u32, make_u32, ops::BitXor::bitxor),

                opcode::ld_u64 => push_value!(self => read_u64, make_u64),
                opcode::add_u64 => binary_op!(self => get_u64, make_u64, ops::Add::add),
                opcode::sub_u64 => binary_op!(self => get_u64, make_u64, ops::Sub::sub),
                opcode::mul_u64 => binary_op!(self => get_u64, make_u64, ops::Mul::mul),
                opcode::div_u64 => binary_op!(self => get_u64, make_u64, ops::Div::div),
                opcode::rem_u64 => binary_op!(self => get_u64, make_u64, ops::Rem::rem),
                opcode::eq_u64 => binary_op!(self => get_u64, make_bool, cmp::PartialEq::eq),
                opcode::ne_u64 => binary_op!(self => get_u64, make_bool, cmp::PartialEq::ne),
                opcode::le_u64 => binary_op!(self => get_u64, make_bool, cmp::PartialOrd::le),
                opcode::lt_u64 => binary_op!(self => get_u64, make_bool, cmp::PartialOrd::lt),
                opcode::ge_u64 => binary_op!(self => get_u64, make_bool, cmp::PartialOrd::ge),
                opcode::gt_u64 => binary_op!(self => get_u64, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_u64 => binary_op!(self => get_u64, make_u64, ops::BitAnd::bitand),
                opcode::bitor_u64 => binary_op!(self => get_u64, make_u64, ops::BitOr::bitor),
                opcode::bitxor_u64 => binary_op!(self => get_u64, make_u64, ops::BitXor::bitxor),

                opcode::ld_i8 => push_value!(self => read_i8, make_i8),
                opcode::add_i8 => binary_op!(self => get_i8, make_i8, ops::Add::add),
                opcode::sub_i8 => binary_op!(self => get_i8, make_i8, ops::Sub::sub),
                opcode::mul_i8 => binary_op!(self => get_i8, make_i8, ops::Mul::mul),
                opcode::div_i8 => binary_op!(self => get_i8, make_i8, ops::Div::div),
                opcode::rem_i8 => binary_op!(self => get_i8, make_i8, ops::Rem::rem),
                opcode::eq_i8 => binary_op!(self => get_i8, make_bool, cmp::PartialEq::eq),
                opcode::ne_i8 => binary_op!(self => get_i8, make_bool, cmp::PartialEq::ne),
                opcode::le_i8 => binary_op!(self => get_i8, make_bool, cmp::PartialOrd::le),
                opcode::lt_i8 => binary_op!(self => get_i8, make_bool, cmp::PartialOrd::lt),
                opcode::ge_i8 => binary_op!(self => get_i8, make_bool, cmp::PartialOrd::ge),
                opcode::gt_i8 => binary_op!(self => get_i8, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_i8 => binary_op!(self => get_i8, make_i8, ops::BitAnd::bitand),
                opcode::bitor_i8 => binary_op!(self => get_i8, make_i8, ops::BitOr::bitor),
                opcode::bitxor_i8 => binary_op!(self => get_i8, make_i8, ops::BitXor::bitxor),
                opcode::neg_i8 => unary_op!(self => get_i8, make_i8, ops::Neg::neg),

                opcode::ld_i16 => push_value!(self => read_i16, make_i16),
                opcode::add_i16 => binary_op!(self => get_i16, make_i16, ops::Add::add),
                opcode::sub_i16 => binary_op!(self => get_i16, make_i16, ops::Sub::sub),
                opcode::mul_i16 => binary_op!(self => get_i16, make_i16, ops::Mul::mul),
                opcode::div_i16 => binary_op!(self => get_i16, make_i16, ops::Div::div),
                opcode::rem_i16 => binary_op!(self => get_i16, make_i16, ops::Rem::rem),
                opcode::eq_i16 => binary_op!(self => get_i16, make_bool, cmp::PartialEq::eq),
                opcode::ne_i16 => binary_op!(self => get_i16, make_bool, cmp::PartialEq::ne),
                opcode::le_i16 => binary_op!(self => get_i16, make_bool, cmp::PartialOrd::le),
                opcode::lt_i16 => binary_op!(self => get_i16, make_bool, cmp::PartialOrd::lt),
                opcode::ge_i16 => binary_op!(self => get_i16, make_bool, cmp::PartialOrd::ge),
                opcode::gt_i16 => binary_op!(self => get_i16, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_i16 => binary_op!(self => get_i16, make_i16, ops::BitAnd::bitand),
                opcode::bitor_i16 => binary_op!(self => get_i16, make_i16, ops::BitOr::bitor),
                opcode::bitxor_i16 => binary_op!(self => get_i16, make_i16, ops::BitXor::bitxor),
                opcode::neg_i16 => unary_op!(self => get_i16, make_i16, ops::Neg::neg),

                opcode::ld_i32 => push_value!(self => read_i32, make_i32),
                opcode::add_i32 => binary_op!(self => get_i32, make_i32, ops::Add::add),
                opcode::sub_i32 => binary_op!(self => get_i32, make_i32, ops::Sub::sub),
                opcode::mul_i32 => binary_op!(self => get_i32, make_i32, ops::Mul::mul),
                opcode::div_i32 => binary_op!(self => get_i32, make_i32, ops::Div::div),
                opcode::rem_i32 => binary_op!(self => get_i32, make_i32, ops::Rem::rem),
                opcode::eq_i32 => binary_op!(self => get_i32, make_bool, cmp::PartialEq::eq),
                opcode::ne_i32 => binary_op!(self => get_i32, make_bool, cmp::PartialEq::ne),
                opcode::le_i32 => binary_op!(self => get_i32, make_bool, cmp::PartialOrd::le),
                opcode::lt_i32 => binary_op!(self => get_i32, make_bool, cmp::PartialOrd::lt),
                opcode::ge_i32 => binary_op!(self => get_i32, make_bool, cmp::PartialOrd::ge),
                opcode::gt_i32 => binary_op!(self => get_i32, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_i32 => binary_op!(self => get_i32, make_i32, ops::BitAnd::bitand),
                opcode::bitor_i32 => binary_op!(self => get_i32, make_i32, ops::BitOr::bitor),
                opcode::bitxor_i32 => binary_op!(self => get_i32, make_i32, ops::BitXor::bitxor),
                opcode::neg_i32 => unary_op!(self => get_i32, make_i32, ops::Neg::neg),

                opcode::ld_i64 => push_value!(self => read_i64, make_i64),
                opcode::add_i64 => binary_op!(self => get_i64, make_i64, ops::Add::add),
                opcode::sub_i64 => binary_op!(self => get_i64, make_i64, ops::Sub::sub),
                opcode::mul_i64 => binary_op!(self => get_i64, make_i64, ops::Mul::mul),
                opcode::div_i64 => binary_op!(self => get_i64, make_i64, ops::Div::div),
                opcode::rem_i64 => binary_op!(self => get_i64, make_i64, ops::Rem::rem),
                opcode::eq_i64 => binary_op!(self => get_i64, make_bool, cmp::PartialEq::eq),
                opcode::ne_i64 => binary_op!(self => get_i64, make_bool, cmp::PartialEq::ne),
                opcode::le_i64 => binary_op!(self => get_i64, make_bool, cmp::PartialOrd::le),
                opcode::lt_i64 => binary_op!(self => get_i64, make_bool, cmp::PartialOrd::lt),
                opcode::ge_i64 => binary_op!(self => get_i64, make_bool, cmp::PartialOrd::ge),
                opcode::gt_i64 => binary_op!(self => get_i64, make_bool, cmp::PartialOrd::gt),
                opcode::bitand_i64 => binary_op!(self => get_i64, make_i64, ops::BitAnd::bitand),
                opcode::bitor_i64 => binary_op!(self => get_i64, make_i64, ops::BitOr::bitor),
                opcode::bitxor_i64 => binary_op!(self => get_i64, make_i64, ops::BitXor::bitxor),
                opcode::neg_i64 => unary_op!(self => get_i64, make_i64, ops::Neg::neg),

                opcode::ld_f32 => push_value!(self => read_f32, make_f32),
                opcode::add_f32 => binary_op!(self => get_f32, make_f32, ops::Add::add),
                opcode::sub_f32 => binary_op!(self => get_f32, make_f32, ops::Sub::sub),
                opcode::mul_f32 => binary_op!(self => get_f32, make_f32, ops::Mul::mul),
                opcode::div_f32 => binary_op!(self => get_f32, make_f32, ops::Div::div),
                opcode::rem_f32 => binary_op!(self => get_f32, make_f32, ops::Rem::rem),
                opcode::eq_f32 => binary_op!(self => get_f32, make_bool, cmp::PartialEq::eq),
                opcode::ne_f32 => binary_op!(self => get_f32, make_bool, cmp::PartialEq::ne),
                opcode::le_f32 => binary_op!(self => get_f32, make_bool, cmp::PartialOrd::le),
                opcode::lt_f32 => binary_op!(self => get_f32, make_bool, cmp::PartialOrd::lt),
                opcode::ge_f32 => binary_op!(self => get_f32, make_bool, cmp::PartialOrd::ge),
                opcode::gt_f32 => binary_op!(self => get_f32, make_bool, cmp::PartialOrd::gt),
                opcode::neg_f32 => unary_op!(self => get_f32, make_f32, ops::Neg::neg),

                opcode::ld_f64 => push_value!(self => read_f64, make_f64),
                opcode::add_f64 => binary_op!(self => get_f64, make_f64, ops::Add::add),
                opcode::sub_f64 => binary_op!(self => get_f64, make_f64, ops::Sub::sub),
                opcode::mul_f64 => binary_op!(self => get_f64, make_f64, ops::Mul::mul),
                opcode::div_f64 => binary_op!(self => get_f64, make_f64, ops::Div::div),
                opcode::rem_f64 => binary_op!(self => get_f64, make_f64, ops::Rem::rem),
                opcode::eq_f64 => binary_op!(self => get_f64, make_bool, cmp::PartialEq::eq),
                opcode::ne_f64 => binary_op!(self => get_f64, make_bool, cmp::PartialEq::ne),
                opcode::le_f64 => binary_op!(self => get_f64, make_bool, cmp::PartialOrd::le),
                opcode::lt_f64 => binary_op!(self => get_f64, make_bool, cmp::PartialOrd::lt),
                opcode::ge_f64 => binary_op!(self => get_f64, make_bool, cmp::PartialOrd::ge),
                opcode::gt_f64 => binary_op!(self => get_f64, make_bool, cmp::PartialOrd::gt),
                opcode::neg_f64 => unary_op!(self => get_f64, make_f64, ops::Neg::neg),

                opcode::ld_bool_false => self.push(Value::make_bool(false)),
                opcode::ld_bool_true => self.push(Value::make_bool(true)),
                opcode::eq_bool => binary_op!(self => get_bool, make_bool, cmp::PartialEq::eq),
                opcode::ne_bool => binary_op!(self => get_bool, make_bool, cmp::PartialEq::ne),
                opcode::and_bool => binary_op!(self => get_bool, make_bool, ops::BitAnd::bitand),
                opcode::or_bool => binary_op!(self => get_bool, make_bool, ops::BitOr::bitor),
                opcode::xor_bool => binary_op!(self => get_bool, make_bool, ops::BitXor::bitxor),
                opcode::not_bool => unary_op!(self => get_bool, make_bool, ops::Not::not),

                _ => {
                    msg::error("encountered illegal opcode");
                    process::exit(1);
                }
            }
        }

        self.pop()
    }

    fn halt(&mut self) {
        while !self.frames.is_empty() {
            self.destroy_frame();
        }
        if !self.stack.is_empty() {
            msg::warn("stack remained non-empty after halt opcode");
        }
        self.done = true;
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(value) => value,
            None => {
                msg::error("stack underflow");
                process::exit(1);
            }
        }
    }

    fn dup(&mut self) {
        match self.stack.last() {
            Some(last) => self.push(*last),
            None => {
                msg::error("stack underflow");
                process::exit(1);
            }
        }
    }

    fn jmp(&mut self) {
        let to = self.read_u32() as u64;
        self.cursor.set_position(to);
    }

    fn jmp_if(&mut self) {
        let to = self.read_u32();
        let guard = self.pop().get_bool();
        if guard {
            self.cursor.set_position(to as u64);
        }
    }

    fn ret(&mut self) {
        let val = self.pop();
        self.destroy_frame();
        self.push(val);
    }

    fn ld_loc(&mut self) {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.push(self.stack[offset + index]);
    }

    fn call(&mut self) {
        let fp = self.read_u32() as u64;
        let back = self.cursor.position();
        self.call_fn(fp, Some(back));
    }

    fn call_fn(&mut self, fp: u64, back: Option<u64>) {
        self.cursor.set_position(fp);
        let (param_count, local_count) = self.read_function_header();
        self.create_frame(back, param_count, local_count);

        // push non-parameter locals
        for _ in 0..(local_count - param_count) {
            self.push(Value::make_u8(0)); // uninitialized
        }
    }

    // returns (param_count, local_count)
    fn read_function_header(&mut self) -> (u8, u8) {
        if !matches!(self.next_opcode(), opcode::function) {
            msg::error("jumped to invalid function");
            process::exit(1);
        }

        let n = self.read_u16() as usize;
        self.cursor.seek(SeekFrom::Current(n as i64)).unwrap();

        let param_count = self.read_u8();
        let local_count = self.read_u8();

        (param_count, local_count)
    }

    fn st_loc(&mut self) {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.stack[offset + index] = self.pop();
    }

    fn next_opcode(&mut self) -> u8 {
        self.read_u8()
    }

    fn read_u8(&mut self) -> u8 {
        self.cursor.read_u8().unwrap()
    }

    fn read_u16(&mut self) -> u16 {
        self.cursor.read_u16::<byteorder::LE>().unwrap()
    }

    fn read_u32(&mut self) -> u32 {
        self.cursor.read_u32::<byteorder::LE>().unwrap()
    }

    fn read_u64(&mut self) -> u64 {
        self.cursor.read_u64::<byteorder::LE>().unwrap()
    }

    fn read_i8(&mut self) -> i8 {
        self.cursor.read_i8().unwrap()
    }

    fn read_i16(&mut self) -> i16 {
        self.cursor.read_i16::<byteorder::LE>().unwrap()
    }

    fn read_i32(&mut self) -> i32 {
        self.cursor.read_i32::<byteorder::LE>().unwrap()
    }

    fn read_i64(&mut self) -> i64 {
        self.cursor.read_i64::<byteorder::LE>().unwrap()
    }

    fn read_f32(&mut self) -> f32 {
        self.cursor.read_f32::<byteorder::LE>().unwrap()
    }

    fn read_f64(&mut self) -> f64 {
        self.cursor.read_f64::<byteorder::LE>().unwrap()
    }
}

macro_rules! push_value {
    ($self:ident => $read_fn:ident, $make_fn:ident) => {{
        let value = $self.$read_fn();
        $self.push(Value::$make_fn(value));
    }};
}

macro_rules! binary_op {
    ($self:ident => $get_fn:ident, $make_fn:ident, $op_fn:path) => {{
        let b = $self.pop().$get_fn();
        let a = $self.pop().$get_fn();
        $self.push(Value::$make_fn($op_fn(&a, &b)));
    }};
}

macro_rules! unary_op {
    ($self:ident => $get_fn:ident, $make_fn:ident, $op_fn:path) => {{
        let e = $self.pop().$get_fn();
        $self.push(Value::$make_fn($op_fn(&e)));
    }};
}

use binary_op;
use push_value;
use unary_op;
