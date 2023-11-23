use std::{
    fmt::Display,
    io::{Cursor, Seek, SeekFrom},
    process::{self},
};

use byteorder::ReadBytesExt;

use crate::msg;

use super::opcode::{self, Opcode};

const INITIAL_STACK_CAPACITY: usize = 512;
const INITIAL_CALLSTACK_CAPACITY: usize = 64;

#[derive(Debug, Clone)]
pub enum Value {
    Uninitialized,
    Unit,
    Num(f64),
    Bool(bool),
}

impl Value {
    fn type_name(&self) -> &str {
        match self {
            Value::Uninitialized => "uninitialized",
            Value::Unit => "unit",
            Value::Num(_) => "number",
            Value::Bool(_) => "boolean",
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Uninitialized => unreachable!(),
            Value::Unit => write!(f, "()"),
            Value::Num(num) => write!(f, "{num}"),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}

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

    pub fn run(&mut self) {
        let first = self.next_opcode();
        let entry_point = self.read_u32() as u64;

        if !matches!(first, opcode::entry_point) {
            msg::error("no entry point defined");
            process::exit(1);
        }

        self.call_fn(entry_point, None);

        while !self.done {
            let opcode = self.next_opcode();
            match opcode {
                opcode::nop => continue,

                opcode::dbg => println!("{}", self.pop()),
                opcode::pop => _ = self.pop(),
                opcode::dup => self.dup(),

                opcode::ld_num_const => self.ld_num_const(),
                opcode::ld_true_const => self.ld_true_const(),
                opcode::ld_false_const => self.ld_false_const(),

                opcode::op_add => self.op_add(),
                opcode::op_sub => self.op_sub(),
                opcode::op_mul => self.op_mul(),
                opcode::op_div => self.op_div(),
                opcode::op_mod => self.op_mod(),
                opcode::op_eq => self.op_eq(),
                opcode::op_ne => self.op_ne(),
                opcode::op_le => self.op_le(),
                opcode::op_lt => self.op_lt(),
                opcode::op_ge => self.op_ge(),
                opcode::op_gt => self.op_gt(),
                opcode::or_and => self.op_and(),
                opcode::or_or => self.op_or(),
                opcode::op_xor => self.op_xor(),

                opcode::op_neg => self.op_neg(),
                opcode::op_not => self.op_not(),

                opcode::ld_loc => self.ld_loc(),
                opcode::st_loc => self.st_loc(),
                
                opcode::jmp => self.jmp(),
                opcode::jmp_if => self.jmp_if(),
                
                opcode::ret => self.ret(),
                opcode::ret_val => self.ret_val(),
                
                opcode::call => self.call(),

                _ => {
                    msg::error("encountered illegal opcode");
                    process::exit(1);
                }
            }
        }
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
            Some(last) => self.push(last.clone()),
            None => {
                msg::error("stack underflow");
                process::exit(1);
            }
        }
    }

    fn ld_num_const(&mut self) {
        let num = self.read_f64();
        self.push(Value::Num(num))
    }

    fn ld_true_const(&mut self) {
        self.push(Value::Bool(true))
    }

    fn ld_false_const(&mut self) {
        self.push(Value::Bool(false))
    }
    
    fn ld_loc(&mut self) {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.push(self.stack[offset + index].clone());
    }

    fn st_loc(&mut self) {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.stack[offset + index] = self.pop();
    }

    fn call(&mut self) {
        let fp = self.read_u32() as u64;
        let back = self.cursor.position();
        self.call_fn(fp, Some(back));
    }

    fn ret(&mut self) {
        self.destroy_frame();
        self.push(Value::Unit);
    }

    fn ret_val(&mut self) {
        let val = self.pop();
        self.destroy_frame();
        self.push(val);
    }
    
    fn jmp(&mut self) {
        let to = self.read_u32() as u64;
        self.cursor.set_position(to);
    }

    fn jmp_if(&mut self) {
        let to = self.read_u32();
        let guard = self.pop();
        match guard {
            Value::Bool(true) => self.cursor.set_position(to as u64),
            Value::Bool(false) => (),
            _ => {
                msg::error(format!("invalid jump condition: {}", guard.type_name()));
                process::exit(1);
            }
        }
    }

    fn call_fn(&mut self, fp: u64, back: Option<u64>) {
        self.cursor.set_position(fp);
        let (param_count, local_count) = self.read_function_header();
        self.create_frame(back, param_count, local_count);

        // push non-parameter locals
        for _ in 0..(local_count - param_count) {
            self.push(Value::Uninitialized);
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

    binary_op! {
        self op_add "+"
        Value::Num(a), Value::Num(b) => Value::Num(a + b)
    }

    binary_op! {
        self op_sub "-"
        Value::Num(a), Value::Num(b) => Value::Num(a - b)
    }

    binary_op! {
        self op_mul "*"
        Value::Num(a), Value::Num(b) => Value::Num(a * b)
    }

    binary_op! {
        self op_div "/"
        Value::Num(a), Value::Num(b) => Value::Num(a / b)
    }

    binary_op! {
        self op_mod "%"
        Value::Num(a), Value::Num(b) => Value::Num(a % b)
    }

    binary_op! {
        self op_eq "=="
        Value::Num(a), Value::Num(b) => Value::Bool(a == b)
        Value::Bool(a), Value::Bool(b) => Value::Bool(a == b)
    }

    binary_op! {
        self op_ne "!="
        Value::Num(a), Value::Num(b) => Value::Bool(a != b)
        Value::Bool(a), Value::Bool(b) => Value::Bool(a != b)
    }

    binary_op! {
        self op_le "<="
        Value::Num(a), Value::Num(b) => Value::Bool(a <= b)
    }

    binary_op! {
        self op_lt "<"
        Value::Num(a), Value::Num(b) => Value::Bool(a < b)
    }

    binary_op! {
        self op_ge ">="
        Value::Num(a), Value::Num(b) => Value::Bool(a >= b)
    }

    binary_op! {
        self op_gt ">"
        Value::Num(a), Value::Num(b) => Value::Bool(a > b)
    }

    binary_op! {
        self op_and "&"
        Value::Bool(a), Value::Bool(b) => Value::Bool(a & b)
    }

    binary_op! {
        self op_or "|"
        Value::Bool(a), Value::Bool(b) => Value::Bool(a | b)
    }

    binary_op! {
        self op_xor "^"
        Value::Bool(a), Value::Bool(b) => Value::Bool(a ^ b)
    }

    unary_op! {
        self op_neg "-"
        Value::Num(f) => Value::Num(-*f)
    }

    unary_op! {
        self op_not "not"
        Value::Bool(b) => Value::Bool(!*b)
    }

    fn next_opcode(&mut self) -> Opcode {
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

    fn read_f64(&mut self) -> f64 {
        self.cursor.read_f64::<byteorder::LE>().unwrap()
    }
}

macro_rules! binary_op {
    (
        $self:ident $name:ident $symbol:literal
        $(
            $a:pat, $b:pat => $r:expr
        )*
    ) => {
        fn $name(&mut $self) {
            let right = $self.pop();
            let left = $self.pop();
            match (&left, &right) {
                $(
                    ($a, $b) => $self.push($r),
                )*
                _ => {
                    msg::error(format!(
                        "invalid operation: {} {} {}",
                        left.type_name(),
                        $symbol,
                        right.type_name()
                    ));
                    process::exit(1);
                }
            }
        }
    };
}

macro_rules! unary_op {
    (
        $self:ident $name:ident $symbol:literal
        $(
            $a:pat => $r:expr
        )*
    ) => {
        fn $name(&mut $self) {
            let value = $self.pop();
            match &value {
                $(
                    $a => $self.push($r),
                )*
                _ => {
                    msg::error(format!(
                        "invalid operation: {} {}",
                        $symbol,
                        value.type_name()
                    ));
                    process::exit(1);
                }
            }
        }
    };
}

use {binary_op, unary_op};
