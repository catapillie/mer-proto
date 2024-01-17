use std::{
    cmp,
    io::{Cursor, Seek, SeekFrom},
    ops::{self},
};

use super::{error::Error, native_type, opcode, value::Value};
use byteorder::ReadBytesExt;

const INITIAL_STACK_CAPACITY: usize = 512;
const INITIAL_CALLSTACK_CAPACITY: usize = 64;
const HEAP_SIZE: usize = 1_000_000; // 8 MB

struct Frame {
    back: Option<u64>,
    local_count: u8,
    local_offset: usize,
}

pub struct VM<'a> {
    cursor: Cursor<&'a [u8]>,
    stack: Vec<Value>,
    heap: Slab<Value>,
    frames: Vec<Frame>,
    done: bool,
}

impl<'a> VM<'a> {
    pub fn new(program: &'a [u8]) -> Self {
        Self {
            cursor: Cursor::new(program),
            stack: Vec::with_capacity(INITIAL_STACK_CAPACITY),
            heap: Slab::with_capacity(HEAP_SIZE),
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

    fn destroy_frame(&mut self) -> Result<(), Error> {
        let frame = self.frames.pop().unwrap();
        for _ in 0..frame.local_count {
            self.pop()?;
        }

        let Some(ip) = frame.back else {
            self.halt()?;
            return Ok(());
        };

        self.cursor.set_position(ip);
        Ok(())
    }

    pub fn run<T>(mut self) -> Result<T, Error>
    where
        T: From<Value>,
    {
        let first = self.next_opcode();
        let entry_point = self.read_u32() as u64;

        if !matches!(first, opcode::entry_point) {
            return Err(Error::NoEntryPoint);
        }

        self.call_fn(entry_point, None)?;

        while !self.done {
            match self.next_opcode() {
                opcode::nop => continue,
                opcode::pop => _ = self.pop(),
                opcode::dup => self.dup()?,

                opcode::dbg => {
                    self.dup()?;
                    let value = self.pop()?;
                    match self.read_u8() {
                        native_type::unit => println!("{:?}", value.get_unit()),
                        native_type::bool => println!("{}", value.get_bool()),
                        native_type::u8 => println!("{}", value.get_u8()),
                        native_type::u16 => println!("{}", value.get_u16()),
                        native_type::u32 => println!("{}", value.get_u32()),
                        native_type::u64 => println!("{}", value.get_u64()),
                        native_type::i8 => println!("{}", value.get_i8()),
                        native_type::i16 => println!("{}", value.get_i16()),
                        native_type::i32 => println!("{}", value.get_i32()),
                        native_type::i64 => println!("{}", value.get_i64()),
                        native_type::f32 => println!("{}", value.get_f32()),
                        native_type::f64 => println!("{}", value.get_f64()),
                        _ => return Err(Error::InvalidNativeType),
                    }
                }

                opcode::jmp => self.jmp(),
                opcode::jmp_if => self.jmp_if()?,
                opcode::ret => self.ret()?,
                opcode::call => self.call()?,

                opcode::ld_loc => self.ld_loc(),
                opcode::st_loc => self.st_loc()?,

                opcode::ld_unit => self.push(Value::make_unit(())),
                opcode::ld_u8 => push_value!(self => read_u8, make_u8),
                opcode::ld_u16 => push_value!(self => read_u16, make_u16),
                opcode::ld_u32 => push_value!(self => read_u32, make_u32),
                opcode::ld_u64 => push_value!(self => read_u64, make_u64),
                opcode::ld_i8 => push_value!(self => read_i8, make_i8),
                opcode::ld_i16 => push_value!(self => read_i16, make_i16),
                opcode::ld_i32 => push_value!(self => read_i32, make_i32),
                opcode::ld_i64 => push_value!(self => read_i64, make_i64),
                opcode::ld_f32 => push_value!(self => read_f32, make_f32),
                opcode::ld_f64 => push_value!(self => read_f64, make_f64),

                opcode::add => numeral_binary_op!(self => ops::Add::add, add),
                opcode::sub => numeral_binary_op!(self => ops::Sub::sub, sub),
                opcode::mul => numeral_binary_op!(self => ops::Mul::mul, mul),
                opcode::div => numeral_binary_op!(self => ops::Div::div, div),
                opcode::rem => numeral_binary_op!(self => ops::Rem::rem, rem),
                opcode::eq => comparison_binary_op!(self => cmp::PartialEq::eq, eq),
                opcode::ne => comparison_binary_op!(self => cmp::PartialEq::ne, ne),
                opcode::le => comparison_binary_op!(self => cmp::PartialOrd::le, le),
                opcode::lt => comparison_binary_op!(self => cmp::PartialOrd::lt, lt),
                opcode::ge => comparison_binary_op!(self => cmp::PartialOrd::ge, ge),
                opcode::gt => comparison_binary_op!(self => cmp::PartialOrd::gt, gt),
                opcode::bitand => bitwise_binary_op!(self => ops::BitAnd::bitand, bitand),
                opcode::bitor => bitwise_binary_op!(self => ops::BitOr::bitor, bitor),
                opcode::bitxor => bitwise_binary_op!(self => ops::BitXor::bitxor, bitxor),

                opcode::alloc => {
                    let value = self.pop()?;
                    let ptr = self.heap.insert(value);
                    self.push(Value::make_usize(ptr));
                }
                opcode::ld_heap => {
                    let ptr = self.pop()?.get_usize();
                    let value = match self.heap.get(ptr) {
                        Some(value) => value,
                        None => return Err(Error::InvalidMemoryAccess),
                    };
                    self.push(*value);
                }
                opcode::st_heap => {
                    let ptr = self.pop()?.get_usize();
                    let value = self.pop()?;
                    let heap_value = match self.heap.get_mut(ptr) {
                        Some(heap_value) => heap_value,
                        None => return Err(Error::InvalidMemoryWrite),
                    };
                    *heap_value = value;
                }

                opcode::neg => {
                    let ty = self.read_u8();
                    let value = self.pop()?;

                    let result = match ty {
                        native_type::bool => Value::make_bool(!value.get_bool()),
                        native_type::i8 => Value::make_i8(-value.get_i8()),
                        native_type::i16 => Value::make_i16(-value.get_i16()),
                        native_type::i32 => Value::make_i32(-value.get_i32()),
                        native_type::i64 => Value::make_i64(-value.get_i64()),
                        native_type::f32 => Value::make_f32(-value.get_f32()),
                        native_type::f64 => Value::make_f64(-value.get_f64()),
                        _ => return Err(Error::InvalidUnaryOperation),
                    };
                    self.push(result);
                }

                _ => return Err(Error::IllegalOpcode),
            }
        }

        Ok(self.pop()?.into())
    }

    fn halt(&mut self) -> Result<(), Error> {
        while !self.frames.is_empty() {
            self.destroy_frame()?;
        }
        if !self.stack.is_empty() {
            return Err(Error::HaltWithNonEmptyStack);
        }
        self.done = true;
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, Error> {
        match self.stack.pop() {
            Some(value) => Ok(value),
            None => Err(Error::StackUnderflow),
        }
    }

    fn dup(&mut self) -> Result<(), Error> {
        match self.stack.last() {
            Some(last) => {
                self.push(*last);
                Ok(())
            },
            None => Err(Error::StackUnderflow),
        }
    }

    fn jmp(&mut self) {
        let to = self.read_u32() as u64;
        self.cursor.set_position(to);
    }

    fn jmp_if(&mut self) -> Result<(), Error> {
        let to = self.read_u32();
        let guard = self.pop()?.get_bool();
        if guard {
            self.cursor.set_position(to as u64);
        }
        Ok(())
    }

    fn ret(&mut self) -> Result<(), Error> {
        let val = self.pop()?;
        self.destroy_frame()?;
        self.push(val);
        Ok(())
    }

    fn call(&mut self) -> Result<(), Error> {
        let fp = self.read_u32() as u64;
        let back = self.cursor.position();
        self.call_fn(fp, Some(back))?;
        Ok(())
    }

    fn call_fn(&mut self, fp: u64, back: Option<u64>) -> Result<(), Error> {
        self.cursor.set_position(fp);
        let (param_count, local_count) = self.read_function_header()?;
        self.create_frame(back, param_count, local_count);

        // push non-parameter locals
        for _ in 0..(local_count - param_count) {
            self.push(Value::make_u8(0)); // uninitialized
        }

        Ok(())
    }

    // returns (param_count, local_count)
    fn read_function_header(&mut self) -> Result<(u8, u8), Error> {
        if !matches!(self.next_opcode(), opcode::function) {
            return Err(Error::InvalidFunctionCall);
        }

        let n = self.read_u16() as usize;
        self.cursor.seek(SeekFrom::Current(n as i64)).unwrap();

        let param_count = self.read_u8();
        let local_count = self.read_u8();

        Ok((param_count, local_count))
    }

    fn ld_loc(&mut self) {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.push(self.stack[offset + index]);
    }

    fn st_loc(&mut self) -> Result<(), Error> {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.stack[offset + index] = self.pop()?;
        Ok(())
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
    (
        ($self:ident, $op_fn:path, $op_name:ident)
        $(($type:path, $get_fn:ident, $make_fn:ident))*
    ) => {
        {
            match $self.read_u8() {
                $(
                    $type => {
                        let b = $self.pop()?.$get_fn();
                        let a = $self.pop()?.$get_fn();
                        $self.push(Value::$make_fn($op_fn(&a, &b)));
                    },
                )*
                _ => {
                    return Err(Error::InvalidBinaryOperation)
                }
            }
        }
    };
}

macro_rules! numeral_binary_op {
    (
        $self:ident => $op_trait:path, $op_name:ident
    ) => {
        binary_op! { ($self, $op_trait, $op_name)
            (native_type::u8, get_u8, make_u8)
            (native_type::u16, get_u16, make_u16)
            (native_type::u32, get_u32, make_u32)
            (native_type::u64, get_u64, make_u64)
            (native_type::i8, get_i8, make_i8)
            (native_type::i16, get_i16, make_i16)
            (native_type::i32, get_i32, make_i32)
            (native_type::i64, get_i64, make_i64)
            (native_type::f32, get_f32, make_f32)
            (native_type::f64, get_f64, make_f64)
        }
    };
}

macro_rules! comparison_binary_op {
    (
        $self:ident => $op_trait:path, $op_name:ident
    ) => {
        binary_op! { ($self, $op_trait, $op_name)
            (native_type::bool, get_bool, make_bool)
            (native_type::u8, get_u8, make_bool)
            (native_type::u16, get_u16, make_bool)
            (native_type::u32, get_u32, make_bool)
            (native_type::u64, get_u64, make_bool)
            (native_type::i8, get_i8, make_bool)
            (native_type::i16, get_i16, make_bool)
            (native_type::i32, get_i32, make_bool)
            (native_type::i64, get_i64, make_bool)
            (native_type::f32, get_f32, make_bool)
            (native_type::f64, get_f64, make_bool)
        }
    };
}

macro_rules! bitwise_binary_op {
    (
        $self:ident => $op_trait:path, $op_name:ident
    ) => {
        binary_op! { ($self, $op_trait, $op_name)
            (native_type::bool, get_bool, make_bool)
            (native_type::u8, get_u8, make_u8)
            (native_type::u16, get_u16, make_u16)
            (native_type::u32, get_u32, make_u32)
            (native_type::u64, get_u64, make_u64)
            (native_type::i8, get_i8, make_i8)
            (native_type::i16, get_i16, make_i16)
            (native_type::i32, get_i32, make_i32)
            (native_type::i64, get_i64, make_i64)
        }
    };
}

use binary_op;
use bitwise_binary_op;
use comparison_binary_op;
use numeral_binary_op;
use push_value;
use slab::Slab;
