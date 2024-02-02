use core::slice;
use std::{
    alloc::{self, Layout},
    cmp,
    io::{Cursor, Seek, SeekFrom},
    ops::{self},
};

use super::{error::Error, native_type, opcode, value::Value};
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

    fn destroy_frame(&mut self) -> Result<(), Error> {
        let frame = self.frames.pop().unwrap();
        self.stack.truncate(frame.local_offset);

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
                opcode::pop_n => {
                    let n = self.read_u8() as usize;
                    let len = self.stack.len();
                    if n > len {
                        return Err(Error::StackUnderflow);
                    }
                    self.stack.truncate(len - n);
                }

                opcode::dup => self.dup()?,
                opcode::dup_n => self.dup_n()?,

                opcode::keep => self.keep()?,
                opcode::keep_at => self.keep_at()?,

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
                opcode::call_addr => self.call_addr()?,

                opcode::ld_loc => self.ld_loc(),
                opcode::ld_loc_n => self.ld_loc_n(),
                opcode::st_loc => self.st_loc()?,
                opcode::st_loc_n => self.st_loc_n()?,

                opcode::alloc => self.alloc()?,
                opcode::alloc_n => self.alloc_n()?,
                opcode::ld_heap => self.ld_heap()?,
                opcode::ld_heap_n => self.ld_heap_n()?,
                opcode::st_heap => self.st_heap()?,
                opcode::st_heap_n => self.st_heap_n()?,
                opcode::realloc_loc => self.realloc_loc()?,
                opcode::realloc_loc_n => self.realloc_loc_n()?,

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

                opcode::todo => return Err(Error::Todo),
                opcode::unreachable => return Err(Error::Unreachable),

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
            }
            None => Err(Error::StackUnderflow),
        }
    }

    fn dup_n(&mut self) -> Result<(), Error> {
        let n = self.read_u8() as usize;
        let len = self.stack.len();
        if n > len {
            return Err(Error::StackUnderflow);
        }

        self.stack.extend_from_within((len - n)..);
        Ok(())
    }

    fn keep(&mut self) -> Result<(), Error> {
        let at = self.read_u8() as usize;
        let n = self.read_u8() as usize;
        let len = self.read_u8() as usize;
        if n > len || at + n > len {
            return Err(Error::StackUnderflow);
        }

        let trim_top = len - (at + n);
        let trim_bottom = at;

        self.stack.truncate(self.stack.len() - trim_top);
        let kept = self.stack.split_off(self.stack.len() - n);
        self.stack.truncate(self.stack.len() - trim_bottom);

        self.stack.extend_from_slice(&kept);

        Ok(())
    }

    fn keep_at(&mut self) -> Result<(), Error> {
        let n = self.read_u8() as usize;
        let len = self.read_u8() as usize;
        let at = self.pop()?.get_usize();
        if n > len || at + n > len {
            return Err(Error::StackUnderflow);
        }

        let trim_top = len - (at + n);
        let trim_bottom = at;

        self.stack.truncate(self.stack.len() - trim_top);
        let kept = self.stack.split_off(self.stack.len() - n);
        self.stack.truncate(self.stack.len() - trim_bottom);

        self.stack.extend_from_slice(&kept);

        Ok(())
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
        let frame = self.frames.last().unwrap();
        let values = self.stack.split_off(frame.local_offset + frame.local_count as usize);
        self.destroy_frame()?;
        self.stack.extend_from_slice(&values);
        Ok(())
    }

    fn call(&mut self) -> Result<(), Error> {
        let fp = self.read_u32() as u64;
        let back = self.cursor.position();
        self.call_fn(fp, Some(back))?;
        Ok(())
    }

    fn call_addr(&mut self) -> Result<(), Error> {
        let fp = self.pop()?.get_u32() as u64;
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

    fn ld_loc_n(&mut self) {
        let index = self.read_u8() as usize;
        let size = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        for i in 0..size {
            self.push(self.stack[offset + index + i]);
        }
    }

    fn st_loc(&mut self) -> Result<(), Error> {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        self.stack[offset + index] = self.pop()?;
        Ok(())
    }

    fn st_loc_n(&mut self) -> Result<(), Error> {
        let index = self.read_u8() as usize;
        let size = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        for i in 0..size {
            self.stack[offset + index + (size - i - 1)] = self.pop()?;
        }
        Ok(())
    }

    fn alloc(&mut self) -> Result<(), Error> {
        let value = self.pop()?;
        let addr = Self::alloc_value(value);
        self.push(Value::make_usize(addr));
        Ok(())
    }

    fn alloc_n(&mut self) -> Result<(), Error> {
        let n = self.read_u8() as usize;
        let len = self.stack.len();
        if n > len {
            return Err(Error::StackUnderflow);
        }
        let values = self.stack.split_off(len - n);
        let addr = Self::alloc_array(&values);
        self.push(Value::make_usize(addr));
        Ok(())
    }

    fn ld_heap(&mut self) -> Result<(), Error> {
        let addr = self.pop()?.get_usize();
        let value = Self::read_heap(addr);
        self.push(value);
        Ok(())
    }

    fn ld_heap_n(&mut self) -> Result<(), Error> {
        let n = self.read_u8() as usize;
        let addr = self.pop()?.get_usize();
        let values = Self::read_heap_array(addr, n);
        self.stack.extend_from_slice(values);
        Ok(())
    }

    fn st_heap(&mut self) -> Result<(), Error> {
        let addr = self.pop()?.get_usize();
        let value = self.pop()?;
        Self::store_heap(addr, value);
        Ok(())
    }

    fn st_heap_n(&mut self) -> Result<(), Error> {
        let n = self.read_u8() as usize;
        let len = self.stack.len();
        if n > len {
            return Err(Error::StackUnderflow);
        }
        let addr = self.pop()?.get_usize();
        let values = self.stack.split_off(len - n - 1);
        Self::store_heap_array(addr, &values);
        Ok(())
    }

    fn realloc_loc(&mut self) -> Result<(), Error> {
        let index = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        let value = self.stack[offset + index];
        let addr = Self::alloc_value(value);
        self.stack[offset + index] = Value::make_usize(addr);
        Ok(())
    }

    fn realloc_loc_n(&mut self) -> Result<(), Error> {
        let index = self.read_u8() as usize;
        let n = self.read_u8() as usize;
        let offset = self.frames.last().unwrap().local_offset;
        let values = &self.stack[(offset + index)..(offset + index + n)];
        let addr = Self::alloc_array(values);
        self.stack[offset + index] = Value::make_usize(addr);
        Ok(())
    }

    fn alloc_value(value: Value) -> usize {
        unsafe {
            let ptr = alloc::alloc(Layout::for_value(&value)) as *mut Value;
            ptr.write(value);
            ptr as usize
        }
    }

    fn alloc_array(values: &[Value]) -> usize {
        unsafe {
            let layout = Layout::array::<Value>(values.len()).unwrap();
            let ptr = alloc::alloc(layout) as *mut Value;
            values.as_ptr().copy_to(ptr, values.len());
            ptr as usize
        }
    }

    fn read_heap(addr: usize) -> Value {
        unsafe { (addr as *const Value).read() }
    }

    fn read_heap_array<'b>(addr: usize, n: usize) -> &'b [Value] {
        unsafe { slice::from_raw_parts(addr as *const Value, n) }
    }

    fn store_heap(addr: usize, value: Value) {
        unsafe { (addr as *mut Value).write(value) }
    }

    fn store_heap_array(addr: usize, values: &[Value]) {
        unsafe { values.as_ptr().copy_to(addr as *mut Value, values.len()) }
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
