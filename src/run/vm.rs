use std::{
    fmt::Display,
    process::{self},
};

use crate::msg;

use super::opcode::{self, Opcode};

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
    back: Option<usize>,
    local_count: u8,
    local_offset: usize,
}

pub struct VM {
    program: Vec<u8>,
    ip: usize,
    stack: Vec<Value>,
    frames: Vec<Frame>,
    done: bool,
}

impl VM {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            ip: 0,
            stack: Vec::new(),
            frames: Vec::new(),
            done: false,
        }
    }

    fn has_reached_end(&self) -> bool {
        self.ip >= self.program.len()
    }

    fn create_frame(&mut self, back: Option<usize>, param_count: u8, local_count: u8) {
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

        self.ip = ip;
    }

    pub fn run(&mut self) {
        let first = self.next_opcode();
        let entry_point = self.read_u32() as usize;

        if !matches!(first, opcode::entry_point) {
            msg::error("no entry point defined");
            process::exit(1);
        }

        self.call_fn(entry_point, None);

        while !self.done && !self.has_reached_end() {
            let opcode = self.next_opcode();
            match opcode {
                opcode::nop => continue,
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
                opcode::op_amp => self.op_amp(),
                opcode::op_bar => self.op_bar(),
                opcode::op_car => self.op_car(),
                opcode::op_plus => self.op_plus(),
                opcode::op_minus => self.op_minus(),
                opcode::op_not => self.op_not(),
                opcode::ld_loc => self.ld_loc(),
                opcode::st_loc => self.st_loc(),

                opcode::pop => _ = self.pop(),

                opcode::jmp => self.jmp(),
                opcode::jmp_if => self.jmp_if(),

                opcode::ret => {
                    self.destroy_frame();
                    self.push(Value::Unit);
                },
                opcode::ret_val => {
                    let val = self.pop();
                    self.destroy_frame();
                    self.push(val);
                },

                opcode::dbg => {
                    println!("{}", self.pop());
                }

                opcode::call => {
                    let fp = self.read_u32() as usize;
                    let back = self.ip;
                    self.call_fn(fp, Some(back));
                }
                
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

    fn call_fn(&mut self, fp: usize, back: Option<usize>) {
        self.ip = fp;
        let (param_count, local_count) = self.read_function();
        self.create_frame(back, param_count, local_count);

        // push non-parameter locals
        for _ in 0..(local_count - param_count) {
            self.push(Value::Uninitialized);
        }
    }

    fn read_function(&mut self) -> (u8, u8) {
        if !matches!(self.next_opcode(), opcode::function) {
            msg::error("jumped to invalid function");
            process::exit(1);
        }

        let n = self.read_u16() as usize;
        // let bytes = &self.program[self.ip..self.ip + n];
        self.ip += n;

        // let _name = String::from_utf8(bytes.to_vec()).unwrap();
        let param_count = self.read_u8();
        let local_count = self.read_u8();

        (param_count, local_count)
    }

    fn jmp(&mut self) {
        self.ip = self.read_u32() as usize;
    }

    fn jmp_if(&mut self) {
        let to = self.read_u32();
        let guard = self.pop();
        match guard {
            Value::Bool(true) => self.ip = to as usize,
            Value::Bool(false) => (),
            _ => {
                msg::error(format!("invalid jump condition: {}", guard.type_name()));
                process::exit(1);
            }
        }
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

    fn op_add(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a + b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} + {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_sub(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a - b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} - {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_mul(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a * b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} * {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_div(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a / b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} / {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_mod(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Num(a % b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} % {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_eq(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a == b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} == {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_ne(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a != b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} != {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_le(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a <= b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} <= {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_lt(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a < b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} < {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_ge(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a >= b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} >= {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_gt(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(a > b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} > {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_amp(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a & b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} & {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_bar(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a | b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} | {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_car(&mut self) {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Value::Bool(a), Value::Bool(b)) => self.push(Value::Bool(a ^ b)),
            _ => {
                msg::error(format!(
                    "invalid operation: {} ^ {}",
                    left.type_name(),
                    right.type_name()
                ));
                process::exit(1);
            }
        }
    }

    fn op_plus(&mut self) {
        let val = self.pop();
        match &val {
            Value::Num(a) => self.push(Value::Num(*a)),
            _ => {
                msg::error(format!("invalid operation: + {}", val.type_name()));
                process::exit(1);
            }
        }
    }

    fn op_minus(&mut self) {
        let val = self.pop();
        match &val {
            Value::Num(a) => self.push(Value::Num(-*a)),
            _ => {
                msg::error(format!("invalid operation: - {}", val.type_name()));
                process::exit(1);
            }
        }
    }

    fn op_not(&mut self) {
        let val = self.pop();
        match &val {
            Value::Bool(a) => self.push(Value::Bool(!*a)),
            _ => {
                msg::error(format!("invalid operation: not {}", val.type_name()));
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

    fn next_opcode(&mut self) -> Opcode {
        self.read_u8()
    }

    fn read_u8(&mut self) -> u8 {
        let byte = self.program[self.ip];
        self.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        u16::from_le_bytes([self.read_u8(), self.read_u8()])
    }

    fn read_u32(&mut self) -> u32 {
        u32::from_le_bytes([
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
        ])
    }

    fn read_f64(&mut self) -> f64 {
        f64::from_le_bytes([
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
        ])
    }
}
