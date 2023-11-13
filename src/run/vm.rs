use std::{
    fmt::Display,
    process::{self},
};

use crate::msg;

use super::opcode::Opcode;

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
            Value::Uninitialized => write!(f, "??"),
            Value::Unit => write!(f, "()"),
            Value::Num(num) => write!(f, "{num}"),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}

pub struct VM {
    program: Vec<u8>,
    ip: usize,
    stack: Vec<Value>,
    loc_offsets: Vec<usize>,
}

impl VM {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            ip: 0,
            stack: Vec::new(),
            loc_offsets: Vec::new(),
        }
    }

    fn has_reached_end(&self) -> bool {
        self.ip >= self.program.len()
    }

    pub fn run(&mut self) {
        while !self.has_reached_end() {
            let opcode = self.next_opcode();
            match opcode {
                Opcode::nop => continue,
                Opcode::ld_num_const => self.ld_num_const(),
                Opcode::ld_true_const => self.ld_true_const(),
                Opcode::ld_false_const => self.ld_false_const(),
                Opcode::op_add => self.op_add(),
                Opcode::op_sub => self.op_sub(),
                Opcode::op_mul => self.op_mul(),
                Opcode::op_div => self.op_div(),
                Opcode::op_mod => self.op_mod(),
                Opcode::op_eq => self.op_eq(),
                Opcode::op_ne => self.op_ne(),
                Opcode::op_le => self.op_le(),
                Opcode::op_lt => self.op_lt(),
                Opcode::op_ge => self.op_ge(),
                Opcode::op_gt => self.op_gt(),
                Opcode::op_amp => self.op_amp(),
                Opcode::op_bar => self.op_bar(),
                Opcode::op_car => self.op_car(),
                Opcode::op_plus => self.op_plus(),
                Opcode::op_minus => self.op_minus(),
                Opcode::op_not => self.op_not(),
                Opcode::init_loc => self.init_loc(),
                Opcode::ld_loc => self.ld_loc(),
                Opcode::st_loc => self.st_loc(),
                Opcode::pop => {
                    self.pop();
                }
                Opcode::jmp => self.jmp(),
                Opcode::jmp_if => self.jmp_if(),
                Opcode::dbg => {
                    println!("{}", self.pop());
                }
                Opcode::halt => {
                    self.clear_loc();
                    if !self.stack.is_empty() {
                        msg::warn("stack remained non-empty after halt opcode");
                    }
                    return;
                }
            }
        }
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

    fn init_loc(&mut self) {
        let stack_len = self.stack.len();
        self.loc_offsets.push(stack_len);
        let count = self.read_u8().to_be();
        for _ in 0..count {
            self.push(Value::Uninitialized);
        }
    }

    fn clear_loc(&mut self) {
        let offset = *self.loc_offsets.last().unwrap();
        while self.stack.len() > offset {
            self.pop();
        }
    }

    fn ld_loc(&mut self) {
        let index = self.read_u8().to_be() as usize;
        let offset = *self.loc_offsets.last().unwrap();
        self.push(self.stack[offset + index].clone());
    }

    fn st_loc(&mut self) {
        let index = self.read_u8().to_be() as usize;
        let offset = *self.loc_offsets.last().unwrap();
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
        match Opcode::try_from(self.read_u8()) {
            Ok(opcode) => opcode,
            Err(_) => {
                msg::error("encountered illegal opcode");
                process::exit(1);
            }
        }
    }

    fn read_u8(&mut self) -> u8 {
        let byte = self.program[self.ip];
        self.ip += 1;
        byte
    }

    fn read_u32(&mut self) -> u32 {
        u32::from_be_bytes([
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
            self.read_u8(),
        ])
    }

    fn read_f64(&mut self) -> f64 {
        f64::from_be_bytes([
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
