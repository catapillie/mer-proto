use std::process;

use crate::msg;

use super::opcode::Opcode;

pub enum Value {
    Num(f64),
    Bool(bool),
}

pub struct VM {
    program: Vec<u8>,
    ip: usize,
    stack: Vec<Value>,
}

impl VM {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            ip: 0,
            stack: Vec::new(),
        }
    }

    fn has_reached_end(&self) -> bool {
        self.ip >= self.program.len()
    }

    pub fn run(&mut self) {
        while !self.has_reached_end() {
            let opcopde = self.next_opcode();
            match opcopde {
                Opcode::nop => continue,
                Opcode::ld_num_const => self.ld_num_const(),
                Opcode::ld_true_const => self.ld_true_const(),
                Opcode::ld_false_const => self.ld_false_const(),
                Opcode::op_add => todo!(),
                Opcode::op_sub => todo!(),
                Opcode::op_mul => todo!(),
                Opcode::op_div => todo!(),
                Opcode::op_mod => todo!(),
                Opcode::op_eq => todo!(),
                Opcode::op_ne => todo!(),
                Opcode::op_le => todo!(),
                Opcode::op_lt => todo!(),
                Opcode::op_ge => todo!(),
                Opcode::op_gt => todo!(),
                Opcode::op_amp => todo!(),
                Opcode::op_bar => todo!(),
                Opcode::op_car => todo!(),
                Opcode::halt => return,
            }
        }
    }

    fn ld_num_const(&mut self) {
        let num = self.read_number();
        self.push(Value::Num(num))
    }

    fn ld_true_const(&mut self) {
        self.push(Value::Bool(true))
    }

    fn ld_false_const(&mut self) {
        self.push(Value::Bool(false))
    } 


    fn push(&mut self, value: Value) {
        self.stack.push(value)
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
        match Opcode::try_from(self.read_byte()) {
            Ok(opcode) => opcode,
            Err(_) => {
                msg::error("encountered illegal opcode");
                process::exit(1);
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.program[self.ip];
        self.ip += 1;
        byte
    }

    fn read_number(&mut self) -> f64 {
        f64::from_be_bytes([
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
        ])
    }
}
