#[derive(Debug, Clone)]
pub enum BinOp {
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    Mod,
    Amper,
    Bar,
    Caret,
    Eq,
    Neq,
    LtEq,
    Lt,
    GtEq,
    Gt,
    And,
    Or,
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
        match self {
            BinOp::Equal => 100,
            BinOp::Plus => 10,
            BinOp::Minus => 10,
            BinOp::Star => 20,
            BinOp::Slash => 20,
            BinOp::Mod => 20,
            BinOp::Amper => 30,
            BinOp::Bar => 40,
            BinOp::Caret => 35,
            BinOp::Eq => 60,
            BinOp::Neq => 60,
            BinOp::LtEq => 60,
            BinOp::Lt => 60,
            BinOp::GtEq => 60,
            BinOp::Gt => 60,
            BinOp::And => 50,
            BinOp::Or => 50,
        }
    }

    pub fn associativity(&self) -> Associativity {
        match self {
            BinOp::Equal => Associativity::Right,
            _ => Associativity::Left,
        }
    }
}

#[derive(Debug)]
pub enum Associativity {
    Left, Right
}

impl Associativity {
    pub fn is_left(&self) -> bool {
        match self {
            Associativity::Left => true,
            Associativity::Right => false,
        }
    }
}
