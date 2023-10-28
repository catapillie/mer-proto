use std::fmt::Display;

use super::pos::Pos;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    from: Pos,
    to: Pos,
}

impl Span {
    pub const EOF: Span = Self::at(Pos::MAX);

    pub const fn new(from: Pos, to: Pos) -> Self {
        Self { from, to }
    }

    pub const fn at(pos: Pos) -> Self {
        Self::new(pos, pos)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self == &Self::EOF {
            write!(f, "eof")
        } else if self.from == self.to {
            write!(f, "{}", self.from)
        } else {
            write!(f, "{}->{}", self.from, self.to)
        }
    }
}
