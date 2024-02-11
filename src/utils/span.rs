use std::fmt::Display;

use super::pos::Pos;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub from: Pos,
    pub to: Pos,
}

impl Span {
    pub const EOF: Span = Self::at(Pos::MAX);

    pub const fn new(from: Pos, to: Pos) -> Self {
        Self { from, to }
    }

    pub const fn at(pos: Pos) -> Self {
        Self::new(pos, pos)
    }

    // joins two slices into one
    pub fn join(self, rhs: Self) -> Self {
        let max = if self.to.index >= rhs.to.index {
            self.to
        } else {
            rhs.to
        };

        let min = if self.from.index <= rhs.from.index {
            self.from
        } else {
            rhs.from
        };

        Span::new(min, max)
    }

    pub fn is_one_line(&self) -> bool {
        self.from.line == self.to.line
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
