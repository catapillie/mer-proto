use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub const MIN: Self = Self::new(usize::MIN, usize::MIN, usize::MIN);
    pub const MAX: Self = Self::new(usize::MAX, usize::MAX, usize::MAX);

    pub const fn new(index: usize, line: usize, column: usize) -> Self {
        Self {
            index,
            line,
            column,
        }
    }
}

impl Default for Pos {
    fn default() -> Self {
        Self::new(0, 0, 0)
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self == &Self::MAX {
            write!(f, "eof")
        } else {
            write!(f, "{}:{}", self.line + 1, self.column + 1)
        }
    }
}
