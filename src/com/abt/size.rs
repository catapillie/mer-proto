use std::{
    iter::Sum,
    ops::{Add, Mul},
};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Size {
    Known(usize),
    Infinite,
}

impl Size {
    pub fn unwrap(self) -> usize {
        match self {
            Self::Known(size) => size,
            _ => panic!("attempt to unwrap unknown size"),
        }
    }
}

impl Add for Size {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Known(left), Self::Known(right)) => Self::Known(left + right),
            (_, Self::Infinite) => Self::Infinite,
            (Self::Infinite, _) => Self::Infinite,
        }
    }
}

impl Add<usize> for Size {
    type Output = Self;

    fn add(self, right: usize) -> Self::Output {
        match self {
            Self::Known(left) => Self::Known(left + right),
            Self::Infinite => Self::Infinite,
        }
    }
}

impl Mul for Size {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Known(left), Self::Known(right)) => Self::Known(left * right),
            (_, Self::Infinite) => Self::Infinite,
            (Self::Infinite, _) => Self::Infinite,
        }
    }
}

impl Mul<usize> for Size {
    type Output = Self;

    fn mul(self, right: usize) -> Self::Output {
        match self {
            Self::Known(left) => Self::Known(left * right),
            Self::Infinite => Self::Infinite,
        }
    }
}

impl Sum for Size {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut size = 0;
        for s in iter {
            match s {
                Size::Known(s) => size += s,
                Size::Infinite => return Size::Infinite,
            }
        }
        Size::Known(size)
    }
}
