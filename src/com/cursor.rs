use std::{iter::Peekable, str::Chars};

use super::pos::Pos;

#[derive(Clone)]
pub struct Cursor<'a> {
    chars: Peekable<Chars<'a>>,
    pos: Pos,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            pos: Pos::default(),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(c) = self.chars.next() else {
            return None;
        };

        self.pos.index += 1;
        if c == '\n' {
            self.pos.line += 1;
            self.pos.column = 0;
        } else {
            self.pos.column += 1;
        }

        Some(c)
    }
}
