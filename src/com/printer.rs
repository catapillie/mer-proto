use std::{collections::BTreeMap, ops::Add};

use colored::{Color, Colorize};

use crate::msg;

use super::{
    diagnostics::{Diagnostic, Severity},
    span::Span,
};

pub fn print_diagnostic(_path: &str, lines: &[&str], diagnostic: &Diagnostic) {
    let msg = diagnostic.kind.msg();
    let color = match diagnostic.severity {
        Severity::Error => {
            msg::error(msg);
            Color::BrightRed
        }
        Severity::Warning => {
            msg::warn(msg);
            Color::BrightYellow
        }
    };

    let mut printer = Printer::new(lines);
    for (span, note) in &diagnostic.notes {
        printer.highlight_span(span, Color::BrightBlue);
        if span.is_one_line() {
            printer.underline_span(span, Color::BrightBlue, Some(&note.msg()));
        } else {
            todo!()
        }
    }
    if let Some(span) = diagnostic.span {
        printer.highlight_span(&span, color);
        if span.is_one_line() {
            printer.underline_span(&span, color, None);
        }
    }
    printer.print();
}

struct Printer<'s> {
    lines: &'s [&'s str],
    content: BTreeMap<usize, Vec<(Color, String)>>,
}

impl<'s> Printer<'s> {
    fn new(lines: &'s [&'s str]) -> Self {
        Self {
            lines,
            content: Default::default(),
        }
    }

    fn print(self) {
        if self.content.is_empty() {
            return;
        }

        let max_line_len = self
            .content
            .keys()
            .cloned()
            .fold(0, usize::max)
            .add(1)
            .checked_ilog10()
            .unwrap_or(0) as usize
            + 1;

        println!(" {:>max_line_len$} ╥", " ");

        for (index, lines) in self.content {
            let mut iter = lines.iter();
            let Some((first_col, first)) = iter.by_ref().next() else {
                return;
            };

            println!(
                " {:>max_line_len$} {} {first}",
                (index + 1).to_string().color(*first_col),
                "║".color(*first_col)
            );
            for (col, line) in iter {
                println!(" {:>max_line_len$} {}{line}", " ", "║".color(*col));
            }
        }

        println!(" {:>max_line_len$} ╨", " ");
    }

    fn highlight_span(&mut self, span: &Span, color: Color) {
        let first_line = span.from.line;
        let last_line = span.to.line;

        if first_line > 0 {
            self.ensure_line_unless_empty(first_line - 1, Color::White);
        }
        for i in first_line..=last_line {
            self.ensure_line(i, color);
        }
        if last_line < self.lines.len() - 1 {
            self.ensure_line_unless_empty(last_line + 1, Color::White);
        }

        if span.is_one_line() {
            let line = self.get_line_mut(first_line).unwrap();
            *line = Self::color_span(line, color, Some(span.from.column), Some(span.to.column));
        } else {
            let first = self.get_line_mut(first_line).unwrap();
            *first = Self::color_span(first, color, Some(span.from.column), None);

            for i in (first_line + 1)..(last_line) {
                let line = self.get_line_mut(i).unwrap();
                *line = Self::color_span(line, color, None, None);
            }

            let last = self.get_line_mut(last_line).unwrap();
            *last = Self::color_span(last, color, None, Some(span.to.column));
        }
    }

    fn underline_span(&mut self, span: &Span, color: Color, msg: Option<&str>) {
        let from = span.from.column;
        let to = span.to.column;
        let w = to - from;

        let mut s = String::new();

        if from > 0 {
            s.push_str(&" ".repeat(from));
        }

        if w > 1 {
            s.push_str(&format!("└{}┘", "─".repeat(w)));
        } else {
            s.push_str(&format!("{:>s$}↑", " ", s = 2 - w));
            if w == 0 && msg.is_none() {
                s.push_str(" here");
            }
        }

        if let Some(msg) = msg {
            s.push(' ');
            s.push_str(&msg.color(color).to_string());
        }

        let index = span.from.line;
        self.ensure_line(index, color);
        self.content
            .get_mut(&index)
            .unwrap()
            .push((color, s.color(color).to_string()));
    }

    fn get_line_mut(&mut self, index: usize) -> Option<&mut String> {
        self.content
            .get_mut(&index)
            .and_then(|v| v.first_mut())
            .map(|(_, f)| f)
    }

    fn ensure_line(&mut self, index: usize, color: Color) {
        let lines = self.content.entry(index).or_default();
        if lines.is_empty() {
            lines.push((color, self.lines[index].to_string()));
        }
    }

    fn ensure_line_unless_empty(&mut self, index: usize, color: Color) {
        let line = self.lines[index];
        if !line.is_empty() {
            self.ensure_line(index, color);
        }
    }

    fn color_span(text: &str, color: Color, from: Option<usize>, to: Option<usize>) -> String {
        let chars = text.chars();

        let from = from.unwrap_or(0);
        let to = to.unwrap_or(text.len());
        let w = to - from;

        let before: String = chars.clone().take(from).collect();
        let at: String = chars.clone().skip(from).take(w).collect();
        let after: String = chars.clone().skip(to).collect();

        format!("{before}{}{after}", at.color(color))
    }
}
