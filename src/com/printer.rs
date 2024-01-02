use std::{collections::BTreeMap, ops::Add};

use colored::{Color, Colorize};

use super::{
    diagnostics::{Diagnostic, NoteSeverity, Severity},
    span::Span,
};

pub fn print_diagnostic(path: &str, source: &str, diagnostic: &Diagnostic) {
    let lines = source.lines().collect::<Vec<_>>().into_boxed_slice();

    let msg = diagnostic.kind.msg();
    let color = match diagnostic.severity {
        Severity::Error => {
            println!("{} {msg}", "error".bright_red());
            Color::BrightRed
        }
        Severity::Warning => {
            println!("{} {msg}", "warning".bright_yellow());
            Color::BrightYellow
        }
    };

    let mut printer = Printer::new(&lines);
    for (span, note, severity) in &diagnostic.annotations {
        let note_color = match severity {
            NoteSeverity::Default => color,
            NoteSeverity::Annotation => Color::BrightYellow,
        };

        printer.highlight_span(span, note_color);
        let line = span.to.line;
        let msg = note.msg();
        if span.is_one_line() {
            let from = span.from.column;
            let to = span.to.column;
            printer.underline_span(line, from, to, note_color, &msg);
        } else if !msg.is_empty() {
            printer.add_on_line(line, note_color, &msg);
        }
    }

    if let Some(span) = diagnostic.span {
        println!("{}", format!("  --> {}:{}", path, span).cyan());
    } else {
        println!("{}", format!("  --> {}", path).cyan());
    }

    printer.print();
}

struct Printer<'s> {
    lines: &'s [&'s str],
    content: BTreeMap<usize, Vec<(Option<Color>, String, bool)>>,
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

        let mut prev_index = None;
        for (index, lines) in self.content {
            let mut iter = lines.iter();
            let Some((first_col, first, _)) = iter.by_ref().next() else {
                return;
            };
            let first_col = first_col.unwrap_or(Color::White);

            match prev_index {
                Some(prev) if prev + 1 != index => {
                    println!(" {:>max_line_len$} ╨ ", " ");
                    println!(" {:>max_line_len$}...", " ");
                    println!(" {:>max_line_len$} ╥ ", " ");
                }
                _ => {}
            }
            prev_index = Some(index);

            println!(
                " {:>max_line_len$} {} {first}",
                (index + 1).to_string().color(first_col),
                "║".color(first_col)
            );
            for (col, line, on_line) in iter {
                let col = col.unwrap_or(Color::White);
                if *on_line {
                    println!(" {:>max_line_len$} {} ({line})", " ", "╟─".color(col));
                } else {
                    println!(" {:>max_line_len$} {}{line}", " ", "║".color(col));
                }
            }
        }

        println!(" {:>max_line_len$} ╨", " ");
    }

    fn highlight_span(&mut self, span: &Span, color: Color) {
        let first_line = span.from.line;
        let last_line = span.to.line;

        if first_line > 0 {
            self.ensure_line_unless_empty(first_line - 1, None);
        }
        for i in first_line..=last_line {
            self.ensure_line(i, Some(color));
        }
        if last_line < self.lines.len() - 1 {
            self.ensure_line_unless_empty(last_line + 1, None);
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

    fn underline_span(&mut self, line: usize, from: usize, to: usize, color: Color, msg: &str) {
        let mut s = String::new();

        if from > 0 {
            s.push_str(&" ".repeat(from));
        }

        let w = to - from;
        if w > 1 {
            s.push_str(&format!("└{}┘", "─".repeat(w)));
        } else {
            s.push_str(&format!("{:>s$}↑", " ", s = 2 - w));
        }

        s.push(' ');
        s.push_str(&msg.color(color).to_string());

        self.ensure_line(line, Some(color));
        self.content
            .get_mut(&line)
            .unwrap()
            .push((Some(color), s.color(color).to_string(), false));
    }

    fn add_on_line(&mut self, line: usize, color: Color, msg: &str) {
        self.ensure_line(line, Some(color));
        self.content.get_mut(&line).unwrap().push((
            Some(color),
            msg.color(color).to_string(),
            true,
        ));
    }

    fn get_line_mut(&mut self, index: usize) -> Option<&mut String> {
        self.content
            .get_mut(&index)
            .and_then(|v| v.first_mut())
            .map(|(_, f, _)| f)
    }

    fn ensure_line(&mut self, index: usize, color: Option<Color>) {
        let lines = self.content.entry(index).or_default();
        if let Some((col, _, _)) = lines.first_mut() {
            if col.is_none() {
                *col = color;
            }
        } else {
            lines.push((color, self.lines[index].to_string(), false));
        }
    }

    fn ensure_line_unless_empty(&mut self, index: usize, color: Option<Color>) {
        let line = self.lines[index];
        if !line.trim().is_empty() {
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
