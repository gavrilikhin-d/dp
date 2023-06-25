use std::ops::Range;

use colored::Colorize;
use log::{debug, log_enabled};

use crate::parser::ParseResult;

pub fn log_result(target: &str, at: usize, source: &str, result: &ParseResult) {
    if log_enabled!(log::Level::Debug) {
        if result.ast.is_none() {
            debug!(
                target: target,
                "{}", "ERR".red().bold()
            );
            return;
        }

        let range = result.syntax.range().unwrap_or(at..at);
        log_with_highlight(target, at, source, range);
    }
}

pub fn log_with_highlight(target: &str, at: usize, source: &str, range: Range<usize>) {
    if log_enabled!(log::Level::Debug) {
        let Range { start, end } = range;

        let mut line_start = source[..at].rfind('\n').unwrap_or(0);
        if source.chars().nth(line_start) == Some('\n') {
            line_start += 1;
        }
        let line_end = end + source[end..].find('\n').unwrap_or(source[end..].len());

        let trivia = source[at..start]
            .split("\n")
            .map(|p| p.on_bright_black().to_string())
            .collect::<Vec<_>>()
            .join(format!("{}\n", r"\n".bright_black()).as_str());
        let parsed = source[start..end]
            .split("\n")
            .map(|p| p.on_bright_green().to_string())
            .collect::<Vec<_>>()
            .join(format!("{}\n", r"\n".bright_green()).as_str());

        debug!(
            target: target,
            "{} {}\n{}{}{}{}",
            "OK".green().bold(),
            if range.is_empty() {
                format!("(empty @'{}')", "|".dimmed())
            } else {
                "".to_string()
            },
            &source[line_start..at],
            trivia,
            if range.is_empty() {
                "|".dimmed().italic()
            } else {
                parsed.underline()
            },
            &source[end..line_end]
        );
    }
}
