use crate::*;


pub fn display_error_at_token(t: &Token, source: &str, help_text: &str) {
    let left_delta = source[0..t.span.start.pos].chars().rev().position(|c| c == '\n').unwrap();
    let extend_left = Loc {
        pos: t.span.start.pos - left_delta,
        line: t.span.start.line,
        col: t.span.start.col - left_delta,
    };

    let right_delta = source.chars().skip(t.span.end.pos).position(|c| c == '\n').unwrap();
    let extend_right = Loc {
        pos: t.span.end.pos + right_delta,
        line: t.span.end.line,
        col: t.span.end.col + right_delta,
    };
    dbg!(t, source);

    println!();

    println!("{}", &source[extend_left.pos..extend_right.pos]);
    for _ in 0..t.span.start.col { print!(" ") }
    for _ in 0..t.span.end.col - t.span.start.col { print!("^") }
    println!();
    for _ in 0..t.span.start.col { print!(" ") }
    println!("{help_text}");
}
