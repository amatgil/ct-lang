use crate::*;

const SETBLUE: &str = "\x1b[1;34m";
const SETRED: &str = "\x1b[1;31m";
const NOCOLOR: &str = "\x1b[1;0m";

pub fn display_error_at_token(s: Span, source: &str, help_text: &str) {
    dbg!(s);
    let left_delta = source[0..s.start.pos]
        .chars()
        .rev()
        .position(|c| c == '\n')
        .unwrap();
    let extend_left = Loc {
        pos: s.start.pos - left_delta,
        line: s.start.line,
        col: s.start.col - left_delta,
    };

    let right_delta = source
        .chars()
        .skip(s.end.pos)
        .position(|c| c == '\n')
        .unwrap();
    let extend_right = Loc {
        pos: s.end.pos + right_delta,
        line: s.end.line,
        col: s.end.col + right_delta,
    };


    //    xxxxHIGHLIGHTxxxxx
    //    ^   ^       ^    ^
    //    a   b       c    d

    let a = extend_left.pos;
    let b = s.start.pos;
    let c = s.end.pos;
    let d = extend_right.pos;

    println!();

    print!("{}", &source[a..b]);
    print!("{SETRED}{}{NOCOLOR}", &source[b..=c + 1]);
    println!("{}", &source[c + 2..d]);

    for _ in 0..s.start.col {
        print!(" ")
    }
    for _ in 0..s.end.col - s.start.col + 1 {
        print!("^")
    }
    println!();

    for _ in 0..s.start.col {
        print!(" ")
    }
    println!("{SETBLUE}{help_text}{NOCOLOR}");
}
