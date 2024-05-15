
use crate::*;

#[derive(thiserror::Error, Debug, Clone, Copy)]
enum ParsingError {
    #[error("did not encounter an opening paren when expected")]
    NoOpeningParen,
    #[error("number of parens is mismatched")]
    MismatchedParens,
    #[error("empty input")]
    EmptyInput,
}

// I need to make two separate functions for this: the lexer and the parser. They shouldn't be one:
// https://vishpat.github.io/lisp-rs/overview.html
/****************** LEXER ***********************/

enum Token {
    LParen,
    RParen,
    Builtin(Builtin),
    Tipus,
    Atom,
}



/****************** PARSER ***********************/

// /// *Should* be non-copy, hopefully
//pub fn parse_sexpr(input: &str) -> Result<Sexpr, anyhow::Error> {
//    if input.is_empty() { return Err(ParsingError::EmptyInput)?; }
//    let mut list = Vec::new();
//    let mut stream = input.chars();
//    if stream.next().unwrap() != '(' { return Err(ParsingError::NoOpeningParen)?; }
//    let verb: String = stream.by_ref().take_while(|&c| c != ' ').collect();
//    todo!()
//}
