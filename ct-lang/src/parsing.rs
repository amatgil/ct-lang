
use crate::*;

#[derive(thiserror::Error, Debug, Clone, Copy)]
enum ParsingError {
    #[error("did not encounter an opening paren when expected")]
    NoOpeningParen,
    #[error("number of parens is mismatched")]
    MismatchedParens
}

pub fn parse_sexpr(input: &str) -> Result<Sexpr, anyhow::Error> {
    todo!()
}
