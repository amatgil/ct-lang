
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Atom(Atom),
}

/// Assumes all tokens are space separated!
struct TokenStream<'a> {
    stream: &'a str,
    pos: usize
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(value: &'a str) -> Self {
        Self { stream: value, pos: 0 }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.stream.len() { return None; }
        else {
            let next_space_pos: usize = self.stream.chars()
                .skip(self.pos).position(|c| c == ' ').unwrap_or(self.stream.len() - 1);
            let token_str = &self.stream[self.pos..next_space_pos];

            use Token as T;
            let token: Token =
                if token_str == "(" { T::LParen }
            else if token_str == ")" { T::RParen }
            else if token_str == "'" { T::Quote }
            else { str_to_atom(token_str) };
            
            self.pos = next_space_pos + 1;

            Some(token)
        }
    }
}

#[rustfmt::skip]
fn str_to_atom(s: &str) -> Token {
    let a = match s {
        "nil" => Atom::Nil,
        "#true"  => Atom::Bool(true), "#false" => Atom::Bool(false),
        x if x.parse::<i64>().is_ok() => Atom::Int(x.parse::<i64>().unwrap()),
        x if x.parse::<f64>().is_ok() => Atom::Float(Float(x.parse::<f64>().unwrap())),
        s if str_to_quoted_string(s).is_some() => Atom::String(str_to_quoted_string(s).unwrap()),
        s if str_to_tipus(s).is_some() => Atom::Tipus(str_to_tipus(s).unwrap()),
        s if str_to_quoted_symbol(s).is_some() => Atom::String(str_to_quoted_string(s).unwrap()),
        s if str_to_builtin(s).is_some() => Atom::Builtin(str_to_builtin(s).unwrap()),
        s => Atom::Symbol(s.into())
    };

    Token::Atom(a)
    
}

fn str_to_tipus(s: &str) -> Option<Tipus> {
    todo!()
}

fn str_to_builtin(s: &str) -> Option<Builtin> {
    todo!()
}

fn str_to_quoted_symbol(s: &str) -> Option<String> {
    todo!()
}

/// Predicate: checks if s look like `"whatever"` (with the quotes) and returns `whatever` as a `String`
fn str_to_quoted_string(s: &str) -> Option<String> {
    if s.len() < 2 { return None; }
    else {
        use regex::Regex;
        let re = Regex::new("\"[^\"]*\"").unwrap();
        match re.captures(s) {
            Some(c) => Some(c[0].into()),
            None => None,
        }
    }
}


#[test]
fn quoted_strings() {
    let a = "\"xyzt\"";
    let b = "xyzt\"";
    let c = "\"\"";
    let d = "t";
    assert_eq!(Some("xyzt".into()), str_to_quoted_string(a));
    assert_eq!(None,                str_to_quoted_string(b));
    assert_eq!(Some("".into()),     str_to_quoted_string(c));
    assert_eq!(None           ,     str_to_quoted_string(d));
}

#[test]
fn basic_token_stream() {
    let i = "( + 1 2)";
    let mut s: TokenStream = i.into();
    assert_eq!(Token::LParen, s.next().unwrap());
    assert_eq!(Token::Atom(Atom::Int(1)), s.next().unwrap());
    assert_eq!(Token::Atom(Atom::Int(2)), s.next().unwrap());
    assert_eq!(Token::RParen, s.next().unwrap());
    assert!(s.next().is_none());
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
