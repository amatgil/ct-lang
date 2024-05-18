
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

/****************** LEXER ***********************/

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Atom(Atom),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenStreamPos {
    Outside, // Not inside a function or anything
    FnFirstTerm,
    FnSignature,
    FnBody
}

/// Assumes all tokens are space separated!
struct TokenStream<'a> {
    stream: &'a str,
    pos: usize,
    location: TokenStreamPos,
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(value: &'a str) -> Self {
        Self { stream: value , pos: 0, location: TokenStreamPos::Outside }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.stream.len() { return None; }
        else {
            let next_space_pos: usize = self.stream.chars()
                .skip(self.pos).position(|c| c == ' ').unwrap_or(self.stream.len() - 1) + self.pos;
            let token_str = &self.stream[self.pos..next_space_pos];


            use TokenStreamPos as P;
            use Token as T;
            let token: Token =
                if token_str == "(" { Token::LParen }
            else if token_str == ")" { Token::RParen }
            else if token_str == "'" { Token::Quote }
            else {
                match self.location {
                    P::Outside
                        | P::FnFirstTerm
                        => Token::Atom(Atom::Symbol(token_str.into())),
                    P::FnSignature => Token::Atom(Atom::Tipus(str_to_tipus(token_str))),
                    P::FnBody => str_to_atom(token_str),
                }
            };
            
            self.pos = next_space_pos + 1;

            self.location = match (&self.location, &token) {
                (P::Outside, T::LParen) => todo!(),
                (P::Outside, T::RParen) => self.location,
                (P::Outside, T::Quote) => todo!(),
                (P::Outside, T::Atom(_)) => todo!(),
                (P::FnFirstTerm, T::LParen) => self.location,
                (P::FnFirstTerm, T::RParen) => todo!(),
                (P::FnFirstTerm, T::Quote) => todo!(),
                (P::FnFirstTerm, T::Atom(_)) => todo!(),
                (P::FnSignature, T::LParen) => todo!(),
                (P::FnSignature, T::RParen) => todo!(),
                (P::FnSignature, T::Quote) => todo!(),
                (P::FnSignature, T::Atom(_)) => todo!(),
                (P::FnBody, T::LParen) => todo!(),
                (P::FnBody, T::RParen) => todo!(),
                (P::FnBody, T::Quote) => todo!(),
                (P::FnBody, T::Atom(_)) => todo!(),
            };
              

            Some(token)
        }
    }
}

/// Only for when you know it's not a type
#[rustfmt::skip]
fn str_to_atom(s: &str) -> Token {
    let a = match s {
        "nil" => Atom::Nil,
        "#true"  => Atom::Bool(true), "#false" => Atom::Bool(false),
        x if x.parse::<i64>().is_ok() => Atom::Int(x.parse::<i64>().unwrap()),
        x if x.parse::<f64>().is_ok() => Atom::Float(Float(x.parse::<f64>().unwrap())),
        s if str_to_quoted_string(s).is_some() => Atom::String(str_to_quoted_string(s).unwrap()),
        // No type! There's another fn for that
        s if str_to_quoted_symbol(s).is_some() => Atom::String(str_to_quoted_string(s).unwrap()),
        s if str_to_builtin(s).is_some() => Atom::Builtin(str_to_builtin(s).unwrap()),
        s => Atom::Symbol(s.into())
    };

    Token::Atom(a)
    
}

/// s cannot be empty
/// Only for when you know it must be a type
fn str_to_tipus(s: &str) -> Tipus {
    assert!(!s.is_empty());
    use Tipus as T;

    match s {
        "Nil" => T::Nil,
        "Bool" => T::Bool,
        "Float" => T::Float,
        "Int" => T::Int,
        "String" => T::String,
        f if &s[0..3] == "Fn[" => todo!(),
        //CompType(String, Arc<Tipus>), // E.g. (List a)
        t if t.chars().next().unwrap().is_ascii_lowercase() && t.len() == 1
            => T::Generic(t.chars().next().unwrap()),
        s => T::UserDefined(s.into()),
    }
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
            Some(c) => Some(c[0][1..c[0].len()-1].into()),
            None => None,
        }
    }
}


#[test]
fn quoted_strings() {
    let a = r#""xyzt""#;
    let b = r#"xyzt"#;
    let c = r#""""#;
    let d = r#"t"#;
    dbg!(a, b, c, d);
    assert_eq!(Some("xyzt".into()), str_to_quoted_string(a));
    assert_eq!(None,                str_to_quoted_string(b));
    assert_eq!(Some("".into()),     str_to_quoted_string(c));
    assert_eq!(None           ,     str_to_quoted_string(d));
}

#[test]
fn basic_token_stream() {
    let i = "( + 1 2 ) ";
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
