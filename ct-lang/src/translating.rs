
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
enum Token<'a> {
    LParen,
    RParen,
    Quote,
    Ident(&'a str),
}

/// Assumes all tokens are space separated!
struct TokenStream<'a> {
    stream: &'a str,
    pos: usize,
    done: bool
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(value: &'a str) -> Self {
        Self { stream: value , pos: 0, done: false }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done { return None; }
        else if self.pos >= self.stream.len() { return None; }
        else {
            let next_space_pos: usize = self.stream
                .chars()
                .skip(self.pos)
                .position(|c| c == ' ' || c == '\n')
                .unwrap_or(self.stream.len() - 1)
                + self.pos;

            let token_str = &self.stream[self.pos..next_space_pos];
            self.pos += 1;

            let t = {
                if token_str.is_empty() {
                    self.pos = next_space_pos + 1;
                    self.next()
                }
                else if &token_str[0..1] == "(" { Some(Token::LParen) }
                else if token_str == ")" { Some(Token::RParen) }
                else if &token_str[0..1] == "'" { Some(Token::Quote) }
                else {
                    if token_str.len() > 1 && token_str[1..].contains(')') {
                        let rparen_pos = self.stream
                            .chars()
                            .skip(self.pos)
                            .position(|c| c == ')')
                            .unwrap() // SAFETY: We just checked that an rparen is contained
                            + self.pos;

                        //dbg!(token_str, self.pos, &self.stream[self.pos..self.pos + 1], rparen_pos);
                        let old_pos = self.pos - 1;
                        self.pos = rparen_pos;
                        let true_token_str = &self.stream[old_pos..rparen_pos];
                        if true_token_str == ")" { Some(Token::RParen) }
                        else { Some(Token::Ident(true_token_str))}
                    } else {
                        self.pos = next_space_pos + 1;
                        Some(Token::Ident(token_str))
                    }
                }
            };

            if self.pos == self.stream.len() { self.done = true; }

            t
            

        }
    }
}
impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Quote => "'",
            Token::Ident(s) => s,
        })
    }
}

///// Only for when you know it's not a type
//#[rustfmt::skip]
//fn str_to_atom(s: &str) -> Token {
//    let a = match s {
//        "nil" => Atom::Nil,
//        "#true"  => Atom::Bool(true), "#false" => Atom::Bool(false),
//        x if x.parse::<i64>().is_ok() => Atom::Int(x.parse::<i64>().unwrap()),
//        x if x.parse::<f64>().is_ok() => Atom::Float(Float(x.parse::<f64>().unwrap())),
//        s if str_to_quoted_string(s).is_some() => Atom::String(str_to_quoted_string(s).unwrap()),
//        // No type! There's another fn for that
//        s if str_to_quoted_symbol(s).is_some() => Atom::String(str_to_quoted_string(s).unwrap()),
//        s if str_to_builtin(s).is_some() => Atom::Builtin(str_to_builtin(s).unwrap()),
//        s => Atom::Symbol(s.into())
//    };
//
//    Token::Atom(a)
//    
//}

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
    assert_eq!(Token::Ident("+"), s.next().unwrap());
    assert_eq!(Token::Ident("1"), s.next().unwrap());
    assert_eq!(Token::Ident("2"), s.next().unwrap());
    assert_eq!(Token::RParen, s.next().unwrap());
    assert!(s.next().is_none());
}

#[test]
fn harder_token_stream() {
    let i = "( + ( * 3 4 ) 2 ) ";
    let mut s: TokenStream = i.into();
    assert_eq!(Token::LParen, s.next().unwrap());
    assert_eq!(Token::Ident("+"), s.next().unwrap());
    assert_eq!(Token::LParen, s.next().unwrap());
    assert_eq!(Token::Ident("*"), s.next().unwrap());
    assert_eq!(Token::Ident("3"), s.next().unwrap());
    assert_eq!(Token::Ident("4"), s.next().unwrap());
    assert_eq!(Token::RParen, s.next().unwrap());
    assert_eq!(Token::Ident("2"), s.next().unwrap());
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



#[test]
fn lmao() {
    let t = r#"(deffun fizzGo
  (Int Int (List String))
  (x max)
  (if (=? x max)
    'nil
    (let ((n (case x
      ((=? 0 (% x 15)) "fizzbuzz"
       (=? 0 (% x 3)) "fizz"
       (=? 0 (% x 5)) "buzz"))))
      (concat
        (singleton n)
        (fizzGo (inc x) max)))))
"#;
    let ts: TokenStream = t.into();
    for tok in ts {
        print!("{} ", tok);
    }
    println!();

    //panic!("panic for examining output")
}
