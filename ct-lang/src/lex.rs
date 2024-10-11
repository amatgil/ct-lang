/*!
The Lexer: turn the raw &str into a `Token` steam

Good example: https://github.com/kaikalii/cube/blob/master/src/lex.rs
*/

pub fn lex_source<'a>(input: &'a str) -> Result<Vec<Token<'a>>, LexError> {
    let mut l = Lexer {
        input,
        loc: Loc {
            line: 0,
            col: 0,
            pos: 0,
        },
    };

    l.run()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
struct Lexer<'a> {
    input: &'a str,
    loc: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Loc {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Span {
    start: Loc,
    end: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
struct Token<'a> {
    typ: TokenKind<'a>,
    span: Span,
}

// TODO
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
enum TokenKind<'a> {
    ParenOpen,
    ParenClose,
    CommentStart,
    DoccommentStart,
    Identifier(&'a str),
    Quote,
    DoubleQuote,
    Deffun,
    Literal(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
struct LexError {
    span: Span,
    kind: LexErrorKind
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
enum LexErrorKind {
    UnrecognizedToken
}


type TokensRes<'a> = Result<Vec<Token<'a>>, LexError>;

impl<'l> Lexer<'l> {
    /// Advance if predicate, returning what is now behind the cursor
    fn advance_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = self.input.chars().skip(self.loc.pos).filter(|c| f(*c)).next()?;
        match c {
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            }
            '\r' => {}
            _ => self.loc.col += 1,
        }
        self.loc.pos += 1;
        Some(c)
    }

    fn next_char_exact(&mut self, c: char) -> bool {
        self.advance_if(|c2| c == c2).is_some()
    }
    fn next_char(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    fn gen_token(&'l self, start: Loc, tokenkind: TokenKind<'l>) -> Token<'l> {
        Token {
            typ: tokenkind,
            span: Span {
                start,
                end: self.loc
            },
        }
    }

    fn run(&'l mut self) -> Result<Vec<Token<'l>>, LexError> {
        let mut tokens: Vec<Token<'l>> = Vec::new();
        loop {
            let start = self.loc;
            //if let Some(c) = self.next_char() {
            //    tokens.push(match c {
            //        '(' => self.gen_token(start, TokenKind::ParenOpen),
            //        _ => return Err(LexError {
            //            kind: LexErrorKind::UnrecognizedToken,
            //            span: Span { start, end: self.loc },
            //        }),
            //    })
            //} else { break }

        }

        Ok(tokens)
    }
}

impl Span {
    pub fn union(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}
