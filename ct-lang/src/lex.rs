/*!
The Lexer: turn the raw &str into a `Token` steam

Good example: https://github.com/kaikalii/cube/blob/master/src/lex.rs
*/

use std::fmt::Display;

pub fn lex<'a>(input: &'a str) -> Result<Vec<Token<'a>>, LexError> {
    Lexer {
        input: input.trim(),
        loc: Loc {
            line: 0,
            col: 0,
            pos: 0,
        },
    }
    .run()
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Lexer<'a> {
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
/// Section of the code: [start, end)
struct Span {
    start: Loc,
    end: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Token<'a> {
    typ: TokenKind<'a>,
    span: Span,
}

// TODO
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
enum TokenKind<'a> {
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    /// `<--`
    CommentStart,
    /// `<-->`
    DoccommentStart,
    /// Any identifier
    Identifier(&'a str),
    /// `'`
    Quote,
    /// `"`
    DoubleQuote,
    /// `deffun` or `DEFFUN`
    Deffun,
    /// Any value that evaluates to itself (number, string, etc)
    Literal(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub struct LexError {
    span: Span,
    kind: LexErrorKind,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum LexErrorKind {
    UnrecognizedToken(char),
    UnexpectedEOF,
}

type TokensRes<'a> = Result<Vec<Token<'a>>, LexError>;

impl<'l> Lexer<'l> {
    /// Advance if predicate, returning what is now behind the cursor
    /// If the predicate fails, None is returned
    fn advance_cursor_if(&mut self, f: impl Fn(char) -> bool) -> Result<Option<char>, LexError> {
        let c = self
            .input
            .chars()
            .skip(self.loc.pos)
            .next()
            .ok_or(LexError {
                span: Span {
                    start: self.loc,
                    end: self.loc,
                },
                kind: LexErrorKind::UnexpectedEOF,
            })?;
        if f(c) {
            match c {
                '\n' => {
                    self.loc.line += 1;
                    self.loc.col = 0;
                }
                '\r' => {}
                _ => self.loc.col += 1,
            }
            self.loc.pos += 1;
            Ok(Some(c))
        } else {
            Ok(None)
        }
    }
    fn advance_cursor_while(&mut self, f: impl Fn(char) -> bool) -> Result<Vec<char>, LexError> {
        let mut v = vec![];
        while let Some(c) = self.advance_cursor_if(&f)? {
            v.push(c)
        }
        Ok(v)
    }
    fn advance_cursor_amount(&mut self, amount: usize) -> Result<(), LexError> {
        for _ in 0..amount {
            self.next_char()?;
        }
        Ok(())
    }

    fn next_char_exact(&mut self, c: char) -> Result<bool, LexError> {
        Ok(self.advance_cursor_if(|c2| c == c2)?.is_some())
    }
    /// Matches string. If there's a match, the cursor is advanced. If not, there are no side effects
    fn next_chars_exact(&mut self, s: &str) -> Result<bool, LexError> {
        dbg!(&self, s);
        let n = s.len();
        let mut text = self.input.chars().skip(self.loc.pos).peekable();
        let mut s = s.chars().peekable();

        while let (Some(c1), Some(c2)) = (text.peek(), s.peek()) {
            dbg!(c1, c2);
            if c1 != c2 {
                return Ok(false);
            }

            s.next();
            text.next();
        }

        dbg!(&text, &s);
        match (text.peek(), s.peek()) {
            (Some(_), Some(_)) => unreachable!("while above will not exist while this occurs"),
            (_, None) => {
                self.advance_cursor_amount(n)?;
                Ok(true)
            }
            (None, Some(_)) => Err(LexError {
                span: Span {
                    start: self.loc,
                    end: self.loc,
                },
                kind: LexErrorKind::UnexpectedEOF,
            }),
        }
    }

    fn can_continue(&self) -> bool {
        self.loc.pos < self.input.len()
    }

    fn next_char(&mut self) -> Result<Option<char>, LexError> {
        self.advance_cursor_if(|_| true)
    }

    fn run(mut self) -> Result<Vec<Token<'l>>, LexError> {
        use TokenKind as TK;

        let mut tokens: Vec<Token<'l>> = Vec::new();
        loop {
            if self.can_continue() {
                let _ = self.advance_cursor_while(|c| " \t\n".contains(c));
                let start = self.loc;
                if let Some(c) = self.next_char()? {
                    tokens.push(match c {
                        '(' => with_span(TK::ParenOpen, start, self.loc),
                        ')' => with_span(TK::ParenClose, start, self.loc),
                        '<' if dbg!(self.next_chars_exact("-->")) == Ok(true) => {
                            with_span(TK::CommentStart, start, self.loc)
                        }
                        '<' if dbg!(self.next_chars_exact("--")) == Ok(true) => {
                            with_span(TK::CommentStart, start, self.loc)
                        }
                        _ => {
                            return Err(LexError {
                                kind: LexErrorKind::UnrecognizedToken(c),
                                span: Span {
                                    start,
                                    end: self.loc,
                                },
                            })
                        }
                    })
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(tokens)
    }
}

fn with_span(kind: TokenKind, start: Loc, end: Loc) -> Token {
    Token {
        typ: kind,
        span: Span { start, end },
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

fn print_tokens(ts: &[Token]) {
    for t in ts {
        println!("{t}");
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Token { typ, span } = self;
        let s = match typ {
            TokenKind::ParenOpen => "(",
            TokenKind::ParenClose => ")",
            TokenKind::CommentStart => "<--",
            TokenKind::DoccommentStart => "<-->",
            TokenKind::Identifier(x) => x,
            TokenKind::Quote => "'",
            TokenKind::DoubleQuote => "\"",
            TokenKind::Deffun => "DEFFUN",
            TokenKind::Literal(l) => &*l,
        };

        write!(f, "'{s}' {span}")
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.start, self.end)
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}-{}", self.pos, self.line, self.col)
    }
}

#[test]
fn paren_lex() {
    let input = "((()";
    let l = lex(input);

    let expected = vec![
        Token {
            typ: TokenKind::ParenOpen,
            span: Span {
                start: Loc {
                    pos: 0,
                    line: 0,
                    col: 0,
                },
                end: Loc {
                    pos: 1,
                    line: 0,
                    col: 1,
                },
            },
        },
        Token {
            typ: TokenKind::ParenOpen,
            span: Span {
                start: Loc {
                    pos: 1,
                    line: 0,
                    col: 1,
                },
                end: Loc {
                    pos: 2,
                    line: 0,
                    col: 2,
                },
            },
        },
        Token {
            typ: TokenKind::ParenOpen,
            span: Span {
                start: Loc {
                    pos: 2,
                    line: 0,
                    col: 2,
                },
                end: Loc {
                    pos: 3,
                    line: 0,
                    col: 3,
                },
            },
        },
        Token {
            typ: TokenKind::ParenClose,
            span: Span {
                start: Loc {
                    pos: 3,
                    line: 0,
                    col: 3,
                },
                end: Loc {
                    pos: 4,
                    line: 0,
                    col: 4,
                },
            },
        },
    ];
    assert_eq!(l.unwrap(), expected)
}

#[test]
fn comments_lex() {
    let input = "<-- <--";
    let l = lex(input);

    let expected = vec![
        Token {
            typ: TokenKind::CommentStart,
            span: Span {
                start: Loc {
                    pos: 0,
                    line: 0,
                    col: 0,
                },
                end: Loc {
                    pos: 3,
                    line: 0,
                    col: 3,
                },
            },
        },
        Token {
            typ: TokenKind::CommentStart,
            span: Span {
                start: Loc {
                    pos: 4,
                    line: 0,
                    col: 4,
                },
                end: Loc {
                    pos: 7,
                    line: 0,
                    col: 7,
                },
            },
        },
    ];
    print_tokens(&l.clone().unwrap());
    print_tokens(&expected);
    assert_eq!(l.unwrap(), expected)
}

#[test]
fn whitespace() {
    use TokenKind::*; // bad form, but it's a short test
    let input = "((
<--
)
)";
    let l = lex(input);

    let expected = vec![
        Token {
            typ: ParenOpen,
            span: Span {
                start: Loc {
                    pos: 0,
                    line: 0,
                    col: 0,
                },
                end: Loc {
                    pos: 1,
                    line: 0,
                    col: 1,
                },
            },
        },
        Token {
            typ: ParenOpen,
            span: Span {
                start: Loc {
                    pos: 1,
                    line: 0,
                    col: 1,
                },
                end: Loc {
                    pos: 2,
                    line: 0,
                    col: 2,
                },
            },
        },
        Token {
            typ: CommentStart,
            span: Span {
                start: Loc {
                    pos: 3,
                    line: 1,
                    col: 0,
                },
                end: Loc {
                    pos: 6,
                    line: 1,
                    col: 3,
                },
            },
        },
        Token {
            typ: ParenClose,
            span: Span {
                start: Loc {
                    pos: 7,
                    line: 2,
                    col: 0,
                },
                end: Loc {
                    pos: 8,
                    line: 2,
                    col: 1,
                },
            },
        },
        Token {
            typ: ParenClose,
            span: Span {
                start: Loc {
                    pos: 9,
                    line: 3,
                    col: 0,
                },
                end: Loc {
                    pos: 10,
                    line: 3,
                    col: 1,
                },
            },
        },
    ];
    println!("gotten:");
    print_tokens(&l.clone().unwrap());
    println!("expected");
    print_tokens(&expected);
    assert_eq!(l.unwrap(), expected)
}
