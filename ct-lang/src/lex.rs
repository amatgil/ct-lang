/*!
The Lexer: turn the raw &str into a `Token` steam

Good example: https://github.com/kaikalii/cube/blob/master/src/lex.rs
*/

use std::{fmt::Display, iter};

#[rustfmt::skip]
const LEGAL_IDENT_CHARS: [char; 26*2 + 9 + 8 + 10] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '=', '?', '-', '_', '+', '-', '*', 
    '₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉',
];

const WHITESPACE: [char; 3] = [' ', '\t', '\n'];

const _CHECK_DISJOINTNESS_BETWEEN_LEGAL_IDENT_CHARS_AND_WHITSPACE: () = {
    let mut w_i = 0;
    while w_i < WHITESPACE.len() {
        let mut id_i = 0;
        while id_i < LEGAL_IDENT_CHARS.len() {
            if WHITESPACE[w_i] == LEGAL_IDENT_CHARS[id_i] {
                panic!("There's a char that's in both WHITESPACE and LEGAL_IDENT_CHARS")
            };
            id_i += 1;
        }
        w_i += 1;
    }
};

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
    /// Any identifier. Cannot start with a digit
    Identifier(&'a str),
    /// `'`
    Quote,
    /// `"`
    DoubleQuote,
    /// `deffun` or `DEFFUN`
    Deffun,
    /// `deftype` or `DEFTYPE`
    Deftype,
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
    fn advance_cursor_if(
        &mut self,
        f: impl Fn(char) -> bool,
    ) -> Result<(Loc, Option<char>), LexError> {
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

        let prev = self.loc;

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
            Ok((prev, Some(c)))
        } else {
            Ok((prev, None))
        }
    }
    fn advance_cursor_while(&mut self, f: impl Fn(char) -> bool) -> Result<Vec<char>, LexError> {
        let mut v = vec![];
        while let (_, Some(c)) = self.advance_cursor_if(&f)? {
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

    /// Matches string. If there's a match, the cursor is advanced. If not, there are no side effects
    fn next_chars_exact(&mut self, s: &str) -> Result<bool, LexError> {
        let n = s.len();
        let mut text = self.input.chars().skip(self.loc.pos).peekable();
        let mut s = s.chars().peekable();

        while let (Some(c1), Some(c2)) = (text.peek(), s.peek()) {
            if c1 != c2 {
                return Ok(false);
            }

            s.next();
            text.next();
        }

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

    fn next_char(&mut self) -> Result<(Loc, Option<char>), LexError> {
        self.advance_cursor_if(|_| true)
    }

    fn run(mut self) -> Result<Vec<Token<'l>>, LexError> {
        use TokenKind as TK;

        let mut tokens: Vec<Token<'_>> = Vec::new();
        loop {
            if self.can_continue() {
                let _ = self.advance_cursor_while(|c| WHITESPACE.contains(&c));
                let start = self.loc;
                if let (prev_loc, Some(c)) = self.next_char()? {
                    dbg!(c);
                    let (t, l) = match c {
                        '(' => (with_span(TK::ParenOpen, start, self.loc), self),
                        ')' => (with_span(TK::ParenClose, start, self.loc), self),
                        '<' if self.next_chars_exact("-->") == Ok(true) => {
                            (with_span(TK::CommentStart, start, self.loc), self)
                        }
                        '<' if self.next_chars_exact("--") == Ok(true) => {
                            (with_span(TK::CommentStart, start, self.loc), self)
                        }
                        '\'' => (with_span(TK::Quote, start, self.loc), self),
                        // Digit
                        x if x.is_digit(10) => self.lex_number(start, x),
                        // Identifier
                        _ => {
                            let (probable_ident, l) = self.lex_identifier(prev_loc);
                            let Token {
                                typ: TokenKind::Identifier(ident),
                                span,
                            } = probable_ident.clone()
                            else {
                                unreachable!("lex_identifier returns identifiers")
                            };
                            match ident {
                                "deffun" | "DEFFUN" => (
                                    Token {
                                        typ: TokenKind::Deffun,
                                        span,
                                    },
                                    l,
                                ),
                                "deftype" | "DEFTYPE" => (
                                    Token {
                                        typ: TokenKind::Deftype,
                                        span,
                                    },
                                    l,
                                ),
                                _ => (probable_ident, l),
                            }
                        }
                    };
                    self = l;
                    tokens.push(t);
                } else {
                    break; // We're out of chars!
                }
            } else {
                break; // We're out of chars but in a different way
            }
        }

        Ok(tokens)
    }
    fn lex_identifier(self, start: Loc) -> (Token<'l>, Self) {
        let ident_len = match self
            .input
            .chars()
            .skip(start.pos)
            .position(|c| !LEGAL_IDENT_CHARS.contains(&c))
        {
            Some(i) => dbg!(i),
            None => self.input.len() - start.pos,
        };

        dbg!(start, ident_len);
        let end_loc = Loc {
            pos: start.pos + ident_len,
            line: self.loc.line,
            col: start.pos + ident_len,
        };
        (
            Token {
                typ: TokenKind::Identifier(&self.input[start.pos..start.pos + ident_len]),
                span: Span {
                    start,
                    end: end_loc,
                },
            },
            Self {
                input: self.input,
                loc: end_loc,
            },
        )
    }
    fn lex_number(self, start: Loc, first_char: char) -> (Token<'l>, Self) {
        let cs = iter::once(first_char)
            .chain(
                self.input
                    .chars()
                    .skip(self.loc.pos)
                    .take_while(|&c| char::is_digit(c, 10)),
            )
            .collect::<String>();

        let n = cs.len();
        let end_loc = Loc {
            // - 1 because we already too `c`
            pos: self.loc.pos + n - 1,
            line: self.loc.line,
            col: self.loc.col + n - 1,
        };

        (
            Token {
                typ: TokenKind::Literal(cs),
                span: Span {
                    start,
                    end: end_loc,
                },
            },
            Self {
                input: self.input,
                loc: end_loc,
            },
        )
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
            TokenKind::Deftype => "DEFTYPE",
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


// =================================
// =================================
// =================================
//   _____ _____ ____ _____ ____
//  |_   _| ____/ ___|_   _/ ___|
//    | | |  _| \___ \ | | \___ \
//    | | | |___ ___) || |  ___) |
//    |_| |_____|____/ |_| |____/
// =================================
// =================================
// =================================

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

#[test]
fn numeric() {
    let input = "(1 3 78 1231 543212)";
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
            typ: TokenKind::Literal("1".into()),
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
            typ: TokenKind::Literal("3".into()),
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
        Token {
            typ: TokenKind::Literal("78".into()),
            span: Span {
                start: Loc {
                    pos: 5,
                    line: 0,
                    col: 5,
                },
                end: Loc {
                    pos: 7,
                    line: 0,
                    col: 7,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("1231".into()),
            span: Span {
                start: Loc {
                    pos: 8,
                    line: 0,
                    col: 8,
                },
                end: Loc {
                    pos: 12,
                    line: 0,
                    col: 12,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("543212".into()),
            span: Span {
                start: Loc {
                    pos: 13,
                    line: 0,
                    col: 13,
                },
                end: Loc {
                    pos: 19,
                    line: 0,
                    col: 19,
                },
            },
        },
        Token {
            typ: TokenKind::ParenClose,
            span: Span {
                start: Loc {
                    pos: 19,
                    line: 0,
                    col: 19,
                },
                end: Loc {
                    pos: 20,
                    line: 0,
                    col: 20,
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
#[test]
fn list() {
    let input = "(concat '(1 2) '(3 4 5))";
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
            typ: TokenKind::Identifier("concat".into()),
            span: Span {
                start: Loc {
                    pos: 1,
                    line: 0,
                    col: 1,
                },
                end: Loc {
                    pos: 7,
                    line: 0,
                    col: 7,
                },
            },
        },
        Token {
            typ: TokenKind::Quote,
            span: Span {
                start: Loc {
                    pos: 8,
                    line: 0,
                    col: 8,
                },
                end: Loc {
                    pos: 9,
                    line: 0,
                    col: 9,
                },
            },
        },
        Token {
            typ: TokenKind::ParenOpen,
            span: Span {
                start: Loc {
                    pos: 9,
                    line: 0,
                    col: 9,
                },
                end: Loc {
                    pos: 10,
                    line: 0,
                    col: 10,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("1".into()),
            span: Span {
                start: Loc {
                    pos: 10,
                    line: 0,
                    col: 10,
                },
                end: Loc {
                    pos: 11,
                    line: 0,
                    col: 11,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("2".into()),
            span: Span {
                start: Loc {
                    pos: 12,
                    line: 0,
                    col: 12,
                },
                end: Loc {
                    pos: 13,
                    line: 0,
                    col: 13,
                },
            },
        },
        Token {
            typ: TokenKind::ParenClose,
            span: Span {
                start: Loc {
                    pos: 13,
                    line: 0,
                    col: 13,
                },
                end: Loc {
                    pos: 14,
                    line: 0,
                    col: 14,
                },
            },
        },
        Token {
            typ: TokenKind::Quote,
            span: Span {
                start: Loc {
                    pos: 15,
                    line: 0,
                    col: 15,
                },
                end: Loc {
                    pos: 16,
                    line: 0,
                    col: 16,
                },
            },
        },
        Token {
            typ: TokenKind::ParenOpen,
            span: Span {
                start: Loc {
                    pos: 16,
                    line: 0,
                    col: 16,
                },
                end: Loc {
                    pos: 17,
                    line: 0,
                    col: 17,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("3".into()),
            span: Span {
                start: Loc {
                    pos: 17,
                    line: 0,
                    col: 17,
                },
                end: Loc {
                    pos: 18,
                    line: 0,
                    col: 18,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("4".into()),
            span: Span {
                start: Loc {
                    pos: 19,
                    line: 0,
                    col: 19,
                },
                end: Loc {
                    pos: 20,
                    line: 0,
                    col: 20,
                },
            },
        },
        Token {
            typ: TokenKind::Literal("5".into()),
            span: Span {
                start: Loc {
                    pos: 21,
                    line: 0,
                    col: 21,
                },
                end: Loc {
                    pos: 22,
                    line: 0,
                    col: 22,
                },
            },
        },
        Token {
            typ: TokenKind::ParenClose,
            span: Span {
                start: Loc {
                    pos: 22,
                    line: 0,
                    col: 22,
                },
                end: Loc {
                    pos: 23,
                    line: 0,
                    col: 23,
                },
            },
        },
        Token {
            typ: TokenKind::ParenClose,
            span: Span {
                start: Loc {
                    pos: 23,
                    line: 0,
                    col: 23,
                },
                end: Loc {
                    pos: 24,
                    line: 0,
                    col: 24,
                },
            },
        },
    ];

    println!("GOTTEN:");
    print_tokens(&l.clone().unwrap());
    println!("\nEXPECTED:");
    print_tokens(&expected);
    assert_eq!(l.unwrap(), expected)
}
