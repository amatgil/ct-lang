/*!
The Lexer: turn the raw &str into a `Token` steam

Good example: https://github.com/kaikalii/cube/blob/master/src/lex.rs
*/

// TODO
enum Token<'a> {
    ParenOpen,
    ParenClose,
    CommentStart,
    DoccommentStart,
    Identifier(&'a str)
}

enum LexError {
}

type TokensRes<'a> = Result<Vec<Token<'a>>, LexError>;

fn lex(input: &str) -> TokensRes<'_> {
    todo!()
}

fn lex_deffun(input: &str) -> TokensRes<'_> {
    todo!()
}
