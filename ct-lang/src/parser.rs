use nom::{bytes::complete::tag, IResult};

enum Type {
    Uint(usize),
    Int(isize),
    Fn {
        inputs: Vec<Type>,
        outputs: Box<Type>,
    },
    Char(char),
}

enum Atom {
    Deftype,
    Deffun,
    Type(Type),
}

enum AST {
    Atom(Atom),
    SubTree(Box<AST>)
}

fn parse_expr(input: &str) -> IResult<&str, AST> {
    let (input, _) = tag("(")(input)?;

    todo!()
}
