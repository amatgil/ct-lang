use std::{fmt::Display, rc::Rc};

mod repl;

#[derive(Debug, Clone)]
pub struct Env {
}

#[derive(Debug, Clone)]
pub enum Builtin {
    Deffun,
    Deftype,
    Deftypeclass,
    If,
    Match,
    Where,
    Impls,
    Let,
}

#[derive(Debug, Clone)]
pub enum Tipus {
    Nil,
    Bool,
    Float,
    Int,
    String,
    CompType(String, Rc<Tipus>), // E.g. (List a)
    Function(Vec<Tipus>),
    UserDefined(String)
}

#[derive(Debug, Clone)]
pub enum Atom {
    Nil,
    Bool(bool),
    Int(i64),
    QuotedSymbol(String),
    Symbol(String),
    String(String),
    Tipus(Tipus),
    Float(f64),
    Builtin(Builtin)
}

#[derive(Debug, Clone)]
pub enum Sexpr {
    Atom(Atom),
    Comp(Vec<Rc<Sexpr>>),
}

impl Display for Tipus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Tipus::Nil            => "Nil".to_string(),
            Tipus::Bool           => "Bool".to_string(),
            Tipus::Float          => "Float".to_string(),
            Tipus::Int            => "Int".to_string(),
            Tipus::String         => "String".to_string(),
            Tipus::CompType(s, t) => format!("{s} {t}"),
            Tipus::Function(ts)   => {
                let mut out: String = "Fn[".into();
                let body: String = ts.iter().map(|t| format!("{t} ")).collect();
                out.push_str(&body[0..body.len() - 1]);
                out.push_str("]");
                out
            },
            Tipus::UserDefined(_) => todo!(),
        })
    }
}
    
impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Atom::Nil             => "nil".to_string(),
            Atom::Bool(b)         => if *b { "#true" } else { "#false" }.to_string(), 
            Atom::Int(i)          => i.to_string(),
            Atom::Symbol(s)       => s.to_string(),
            Atom::QuotedSymbol(s) => format!("'{s}"),
            Atom::String(s)       => s.to_string(),
            Atom::Tipus(t)        => t.to_string(),
            Atom::Float(f)        => f.to_string(),
            Atom::Builtin(b)      => b.to_string(),
        })
    }
}

impl Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Sexpr::Atom(a) => a.to_string(),
            Sexpr::Comp(v) => {
                let mut out: String = "(".into();
                let body: String = v.iter().map(|e| format!("{e} ")).collect();
                out.push_str(&body[0..body.len()-1]);
                out.push(')');
                out
            }
        })
    }
}


impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Builtin::Deffun       => "DEFFUN",
            Builtin::Deftype      => "DEFTYPE",
            Builtin::Deftypeclass => "DEFTYPECLASS",
            Builtin::If           => "IF",
            Builtin::Match        => "MATCH",
            Builtin::Where        => "WHERE",
            Builtin::Impls        => "IMPLS",
            Builtin::Let          => "LET",
        })
    }
}
