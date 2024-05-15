use std::cmp::Ordering;
use std::collections::{BTreeMap, HashSet};
use std::hash::Hash;
use std::fmt::Display;
use std::sync::Arc;

mod repl;
mod parsing;
mod eval;

pub use repl::*;
pub use parsing::*;
pub use eval::*;

#[derive(Debug, Clone)]
pub struct Env {
    typeclasses: BTreeMap<(String, Tipus), Typeclass>,
    typeclass_impls: BTreeMap<(Tipus, Typeclass), Arc<CtFunction>>,
}

impl Env {
    pub fn new() -> Self {
        /////// Typeclasses ///////
        let class_eq = Typeclass {
            name: "EQ".into(),
            missing_members: HashSet::from([
                ("EQ".into(), Tipus::Function(vec![
                    Tipus::Generic('a'), Tipus::Generic('a'), Tipus::Bool])),
            ]),
            default_members: HashSet::from([]),
        };
        let class_add = Typeclass {
            name: "ADD".into(),
            missing_members: HashSet::from([
                ("ADD".into(), Tipus::Function(vec![
                    Tipus::Generic('a'), Tipus::Generic('a'), Tipus::Generic('a')])),
            ]),
            default_members: HashSet::from([]),
        };

        let int_add: CtFunction = {
            let mut f = CtFunction {
                name: "ADD".into(),
                f: CtFunctionInternal::Builtin(Builtin::Add),
            };
            f
        };
        Self {
            typeclasses: BTreeMap::from([
                (("EQ".into(), Tipus::Int), class_eq.clone()),
                (("EQ".into(), Tipus::Float), class_eq.clone()),
                (("ADD".into(), Tipus::Int), class_add.clone()),
                (("ADD".into(), Tipus::Float), class_add.clone()),
            ]),
            typeclass_impls: BTreeMap::from([
                ((Tipus::Int, class_add.clone()), int_add.into())
            ]),
        }
    }

    /// Error means it wasn't found
    pub fn get_fn(&self, t: &Tipus, nom: &str) -> Result<Arc<CtFunction>,()> {
        for ((typ, class), f) in &self.typeclass_impls {
            if class.name == nom { return Ok(f.clone()); }
        }
        for ((name, typ), class) in &self.typeclasses {
            for member in &class.default_members {
                dbg!(&member.name, &class);
                if member.name == nom { return Ok(member.clone()); }
            }
        }
        Err(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CtFunction {
    name: String,
    f: CtFunctionInternal
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CtFunctionInternal {
    Builtin(Builtin),
    Native {
        id: u64,
        body: Sexpr
    }
}

impl CtFunction {
    fn call(&self, args: &[Sexpr]) -> Sexpr {
        todo!()
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Typeclass {
    name: String,
    default_members: HashSet<Arc<CtFunction>>,
    missing_members: HashSet<(String, Tipus)>, // Type must be a function type, obviously
}

impl PartialEq for Typeclass {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl PartialOrd for Typeclass {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}
impl Ord for Typeclass {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    // Keywords
    Deffun,
    Deftype,
    Deftypeclass,
    If,
    Match,
    Where,
    Impls,
    Impl,
    Let,

    // Default functions
    Add,
    Sub,
    Mul,
    Div,
    Print,
    Car,
    Cdr
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tipus {
    Nil,
    Bool,
    Float,
    Int,
    String,
    CompType(String, Arc<Tipus>), // E.g. (List a)
    Function(Vec<Tipus>),
    Generic(char),
    UserDefined(String)
}

#[derive(Debug, Clone, Copy)]
pub struct Float(f64);
impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        self.0.total_cmp(&other.0) == Ordering::Equal
    }
}
impl Eq for Float {}
impl Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Hash for Float {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Atom {
    Nil,
    Bool(bool),
    Int(i64),
    QuotedSymbol(String),
    Symbol(String),
    String(String),
    Tipus(Tipus),
    Float(Float),
    Builtin(Builtin)
}

impl Atom {
    fn tipusde(&self) -> Tipus {
        match self {
            Atom::Nil             => Tipus::Nil,
            Atom::Bool(_)         => Tipus::Bool,
            Atom::Int(_)          => Tipus::Int,
            Atom::QuotedSymbol(_) => todo!(),
            Atom::Symbol(_)       => todo!(),
            Atom::String(_)       => todo!(),
            Atom::Tipus(_)        => todo!(),
            Atom::Float(_)        => todo!(),
            Atom::Builtin(_)      => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sexpr {
    Atom(Atom),
    Comp(Vec<Arc<Sexpr>>),
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
            Tipus::Generic(c)     => c.to_string(),
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
            Builtin::Impl         => "IMPL",
            Builtin::Let          => "LET",
            Builtin::Add          => "+",
            Builtin::Sub          => "-",
            Builtin::Mul          => "*",
            Builtin::Div          => "/",
            Builtin::Print        => "PRINT",
            Builtin::Car          => "CAR",
            Builtin::Cdr          => "CDR",

        })
    }
}
