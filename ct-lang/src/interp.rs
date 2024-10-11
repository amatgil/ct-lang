use itertools::izip;
use std::{collections::HashMap, fmt::Display};

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
struct Symbol(String);

/// TODO: enumerate builtin types while allowing for user-defined ones
#[derive(Eq, Hash, PartialEq, Clone, Debug)]
enum Type {
    Unit,
    Nat,
    Bool,
    Fn,
}

/// TODO
#[derive(Eq, Hash, PartialEq, Clone, Debug)]
enum Atom {
    Bool(bool),
    Nat(usize),
}

impl Atom {
    fn type_of(&self) -> Type {
        match self {
            Self::Bool(_) => Type::Bool,
            Self::Nat(_) => Type::Nat,
        }
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum ExprC {
    /// An atomic expression
    Atom(Atom),
    /// An identifier, to be looked up in the environment
    Identifier(Symbol),
    /// A function call/application
    FunCall { f: Symbol, args: Vec<ExprC> },
    /// A function definition, evaluates to the function it defines
    Deffun {
        name: Symbol,
        arg_types: Vec<Type>,
        arg_names: Vec<Symbol>,
        output_type: Type,
        body: Box<ExprC>,
    },
    /// Basic conditionals
    IfElse {
        clause: Box<ExprC>,
        truepath: Box<ExprC>,
        falsepath: Box<ExprC>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ÇValue {
    Unit,
    Atom(Atom),
    Lambda {
        args: Vec<(Symbol, Type)>,
        body: ExprC,
        output_type: Type,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct Env {
    bindings: HashMap<Symbol, ÇValue>,
}

impl Env {
    fn lookup(&self, id: Symbol) -> Option<&ÇValue> {
        self.bindings.get(&id)
    }
    fn add(mut self, sym: Symbol, val: ÇValue) -> Self {
        self.bindings.insert(sym, val);
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum InterpErr {
    UnboundVariable(Symbol),
    TypeMismatch { expected: Type, got: Type },
}

fn interp(expr: ExprC, mut env: Env) -> Result<(ÇValue, Env), InterpErr> {
    match dbg!(expr) {
        ExprC::Atom(a) => Ok((ÇValue::Atom(a), env)),
        ExprC::FunCall { f, args } => {
            let f = env.lookup(f.clone()).ok_or(InterpErr::UnboundVariable(f))?;
            match f {
                // TODO: typecheck
                ÇValue::Lambda {
                    args: arg_types,
                    body,
                    output_type: _,
                } => {
                    let og_env = env.clone();
                    let mut fn_env = env.clone();

                    if arg_types.len() != args.len() {
                        todo!("type error");
                    }
                    for (e, (s, _t)) in izip!(args, arg_types) {
                        let (v, _) = interp(e, og_env.clone())?;
                        fn_env = fn_env.add(s.clone(), v);
                        //TODO: typecheck t with s (Actually, all the typechecking should be done _before_ the interpreting.. hmm
                    }
                    dbg!(&fn_env);
                    dbg!(interp(body.clone(), fn_env))
                }
                ÇValue::Atom(a) => Err(InterpErr::TypeMismatch {
                    expected: Type::Fn,
                    got: a.type_of(),
                }),
                ÇValue::Unit => Err(InterpErr::TypeMismatch {
                    expected: Type::Fn,
                    got: Type::Unit,
                }),
            }
        }
        ExprC::Identifier(id) => Ok((
            env.lookup(id.clone())
                .ok_or(InterpErr::UnboundVariable(id))?
                .clone(),
            env,
        )),
        ExprC::Deffun {
            name,
            arg_types,
            arg_names,
            output_type,
            body,
        } => {
            // TODO: type check
            let f = ÇValue::Lambda {
                body: *body,
                args: izip!(arg_names, arg_types).collect(),
                output_type,
            };
            env = env.add(name, f);
            Ok((ÇValue::Unit, env))
        }
        ExprC::IfElse {
            clause,
            truepath,
            falsepath,
        } => {
            let (p, new_env) = interp(*clause, env)?;
            if ÇValue::Atom(Atom::Bool(true)) == p {
                interp(*truepath, new_env)
            } else if ÇValue::Atom(Atom::Bool(false)) == p {
                interp(*falsepath, new_env)
            } else {
                unreachable!("Internal type erorr: clause in `if`")
            }
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

// ==== TESTS ====
#[test]
fn atoms() {
    let exprs = [
        (
            ExprC::Atom(Atom::Bool(true)),
            ÇValue::Atom(Atom::Bool(true)),
        ),
        (
            ExprC::Atom(Atom::Bool(false)),
            ÇValue::Atom(Atom::Bool(false)),
        ),
    ];
    for (e, v) in exprs {
        assert_eq!(interp(e, Env::default()).unwrap().0, v)
    }
}

#[test]
fn fun_app() {
    // Lambda true cond: lambda x. lambda y. x
    let two = ExprC::Atom(Atom::Nat(2));
    let three = ExprC::Atom(Atom::Nat(3));

    let x = (Symbol("x".into()), Type::Nat);
    let y = (Symbol("y".into()), Type::Nat);

    let f = ÇValue::Lambda {
        args: vec![x, y],
        body: ExprC::Identifier(Symbol("x".into())),
        output_type: Type::Nat,
    };
    let mut env = Env::default();
    env = env.add(Symbol("f".into()), f);

    let e = ExprC::FunCall {
        f: Symbol("f".into()),
        args: vec![two, three],
    };

    assert_eq!(interp(e, env).unwrap().0, ÇValue::Atom(Atom::Nat(2)),)
}

#[test]
fn conditionals() {
    let e = ExprC::IfElse {
        clause: Box::new(ExprC::Atom(Atom::Bool(true))),
        truepath: Box::new(ExprC::Atom(Atom::Nat(5))),
        falsepath: Box::new(ExprC::Atom(Atom::Nat(6))),
    };

    assert_eq!(
        interp(e, Env::default()).unwrap().0,
        ÇValue::Atom(Atom::Nat(5))
    )
}
