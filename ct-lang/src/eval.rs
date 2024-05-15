use crate::*;

#[derive(thiserror::Error, Debug, Clone)]
pub enum EvalError {
    #[error("expression was invalid: {0}")]
    InvalidExpr(Sexpr),
    #[error("expression was empty")]
    EmptyExpr,
    #[error("unrecognized function: {0}")]
    UnrecognizedFn(String),
    #[error("expected argument ")]
    MissingArgument,
    #[error("extra argument: {0}")]
    ExtraArgument(Arc<Sexpr>),
    #[error("function does not exist: '{0}'")]
    FunctionDoesNotExist(String),
    #[error("missmatched types")]
    MismatchedTypes,
}

pub fn eval(s: Arc<Sexpr>, env: &mut Env) -> Result<Atom, EvalError> {
    match (*s).clone() {
        Sexpr::Atom(a) => Ok(a),
        Sexpr::Comp(list) => {
            let verb = list.get(0).ok_or(EvalError::EmptyExpr)?;
            if let Sexpr::Atom(Atom::Builtin(b)) = &(**verb) {
                match b {
                    Builtin::Add => {
                        let lhs: Atom = eval(list.get(1).ok_or(EvalError::MissingArgument)?.clone(), env)?;
                        let rhs: Atom = eval(list.get(2).ok_or(EvalError::MissingArgument)?.clone(), env)?;
                        if list.get(3).is_some() { return Err(EvalError::ExtraArgument(list.get(3).unwrap().clone())); }
                        if lhs.tipusde() != rhs.tipusde() { return Err(EvalError::MismatchedTypes); }
                        let ty: Tipus = lhs.tipusde();
                        //let adding_func: Arc<CtFunction> = env.get_fn(&ty, "ADD").or_else(|_| Err(EvalError::FunctionDoesNotExist("+".to_string())))?;
                        //let r: Sexpr = adding_func.call(&[Sexpr::Atom(lhs), Sexpr::Atom(rhs)]);
                        let r = match (lhs, rhs) {
                            (Atom::Int(x), Atom::Int(y)) => Atom::Int(x + y),
                            (Atom::Float(x), Atom::Float(y)) => Atom::Float(Float(x.0 + y.0)),
                            _ => return Err(EvalError::MismatchedTypes),
                        };
                        return Ok(r);
                    },
                    _ => todo!("Not yet impl'd: {b}"),
                }
            } else if let Sexpr::Atom(Atom::Symbol(sy)) = &(**verb) {
                match &**sy {
                    "deffun" => todo!(),
                    _ => {
                        // TODO: Look for them in Env here
                        return Err(EvalError::UnrecognizedFn(sy.clone()));
                    },
                }

            }
            todo!()
        }
    }

}
