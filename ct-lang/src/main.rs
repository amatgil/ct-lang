use ct::*;
use std::sync::Arc;

fn fib() {
    let fib = Sexpr::Comp(vec![
        Arc::new(Sexpr::Atom(Atom::Symbol("deffun".into()))),
        Arc::new(Sexpr::Atom(Atom::Symbol("fib".into()))),
        Arc::new(Sexpr::Comp(vec![
            Arc::new(Sexpr::Atom(Atom::Tipus(Tipus::Int))),
            Arc::new(Sexpr::Atom(Atom::Tipus(Tipus::CompType("List".into(), Tipus::Int.into())))),
        ])),
        Arc::new(Sexpr::Comp(vec![
            Arc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
        ])),
        Arc::new(Sexpr::Comp(vec![
            Arc::new(Sexpr::Atom(Atom::Builtin(Builtin::If))),
            Arc::new(Sexpr::Comp(vec![ // (or? (=? x 0) (=? x 1))
                Arc::new(Sexpr::Atom(Atom::Symbol("or?".into()))),
                Arc::new(Sexpr::Comp(vec![ // (=? x 0)
                    Arc::new(Sexpr::Atom(Atom::Symbol("=?".into()))),
                    Arc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                    Arc::new(Sexpr::Atom(Atom::Int(0))),
                ])),
                Arc::new(Sexpr::Comp(vec![ // (=? x 1)
                    Arc::new(Sexpr::Atom(Atom::Symbol("=?".into()))),
                    Arc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                    Arc::new(Sexpr::Atom(Atom::Int(1))),
                ])),
            ])),
            Arc::new(Sexpr::Atom(Atom::Int(1))),
            Arc::new(Sexpr::Comp(vec![   // (+ (fib (- x 1)) (fib (- x 2)))))
                Arc::new(Sexpr::Atom(Atom::Symbol("+".into()))),
                Arc::new(Sexpr::Comp(vec![   // (fib (- x 1))
                    Arc::new(Sexpr::Atom(Atom::Symbol("fib".into()))),
                    Arc::new(Sexpr::Comp(vec![   // (- x 1)
                        Arc::new(Sexpr::Atom(Atom::Symbol("-".into()))),
                        Arc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                        Arc::new(Sexpr::Atom(Atom::Int(1))),
                    ])),
                ])),
                Arc::new(Sexpr::Comp(vec![   // (fib (- x 2))
                    Arc::new(Sexpr::Atom(Atom::Symbol("fib".into()))),
                    Arc::new(Sexpr::Comp(vec![   // (- x 1)
                        Arc::new(Sexpr::Atom(Atom::Symbol("-".into()))),
                        Arc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                        Arc::new(Sexpr::Atom(Atom::Int(2))),
                    ])),
                ])),
            ]))
        ])),
    ]);
    println!("{fib}");
}

fn main() -> Result<(), anyhow::Error>{
    start_repl()?;
    //fib();
    Ok(())
}
