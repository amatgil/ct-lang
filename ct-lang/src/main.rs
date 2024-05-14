use ct::*;

use std::rc::Rc;

fn fib() {
    let fib = Sexpr::Comp(vec![
        Rc::new(Sexpr::Atom(Atom::Symbol("deffun".into()))),
        Rc::new(Sexpr::Atom(Atom::Symbol("fib".into()))),
        Rc::new(Sexpr::Comp(vec![
            Rc::new(Sexpr::Atom(Atom::Tipus(Tipus::Int))),
            Rc::new(Sexpr::Atom(Atom::Tipus(Tipus::CompType("List".into(), Tipus::Int.into())))),
        ])),
        Rc::new(Sexpr::Comp(vec![
            Rc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
        ])),
        Rc::new(Sexpr::Comp(vec![
            Rc::new(Sexpr::Atom(Atom::Builtin(Builtin::If))),
            Rc::new(Sexpr::Comp(vec![ // (or? (=? x 0) (=? x 1))
                Rc::new(Sexpr::Atom(Atom::Symbol("or?".into()))),
                Rc::new(Sexpr::Comp(vec![ // (=? x 0)
                    Rc::new(Sexpr::Atom(Atom::Symbol("=?".into()))),
                    Rc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                    Rc::new(Sexpr::Atom(Atom::Int(0))),
                ])),
                Rc::new(Sexpr::Comp(vec![ // (=? x 1)
                    Rc::new(Sexpr::Atom(Atom::Symbol("=?".into()))),
                    Rc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                    Rc::new(Sexpr::Atom(Atom::Int(1))),
                ])),
            ])),
            Rc::new(Sexpr::Atom(Atom::Int(1))),
            Rc::new(Sexpr::Comp(vec![   // (+ (fib (- x 1)) (fib (- x 2)))))
                Rc::new(Sexpr::Atom(Atom::Symbol("+".into()))),
                Rc::new(Sexpr::Comp(vec![   // (fib (- x 1))
                    Rc::new(Sexpr::Atom(Atom::Symbol("fib".into()))),
                    Rc::new(Sexpr::Comp(vec![   // (- x 1)
                        Rc::new(Sexpr::Atom(Atom::Symbol("-".into()))),
                        Rc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                        Rc::new(Sexpr::Atom(Atom::Int(1))),
                    ])),
                ])),
                Rc::new(Sexpr::Comp(vec![   // (fib (- x 2))
                    Rc::new(Sexpr::Atom(Atom::Symbol("fib".into()))),
                    Rc::new(Sexpr::Comp(vec![   // (- x 1)
                        Rc::new(Sexpr::Atom(Atom::Symbol("-".into()))),
                        Rc::new(Sexpr::Atom(Atom::Symbol("x".into()))),
                        Rc::new(Sexpr::Atom(Atom::Int(2))),
                    ])),
                ])),
            ]))
        ])),
    ]);
    println!("{fib}");
}

fn main() {
    fib();
}
