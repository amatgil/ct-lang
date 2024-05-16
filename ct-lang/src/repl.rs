use crate::*;


pub fn start_repl() -> Result<(), anyhow::Error> {
    let mut env = Env::new();
    loop {
        let input = "(+ 1 (+ 9 10))";
        if input == "(exit)" { break; }
        //let exp: Sexpr = parse_sexpr(input)?;
        //let result: Atom = eval(exp.into(), &mut env)?;
        //println!("{result}");
    }
    Ok(())
}
