use ct::{error_reporting::display_error_at_token, *};

// quick, for tests
fn main() {
    let input = "(deffun fib
  yippe ka x🧑‍🌾 yay
  (Nat (List Nat))                      
  (x)                                    <-- Basic legal comment
  (if (or? (=? x 0) (=? x 1))            <-- Comment with: , and illegal char
  1                                      <-- More comments
  (+ (fib (- x 1)) (fib (- x 2)))))

";

    let l = lex(input).unwrap();
    _print_tokens(&l);

    display_error_at_token(
        l[5].span,
        input,
        "Some error here: fix it",
    );

    std::process::exit(1);
}

fn _print_tokens(ts: &[Token]) {
    for t in ts {
        println!("{t}");
    }
}

fn _rebuild_tokens(ts: &[Token]) {
    for Token { typ, .. } in ts {
        print!("{typ} ");
    }
}
