open Expr

let test_expr expected s =
  let exprs = Parser.parse_string s in
  assert (expected = exprs)

let test_eval expected s =
  let exprs = Parser.parse_string s in
  let (env, evaled_expr) = Eval.eval exprs () in
  assert (expected = evaled_expr)
  
let test () =
  test_expr [Func("fib", ["n"; "x"; "y"], Num(2))] "(define (fib n x y) 2)";
  test_expr [Func("sub", ["x"; "y"], Apply("-", [Var("x");Var("y")]))] "(define (sub x y) (- x y))";
  test_expr [Apply("fib", [Num(1); Var("x")])] "(fib 1 x)";
  test_expr [Cond(Apply(">",[Num(1);Num(0)]), Var("x"), Apply("+", [Num(1);Num(2)]))]
        "(if (> 1 0) x (+ 1 2))";
  test_expr [Apply("+", [Var("x");Var("y")])] "(+ x y)";
  test_expr [Apply("fib", [Apply("-", [Var("n");Num(1)]); Var("y"); Apply("+", [Var("x");Var("y")])])]
        "(fib (- n 1) y (+ x y))";
  test_expr [Func("fib", ["n";"x";"y"],
        Cond(Apply("<=", [Var("n");Num(0)]),
        Var("y"),
        Apply("fib", [Apply("-", [Var("n");Num(1)]); Var("y"); Apply("+", [Var("x");Var("y")])])
        ))] "(define (fib n x y) (if (<= n 0) y (fib (- n 1) y (+ x y))))";
  test_expr [Apply("+", [Num(1); Num(2)]); Apply("-", [Num(5); Num(3)])] "(+ 1 2) (- 5 3)";
  test_eval None "";
  test_eval (Some(True)) "(= 1 1)";
  test_eval (Some(False)) "(= 1 2)";
  test_eval (Some(True)) "(= (+ 1 1) 2)";
  test_eval (Some(Num(2))) "(- 5 3)";
  test_eval (Some(True)) "(= 2 (- 5 3))";
  test_eval (Some(True)) "(= (+ 1 1) (- 5 3))";
  test_eval (Some(Num(12))) "( * (+ 1 2) (- 10 6))";
  test_eval (Some(Num(130))) "(define (hoge a b) (+ a b 100)) (hoge 10 20)";
  test_eval (Some(Num(5))) "(if (> 2 1) 5 10)";
  test_eval (Some(Num(1)))
    "(define (hoge x) (if (> x 10) 1 2)) (hoge 11)";
  test_eval (Some(Num(123)))
    "(define (loop n) (if (<= 0 n) 123 (loop (- n 1)))) (loop 3)";
  test_eval (Some(Num(6)))
    "(define (fact a n) (if (<= n 0) a (fact ( * a n) (- n 1)))) (fact 1 3)";
  test_eval (Some(Num(13)))
    "(define (fib n x y) (if (<= n 0) y (fib (- n 1) y (+ x y)))) (fib 5 1 1)"

let () =
  test ()

