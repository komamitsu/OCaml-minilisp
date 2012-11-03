let () =
  let exprs = Parser.parse Sys.argv.(1) in
  let evaled_expr = Eval.eval exprs in
  match evaled_expr with 
  | Some e -> print_endline (Expr.string_of_expr e)
  | None -> ()

