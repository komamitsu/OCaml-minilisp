let () =
  let exprs = Parser.parse Sys.argv.(1) in
  ignore (Eval.eval exprs)

