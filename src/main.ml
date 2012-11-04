let () =
  if Array.length Sys.argv > 1 then
    (* from file *)
    let filename = Sys.argv.(1) in
    let in_ch = open_in filename in
    let exprs = Parser.parse_channel in_ch in
    ignore (Eval.eval exprs ())
  else
    (* repl *)
    let rec repl ?env () =
      print_string "minilisp> ";
      flush stdout;
      let rec read s =
        let line = input_line stdin in
        let s = s ^ line in
        let exprs = Parser.parse_string s in
        if List.length exprs = 0 then read s
        else begin
          let (env, last_expr) = 
          match env with
          | None -> Eval.eval exprs ()
          | Some env -> Eval.eval exprs ~env ()
          in
          begin
            match last_expr with
            | None -> ()
            | Some expr -> print_endline (Expr.string_of_expr expr)
          end;
          env
        end
      in 
      let env = read "" in
      repl ~env ()
    in
    try repl () with End_of_file -> print_newline ()

