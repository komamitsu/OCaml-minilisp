open Expr

let debug_print env expr =
  Printf.printf "eval_expr -----------------------------\n";
  Printf.printf "  env ----------------------------\n";
  Hashtbl.iter
    (fun k v ->
  Printf.printf "    [%s] => %s \n" k (Expr.string_of_expr v)) env;
  Printf.printf "  expr ---------------------------\n";
  Printf.printf "    %s \n" (Expr.string_of_expr expr)

let rec apply_arithm env f label params =
  let rec loop result = function 
    | [] -> begin
        match result with
        | Some (Num a) -> Num a
        | Some _ -> failwith ("'" ^ label ^ "' accepts only numbers")
        | None -> failwith ("'" ^ label ^ "' needs arguments")
    end
    | hd::tl -> begin
      let (newenv, expr) = eval_expr env hd in
      match expr with
      | Num x -> begin
        match result with
        | Some (Num a) -> loop (Some (Num (f a x))) tl
        | Some _ -> failwith ("'" ^ label ^ "' accepts only numbers")
        | None -> loop (Some (Num x)) tl
      end
      | _ -> failwith ("'" ^ label ^ "' accepts only numbers")
    end
  in
  (env, loop None params)

and apply_cond env f label params =
  let (result, _) =
    List.fold_left
      (fun (b, last) expr -> 
        match eval_expr env expr with
        | (newenv, Num x) -> begin
          match last with
          | None -> (true, Some expr)
          | Some (last_expr) -> begin
            match eval_expr newenv last_expr with
            | (newenv, Num y) -> (b && f y x, Some expr)
            | _ -> failwith ("'" ^ label ^ "' accepts only numbers#0")
          end
        end
        | _ -> failwith ("'" ^ label ^ "' accepts only numbers#1")
      ) (true, None) params
  in
  (env, if result then True else False)

and update_apply_params env param_exprs orig_params =
  match orig_params with
  | [] -> (env, List.rev param_exprs)
  | orig_param::rest ->
      let (newenv, new_param) = eval_expr env orig_param in
      update_apply_params newenv (new_param::param_exprs) rest

and eval_expr env expr =
  (*
  debug_print env expr;
  *)
  match expr with
  | Var x -> eval_expr env (Env.get env x)

  | Func (name, _, _) as x -> begin
      Env.put env name x; (env, x)
  end

  | Apply (name, params) -> begin
      let (newenv, params) = update_apply_params env [] params in
      try
        let func = Env.get env name in
        let newenv = Env.clone env in
        match func with
        | Func (_, args, expr) -> begin
          ignore (
            List.fold_left
            (fun i arg ->
              Env.put newenv arg (List.nth params i); i + 1) 0 args
          );
          eval_expr newenv expr
        end
        | _ -> failwith "Func() isn't found"
      with Not_found -> begin
        match name with
        | "+" -> apply_arithm env (+) "+" params
        | "-" -> apply_arithm env (-) "-" params
        | "*" -> apply_arithm env ( * ) "*" params
        | "/" -> apply_arithm env (/) "/" params
        | "=" -> apply_cond env (=) "=" params
        | ">" -> apply_cond env (>) ">" params
        | "<" -> apply_cond env (<) "<" params
        | ">=" -> apply_cond env (>=) ">=" params
        | "<=" -> apply_cond env (<=) "<=" params
        | _ -> failwith (name ^ " isn't defined")
      end
    end

  | Cond (cond, ethen, eelse) -> begin
    let (newenv, evaled_expr) = eval_expr env cond in
      match evaled_expr with
      | True -> eval_expr newenv ethen
      | False -> eval_expr newenv eelse
      | _ -> failwith "condition should be boolean"
  end

  | x -> (env, x)

let eval exprs =
  let rec loop env last_evaled_expr = function
    | [] -> last_evaled_expr
    | expr::rest ->
      let (newenv, evaled) = eval_expr env expr in
      loop newenv (Some evaled) rest
  in
  loop (Env.init ()) None exprs

