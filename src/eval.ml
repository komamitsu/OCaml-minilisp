open Printf
open Expr

let debug_print env expr =
  printf "eval_expr --------------------------\n";
  printf "  env ----------------------------\n";
  Hashtbl.iter
    (fun k v ->
  printf "    [%s] => %s \n" k (string_of_expr v)) env;
  printf "  expr ---------------------------\n";
  printf "    %s \n" (string_of_expr expr)

let rec apply_arithm env f label params =
  let (env, params) = update_apply_params env params in
  let rec loop result = function 
    | [] -> begin
        match result with
        | Some (Num a) -> Num a
        | Some _ -> failwith ("'" ^ label ^ "' accepts only numbers")
        | None -> failwith ("'" ^ label ^ "' needs arguments")
    end
    | Num(x)::rest -> begin
      match result with
      | Some (Num a) -> loop (Some (Num (f a x))) rest
      | Some _ -> failwith ("'" ^ label ^ "' accepts only numbers")
      | None -> loop (Some (Num x)) rest
    end
    | _ -> failwith ("'" ^ label ^ "' accepts only numbers")
  in
  (env, loop None params)

and apply_cond env f label params =
  let (env, params) = update_apply_params env params in
  let (result, _) =
    List.fold_left
      (fun (result, last) -> function
        | Num x as expr -> begin
          match last with
          | None -> (true, Some expr)
          | Some last_expr -> begin
            match last_expr with
            | Num y -> (result && f y x, Some expr)
            | _ -> failwith ("'" ^ label ^ "' accepts only numbers#0")
          end
        end
        | _ -> failwith ("'" ^ label ^ "' accepts only numbers#1")
      ) (true, None) params
  in
  (env, if result then True else False)

and apply_print env params =
  let (env, params) = update_apply_params env params in
  List.iter (fun expr -> print_endline (string_of_expr expr)) params;
  let last_param = List.nth params ((List.length params) - 1) in
  (env, last_param)

and update_apply_params env orig_params =
  let rec _update_apply_params env params orig_params =
    match orig_params with
    | [] -> (env, List.rev params)
    | orig_param::rest ->
        let (env, param) = eval_expr env orig_param in
        _update_apply_params env (param::params) rest
  in _update_apply_params env [] orig_params

and load_params_onto_env env args params =
  ignore (
    List.fold_left
      (fun i arg -> Env.put env arg (List.nth params i); i + 1)
      0 args
  )

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
      let (env, params) = update_apply_params env params in
      try
        let func = Env.get env name in
        let env = Env.clone env in
        match func with
        | Func (_, args, expr) -> begin
          load_params_onto_env env args params;
          eval_expr env expr
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
        | "print" -> apply_print env params
        | _ -> failwith (name ^ " isn't defined")
      end
    end

  | Cond (cond, ethen, eelse) -> begin
    let (env, cond) = eval_expr env cond in
      match cond with
      | True -> eval_expr env ethen
      | False -> eval_expr env eelse
      | _ -> failwith "condition should be boolean"
  end

  | x -> (env, x)

let eval exprs ?(env = Env.init ()) () =
  let rec loop env last_evaled_expr = function
    | [] -> (env, last_evaled_expr)
    | expr::rest ->
      let (env, evaled) = eval_expr env expr in
      loop env (Some evaled) rest
  in
  loop env None exprs

