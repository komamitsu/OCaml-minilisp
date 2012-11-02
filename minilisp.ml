open Genlex
open Printf

module Expr = struct
  type expr =
    | Num of int
    | Var of string
    | True
    | False
    | Func of string * string list * expr
    | Apply of string * expr list
    | List of expr list
    | Cond of expr * expr * expr

  let rec string_of_expr = function
    | Num n -> "Num(" ^ string_of_int n ^ ")"
    | Var s -> "Var(" ^ s ^ ")"
    | True -> "True" 
    | False -> "False" 
    | Func (name, args, expr) -> 
        let args_str = List.fold_left (fun a x -> a ^ string_of_comma a ^ x) "" args in
        "Func(" ^ name ^ ", [" ^ args_str ^ "], " ^ (string_of_expr expr) ^ ")"
    | Apply (name, exprs) -> "Apply(" ^ name ^ ", " ^ (string_of_exprs exprs) ^ ")"
    | List exprs -> "List([" ^ string_of_exprs exprs ^ "])"
    | Cond (expr_cond, else_then, expr_else) ->
        "Cond(" ^ string_of_expr expr_cond ^ ", " ^ string_of_expr else_then ^ 
        ", " ^ string_of_expr expr_else ^ ")"
  and string_of_exprs exprs = 
    List.fold_left (fun a x -> a ^ string_of_comma a ^ string_of_expr x) "" exprs
  and string_of_comma s = if String.length s = 0 then "" else ", "
end

open Expr

module Parser = struct
  let lexer =
    Genlex.make_lexer ["("; ")"]

  let parse_many f x =
    let rec loop x ys =
      try
        let y = f x in loop x (y::ys)
      with _ -> List.rev ys
    in
    loop x []

  let rec parse_token = parser
    | [<'Kwd "("; es = parse_many parse_token; 'Kwd ")">] -> List es
    | [<'Int x>] -> Num x
    | [<'Ident x>] -> Var x
    | [< _ >] -> failwith "parse_token error"

  let rec parse_token_list = parser
    | [<es = parse_many parse_token>] -> es

  let rec parse_exp = function
    | List(exprs) -> begin
      match exprs with
      | Var("define")::List(Var(name)::vars)::expr::[] -> 
        let params =
          List.map 
          (function Var x -> x | _ -> failwith "argument should be var") vars in
        Func(name, params, parse_exp expr)
      | Var("if")::expr_cond::expr_then::expr_else::[] ->
          Cond(parse_exp expr_cond, parse_exp expr_then, parse_exp expr_else)
      | Var(name)::exprs -> 
          let args = List.map parse_exp exprs in
          Apply(name, args)
      | _ -> failwith "parse_exp#0 error"
    end
    | Num x -> Num x
    | Var x -> Var x
    | _ -> failwith "parse_exp#1 error"

  let parse s =
    let preparsed_list = parse_token_list (lexer (Stream.of_string s)) in
    List.rev (
      List.fold_left (fun a x -> parse_exp x::a) [] preparsed_list
    )
end

module Env = struct
  type t = (string, expr) Hashtbl.t

  let init () = Hashtbl.create 30

  let clone t = Hashtbl.copy t

  let get t k = Hashtbl.find t k

  let put t k v = Hashtbl.replace t k v
end

module Eval = struct
  let rec apply_arithm env f label params =
    let rec loop result = function 
      | [] -> begin
          match result with
          | Some (Num a) -> Num a
          | Some _ -> failwith ("'" ^ label ^ "' accepts only numbers")
          | None -> failwith ("'" ^ label ^ "' needs arguments")
      end
      (* TODO: eval hd *)
      | (Num hd)::tl -> begin
          match result with
          | Some (Num a) -> loop (Some (Num (f a hd))) tl
          | Some _ -> failwith ("'" ^ label ^ "' accepts only numbers")
          | None -> loop (Some (Num hd)) tl
      end
      | _ -> failwith ("'" ^ label ^ "' accepts only numbers")
    in
    (env, loop None params)
  and apply_cond env f label params =
    let (result, _) =
      List.fold_left
        (fun (b, last) expr -> 
          match eval env expr with
          | (newenv, Num x) -> begin
            match last with
            | None -> (true, Some expr)
            | Some (last_expr) -> begin
              match eval newenv last_expr with
              | (newenv, Num y) -> (b && f y x, Some expr)
              | _ -> failwith ("'" ^ label ^ "' accepts only numbers#0")
            end
          end
          | _ -> failwith ("'" ^ label ^ "' accepts only numbers#1")
        ) (true, None) params
    in
    (env, if result then True else False)
  and eval (env:Env.t) expr : (Env.t * expr) = match expr with
    | Var x -> eval env (Env.get env x)
    | Func (name, _, _) as x -> begin
        Env.put env name x; (env, x)
    end
    | Apply (name, params) -> begin
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
            eval newenv expr
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
          | "<=" -> apply_cond env (>=) "<=" params
          | _ -> failwith (name ^ " isn't defined")
        end
      end
    | Cond (cond, ethen, eelse) -> begin
        match cond with
        | True -> eval env ethen
        | False -> eval env eelse
        | _ -> failwith "condition should be boolean"
    end
    | x -> (env, x)
end

module Test = struct
  let test_expr expected s =
    let exprs = Parser.parse s in
    assert (expected = exprs)

  let test_eval expected s =
    let exprs = Parser.parse s in
    let evaled_list =
      List.fold_left (fun a x ->
        let (newenv, evaled) = Eval.eval (Env.init ()) x in
        evaled::a
      ) [] exprs
    in
    assert (expected = evaled_list)
    

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
    test_eval [True] "(= 1 1)";
    test_eval [False] "(= 1 2)";
    test_eval [True] "(= (+ 1 1) 2)";
    test_eval [Num(2)] "(- 5 3)";
    test_eval [True] "(= 2 (- 5 3))";
    test_eval [True] "(= (+ 1 1) (- 5 3))"
end

let () =
  Test.test ();
  let exprs = Parser.parse Sys.argv.(1) in
  (* TODO: use newenv *)
  List.iter (fun x ->
    let (newenv, evaled) = Eval.eval (Env.init ()) x in
    print_endline (string_of_expr evaled)
  ) exprs

