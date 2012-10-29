open Printf

module Expr = struct
  type expr =
    | Num of int
    | Var of string
    | T
    | Nil
    | Func of string * string list * expr
    | Apply of string * expr list
    | List of expr list
    | Cond of expr * expr * expr

  let rec string_of_expr = function
    | Num n -> "Num(" ^ string_of_int n ^ ")"
    | Var s -> "Var(" ^ s ^ ")"
    | T -> "T" 
    | Nil -> "Nil" 
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

let lexer =
  Genlex.make_lexer ["("; ")"]

let parse_many f x =
  let rec loop x ys =
    try
      let y = f x in loop x (y::ys)
    with _ -> List.rev ys
  in
  loop x []

open Genlex
open Expr

let rec parse_token = parser
  | [<'Kwd "("; es = parse_many parse_token; 'Kwd ")">] -> List es
  | [<'Int x>] -> Num x
  | [<'Ident x>] -> Var x
  | [< _ >] -> failwith "parse_token error"

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

let rec eval env = function
  | Var x -> eval env (Hashtbl.find env x)
  | Apply (name, params) -> begin
      try
        let func = Hashtbl.find env name in
        match func with
        | Func (_, args, expr) -> begin
          let newenv = Hashtbl.copy env in
          ignore (
            List.fold_left
            (fun i arg ->
              Hashtbl.replace newenv arg (List.nth params i); i + 1) 0 args
          );
          eval newenv expr
        end
        | _ -> failwith "Func() isn't found"
      with Not_found -> begin
        match name with
        | "+" -> 
          let n0 = eval env (List.nth params 0) in
          let n1 = eval env (List.nth params 1) in
          begin
            match (n0, n1) with 
            | (Num(x0), Num(x1)) -> Num(x0 + x1)
            | _ -> failwith "'+' accepts only numbers"
          end
        | _ -> failwith (name ^ " isn't defined")
      end
    end
  | Num x -> Num x
  | _ -> failwith "hogehogehoge"


let test expected expr =
  print_endline ("expr: " ^ expr);
  let expr = parse_token (lexer (Stream.of_string expr)) in
  print_endline (string_of_expr expr);
  let expr = parse_exp expr in
  print_endline (string_of_expr expr);
  assert (expected = expr)

let () =
  test (Func("fib", ["n"; "x"; "y"], Num(2))) "(define (fib n x y) 2)";
  test (Func("sub", ["x"; "y"], Apply("-", [Var("x");Var("y")]))) "(define (sub x y) (- x y))";
  test (Apply("fib", [Num(1); Var("x")])) "(fib 1 x)";
  test (Cond(Apply(">",[Num(1);Num(0)]), Var("x"), Apply("+", [Num(1);Num(2)]))) "(if (> 1 0) x (+ 1 2))";
  test (Apply("+", [Var("x");Var("y")])) "(+ x y)";
  test (Apply("fib", [Apply("-", [Var("n");Num(1)]); Var("y"); 
        Apply("+", [Var("x");Var("y")])])) "(fib (- n 1) y (+ x y))";
  test (Func("fib", ["n";"x";"y"], 
          Cond(Apply("<=", [Var("n");Num(0)]), Var("y"),
          Apply("fib", [Apply("-", [Var("n");Num(1)]); Var("y"); Apply("+", [Var("x");Var("y")])]))))
    "(define (fib n x y) (if (<= n 0) y (fib (- n 1) y (+ x y))))";
  let expr = eval (Hashtbl.create 32) (Apply("+", [Num(2);Num(3)])) in
  print_endline (string_of_expr expr);
  let expr = parse_token (lexer (Stream.of_string Sys.argv.(1))) in
  let expr = parse_exp expr in
  print_endline (string_of_expr expr)

