open Genlex
open Printf
open Expr

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

let rec parse_expr = function
  | List(exprs) -> begin
    match exprs with
    | Var("define") :: List(Var(name)::vars) :: expr :: [] -> 
        let params =
          List.map 
          (function Var x -> x | _ -> failwith "argument should be var") vars in
        Func(name, params, parse_expr expr)
    | Var("if") :: expr_cond :: expr_then :: expr_else :: [] ->
        Cond(parse_expr expr_cond, parse_expr expr_then, parse_expr expr_else)
    | Var(name) :: exprs -> 
        let args = List.map parse_expr exprs in
        Apply(name, args)
    | _ -> failwith "parse_expr#0 error"
  end
    | Num x -> Num x
    | Var x -> Var x
    | _ -> failwith "parse_expr#1 error"

let parse stream = 
  let preparsed_list = parse_token_list (lexer stream) in
  List.rev (
    List.fold_left (fun a x -> parse_expr x::a) [] preparsed_list
  )

let parse_string s = parse (Stream.of_string s)

let parse_channel ch = parse (Stream.of_channel ch)
