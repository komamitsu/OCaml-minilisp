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

