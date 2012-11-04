type expr =
    Num of int
  | Var of string
  | True
  | False
  | Func of string * string list * expr
  | Apply of string * expr list
  | List of expr list
  | Cond of expr * expr * expr
val string_of_expr : expr -> string
