type t
val init : unit -> t
val clone : t -> t
val get : t -> string -> Expr.expr
val put : t -> string -> Expr.expr -> unit
val iter : t -> (string -> Expr.expr -> unit) -> unit

