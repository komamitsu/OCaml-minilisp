val eval :
  (string, Expr.expr) Hashtbl.t ->
  Expr.expr -> (string, Expr.expr) Hashtbl.t * Expr.expr

val eval_exprs : Expr.expr list -> Expr.expr option

