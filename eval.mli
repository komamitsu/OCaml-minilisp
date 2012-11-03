val eval :
    Expr.expr list ->
        ?env:(string, Expr.expr) Hashtbl.t ->
            unit -> (string, Expr.expr) Hashtbl.t * Expr.expr option

