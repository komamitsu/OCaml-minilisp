val eval :
    Expr.expr list ->
        ?env:Env.t ->
            unit -> Env.t * Expr.expr option

