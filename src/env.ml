type t = (string, Expr.expr) Hashtbl.t

let init () = Hashtbl.create 30

let clone t = Hashtbl.copy t

let get t k = Hashtbl.find t k

let put t k v = Hashtbl.replace t k v

