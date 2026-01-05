type t = Lin_ineq.t list * Lin_expr.t

val in_domain : t -> Point.t -> bool
(* val eval : t -> Point.t -> Q.t option *)
