type t

val construct : Lin_ineq.t list -> Lin_expr.t -> t
val constraints : t -> Lin_ineq.t list
val expr : t -> Lin_expr.t
