type rel = LessThan | LessEqual | GreaterThan | GreaterEqual
type t = Lin_expr.t * Lin_expr.t * rel

val is_satisfied : Point.t -> t -> bool
val find_unsatisfied : Point.t -> t list -> t option
val negate : t -> t
