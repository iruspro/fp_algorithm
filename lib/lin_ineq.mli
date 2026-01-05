type rel = LessThan | LessEqual | GreaterThan | GreaterEqual
type t = Lin_expr.t * Lin_expr.t * rel

val is_satisfied : t -> Point.t -> bool
val negate : t -> t
