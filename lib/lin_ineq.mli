type rel = LessThan | LessEqual | GreaterThan | GreaterEqual

type n_type =
  | WithoutLast
  | LastGreaterThan
  | LastGreaterEqual
  | LastLessThan
  | LastLessEqual

type t = Lin_expr.t * Lin_expr.t * rel * n_type

val construct : int -> Lin_expr.t -> Lin_expr.t -> rel -> t
val make_constraints : int -> Lin_expr.t -> Lin_expr.t list -> rel -> t list
val is_satisfied : Point.t -> t -> bool
val find_unsatisfied : Point.t -> t list -> t option
val negate : t -> t
