type rel = LessThan | LessEqual | GreaterThan | GreaterEqual

type n_type =
  | WithoutLast
  | LastGreaterThan
  | LastGreaterEqual
  | LastLessThan
  | LastLessEqual

type t

val lhs : t -> Lin_expr.t
val rhs : t -> Lin_expr.t
val rel : t -> rel
val n_type : t -> n_type
val construct : Lin_expr.t -> Lin_expr.t -> rel -> t
val make_constraints : Lin_expr.t -> Lin_expr.t list -> rel -> t list
val is_satisfied : t -> Point.t -> bool
val find_unsatisfied : t list -> Point.t -> t option
val negate : t -> t
val reverse : t -> t
val extract_rh_sides : t list -> Lin_expr.t list
val to_string : t -> string
val print : t -> unit
