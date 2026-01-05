type rel
type t

val is_satisfied : Point.t -> t -> bool
val find_unsatisfied : Point.t -> t list -> t option
val negate : t -> t
