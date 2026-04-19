(** {1 Constraints} *)

type t

(** {2 Constructors} *)

val of_list : Lin_ineq.t list -> t
val singleton : Lin_ineq.t -> t
val empty : t

(** {2 Functions} *)

val add : t -> Lin_ineq.t -> t
val union : t -> t -> t
val is_satisfied : t -> Point.t -> bool
val find_unsatisfied : t -> Point.t -> Lin_ineq.t option
val substitute : t -> int -> Lin_expr.t -> t
val substitute_many : t -> int array -> Lin_expr.t array -> t
