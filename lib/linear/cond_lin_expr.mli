type t

(* Constructor *)
val construct : Lin_ineq.t list -> Lin_expr.t -> t

(* Getters *)
val constraints : t -> Lin_ineq.t list
val expr : t -> Lin_expr.t

(* Functions *)
val dual : int -> t -> t
