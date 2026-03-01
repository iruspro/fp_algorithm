open Linear

val complement : Lin_expr.t -> Lin_expr.t
(** [complement expr] returns the linear expression obtained by subtracting
    [expr] from the constant expression equal to {m 1}. *)

val dual : Local_alg.t -> Local_alg.t
