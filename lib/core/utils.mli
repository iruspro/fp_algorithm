open Linear

val complement : Lin_expr.t -> Lin_expr.t
(** [complement expr] returns the linear expression [1 - expr]. *)

val dual : Local_alg.t -> Local_alg.t
(** [dual f] returns the dual of the local algorithm [f], defined as
    [(1 - f(x_1, ..., 1 - x_n))]. *)
