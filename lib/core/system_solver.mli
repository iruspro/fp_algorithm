open Linear

val lfp_system : int -> int -> Local_alg_vec.t -> Local_alg_vec.t
(** [lfp_system dim codim local_alg] computes the least fixed point of the
    system of local algorithms [local_alg] in dimension [dim] with [codim] bound
    variables.

    Raise [Invalid_argument] if [dim <> Point.dim point + codim]. *)
