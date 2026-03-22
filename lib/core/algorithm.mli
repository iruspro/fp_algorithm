open Linear

val lfp : int -> Local_alg.t -> Local_alg.t
(** [lfp dim local_alg] computes the least fixed point of [local_alg] in
    dimension [dim].

    Raise [Invalid_argument] if [dim <> Point.dim point + 1]. *)

val gfp : int -> Local_alg.t -> Local_alg.t
(** [gfp dim local_alg] computes the greatest fixed point of [local_alg] in
    dimension [dim], defined as [1 - lfp(dual(local_alg))].

    Raise [Invalid_argument] if [dim <> Point.dim point + 1]. *)
