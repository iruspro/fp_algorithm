type t = Q.t list

val zero : int -> t
val eval : t -> Point.t -> Q.t
val substitute : t -> t list -> t
(* val add : t -> t -> t *)
(* val multiply_by : Q.t -> t -> t *)
(* val ( + ) : t -> t -> t *)
(* val ( * ) : Q.t -> t *)
