type t = Q.t list (* [q_n; ...; q_1; q_0] *)

val zero : int -> t
val eval : t -> Point.t -> Q.t
val sub_last : t -> t -> t
val reduce_dim : t -> t
val equal : t -> t -> bool
val add : t -> t -> t
val mul_by : Q.t -> t -> t
