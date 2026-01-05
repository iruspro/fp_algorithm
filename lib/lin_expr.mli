type t = Q.t list

val zero : int -> t
val eval : t -> Point.t -> Q.t
val sub_last : t -> t -> t
val mul_by : Q.t -> t -> t
