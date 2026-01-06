type t (* [r_n; ...; r_1] *)

val extend_dim : t -> Q.t -> t
val from_array : Q.t array -> t
val as_list : t -> Q.t list
