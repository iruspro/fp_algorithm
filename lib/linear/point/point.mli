type t

(* Constructors *)
val from_list : Q.t list -> t (* [r_1; ...; r_n] -> point in Q^n *)
val from_array : Q.t array -> t (* [|r_1; ...; r_n|] -> point in Q^n *)

(* Getters *)
val as_list : t -> Q.t list (* point in Q^n -> [r_1; ...; r_n] *)
val as_reversed_list : t -> Q.t list (* point in Q^n -> [r_n; ...; r_1] *)
val dim : t -> int (* point in Q^n -> n *)

(* Functions *)
val extend_dim : t -> Q.t -> t (* point in Q^n -> point in Q^{n+1} *)

(* Print *)
val to_string : t -> string
val print : t -> unit
