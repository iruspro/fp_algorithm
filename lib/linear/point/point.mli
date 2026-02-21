type t
(** Point in Q^n of the form (r_1, r_2, ..., r_n) *)

(** {2 Constructors} *)

val from_list : Q.t list -> t
(** [from_list coords] returns the point in Q^n whose coordinates are given by
    [coords], interpreted as [[r_1; ...; r_n]]. *)

(** {2 Getters} *)

val as_list : t -> Q.t list
(** [as_list point] returns the list of coordinates of [point] in the order
    [[r_1; ...; r_n]]. *)

val as_reversed_list : t -> Q.t list
(** [as_reversed_list point] returns the list of coordinates of [point] in
    reverse order [[r_n; ...; r_1]]. *)

val dim : t -> int
(** [dim point] returns the dimension of [point], i.e., the number of its
    coordinates. *)

(** {2 Functions} *)

val extend_dim : t -> Q.t -> t
(** [extend_dim point coord] returns the point in Q^(n+1) obtained from [point]
    in Q^n by appending the coordinate [coord] as the last component. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string point] returns a string representation of the point [point]. *)

val print : t -> unit
(** [print point] prints the point [point] to the standard output. *)
