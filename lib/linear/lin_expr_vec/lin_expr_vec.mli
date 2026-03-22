(** {1 Linear expressions vector} *)

type t
(** Vector of linear expressions [(f_1, ..., f_n)]. *)

(** {2 Constructors} *)

val from_array : Lin_expr.t array -> t
(** [from_array exprs] creates a linear expression vector from [exprs].

    Raise [Invalid_argument] if the array is empty. *)

val const : Q.t array -> t
(** [const consts] creates a linear expression vector of constants from
    [consts].

    Raise [Invalid_argument] if the array is empty. *)

(** {2 Getters} *)

val dim : t -> int
(** [dim le_vec] returns the largest variable index occurring across all
    components of [le_vec]. *)

val codim : t -> int
(** [codim le_vec] returns the number of components in [le_vec]. *)

val expr : t -> int -> Lin_expr.t
(** [expr le_vec k] returns the [k]-th component of [le_vec] (1-indexed).

    Raise [Invalid_argument] if [k < 1] or [k > n]. *)

(** {2 Operators} *)

val with_expr : t -> int -> Lin_expr.t -> t
(** [with_expr le_vec k e] returns a copy of [le_vec] with the [k]-th component
    replaced by [e] (1-indexed). The original is not modified.

    Raise [Invalid_argument] if [k < 1] or [k > n]. *)

val substitute_from : t -> int -> int -> Lin_expr.t -> t
(** [substitute_from le_vec from i expr] returns a copy of [le_vec] with
    variable [x_i] replaced by [expr] in components [from, ..., n] (1-indexed).

    Raise [Invalid_argument] if [i < 1]. *)

val substitute : t -> int -> int -> Lin_expr.t -> t
(** [substitute le_vec k i expr] returns a copy of [le_vec] with the [k]-th
    component's variable [x_i] replaced by [expr].

    Raise [Invalid_argument] if [k < 1], [k > n], or [i < 1]. *)

(** {2 Functions} *)

val eval : t -> Point.t -> Q.t array
(** [eval le_vec point] evaluates each component of [le_vec] at [point],
    returning [[| f_1(point); ...; f_n(point) |]].

    Raise [Invalid_argument] if [dim le_vec > Point.dim point]. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string le_vec] returns a string representation of [le_vec] in the form
    [(f_1, ..., f_n)]. *)

val print : t -> unit
(** [print le_vec] is [print_string (to_string le_vec)]. *)
