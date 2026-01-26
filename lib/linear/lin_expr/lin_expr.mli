type t
(** Linear expressions of the form q_n x_n + ... + q_1 x_1 + q_0. *)

(** {2 Constructors} *)

val from_list : Q.t list -> t
(** [from_list coeffs] creates a linear expression from a list of coefficients
    [q_n; ...; q_1; q_0] representing [q_n x_n + ... + q_1 x_1 + q_0]. *)

(** {2 Getters} *)

val as_list : t -> Q.t list
(** [as_list expr] returns the list of coefficients of [expr] in the form
    [q_n; ...; q_1; q_0]. *)

val dim : t -> int
(** [dim expr] returns the number of variables in [expr] (n in q_n x_n + ... +
    q_1 x_1 + q_0). *)

(** {2 Special expressions} *)

val zero : int -> t
(** [zero dim] returns the zero expression of dimension [dim]. *)

val one : int -> t
(** [one dim] returns the expression representing the constant 1 of dimension
    [dim]. *)

val x : int -> int -> t
(** [x dim i] returns the i-th variable x_i in a linear expression of dimension
    [dim]. *)

(* Operators *)
val equal : t -> t -> bool
val mul_by : Q.t -> t -> t
val add : t -> t -> t
val sub : t -> t -> t

(* Functions *)
val eval : t -> Point.t -> Q.t

val sub_last : t -> t -> t
(** [sub_last e1 e2] substitutes the last variable of [e1] with the linear
    expression [e2]. Both expressions must have the same dimension. *)

val reduce_dim : t -> t
val extend_dim : t -> Q.t -> t
val find_supremum_term : t list -> Point.t -> t option

(* Print *)
val to_string : t -> string
val print : t -> unit
