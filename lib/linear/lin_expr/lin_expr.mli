type t
(** Linear expressions of the form q_n x_n + ... + q_1 x_1 + q_0. *)

(** {2 Constructors} *)

val from_list : Q.t list -> t
(** [from_list coeffs] creates a linear expression from a list of coefficients
    [q_n; ...; q_1; q_0] representing [q_n x_n + ... + q_1 x_1 + q_0].

    Raise [Invalid_argument] if the list is empty. *)

(** {2 Getters} *)

val as_list : t -> Q.t list
(** [as_list expr] returns the list of coefficients of [expr] in the form
    [q_n; ...; q_1; q_0]. *)

val dim : t -> int
(** [dim expr] returns the number of variables in [expr] (n in q_n x_n + ... +
    q_1 x_1 + q_0). *)

(** {2 Special expressions} *)

val zero : int -> t
(** [zero dim] returns the zero expression of dimension [dim].

    Raise [Invalid_argument] if [dim < 0]. *)

val one : int -> t
(** [one dim] returns the expression representing the constant 1 of dimension
    [dim].

    Raise [Invalid_argument] if [dim < 0]. *)

val x : int -> int -> t
(** [x dim i] returns the i-th variable x_i in a linear expression of dimension
    [dim].

    Raise [Invalid_argument] if [dim < 1 || i < 1 || i > dim]. *)

(** {2 Operators} *)

val equal : t -> t -> bool
(** [equal expr1 expr2] returns [true] if [expr1 = expr2] and [false] otherwise

    Raise [Invalid_argument] if the dimensions of the expressions differ. *)

val mul_by : Q.t -> t -> t
(** [mul_by q expr] returns the linear expression obtained by multiplying every
    coefficient of [expr] by [q]. *)

val add : t -> t -> t
(** [add expr1 expr2] returns the linear expression obtained by adding
    corresponding coefficients of [expr1] and [expr2].

    Raise [Invalid_argument] if the dimensions of the expressions differ. *)

val sub : t -> t -> t
(** [sub expr1 expr2] returns the linear expression obtained by subtracting the
    coefficients of [expr2] from [expr1].

    Raise [Invalid_argument] if the dimensions of the expressions differ. *)

(** {2 Functions} *)

val eval : t -> Point.t -> Q.t
(** [eval expr point] evaluates the linear expression [expr] at the given
    [point].

    Raise [Invalid_argument] if the dimensions of [expr] and [point] differ. *)

val sub_last : t -> t -> t
(** [sub_last expr1 expr2] substitutes the last variable of [expr1] with the
    linear expression [expr2].

    Raise [Invalid_argument] if the dimensions of the expressions differ. *)

val reduce_dim : t -> t
(** [reduce_dim expr] returns a new linear expression obtained from [expr] by
    removing the leading coefficient (the coefficient of the highest-indexed
    variable [x_n]).

    Raise [Invalid_argument] if [dim expr = 0]. *)

val extend_dim : t -> Q.t -> t
(** [extend_dim expr v] returns a new linear expression obtained by appending a
    new variable [x_{n+1}] with coefficient [v] to [expr]. *)

val find_supremum_term : t list -> Point.t -> t option
(** [find_supremum_term terms point] returns [Some term] corresponding to the
    linear expression in [terms] that evaluates to the minimal value at [point].
    Returns [None] if [terms] is empty. *)

val complement : t -> t
(** [complement expr] returns the linear expression obtained by subtracting
    [expr] from the constant expression equal to [1]. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string expr] returns a string representation of the linear expression
    [expr]. *)

val print : t -> unit
(** [print expr] prints the linear expression [expr] to the standard output. *)
