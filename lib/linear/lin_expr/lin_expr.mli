type t
(** Linear expressions of the form [q_n x_n + ... + q_1 x_1 + q_0], where
    [q_n <> 0] if [n > 0]. *)

(** {2 Constructors} *)

val from_list : Q.t list -> t
(** [from_list coeffs] creates a linear expression from a list of coefficients
    [q_n; ...; q_1; q_0] representing [q_m x_m + ... + q_1 x_1 + q_0], where
    [m <= n] and [q_m <> 0] if [m > 0].

    Raise [Invalid_argument] if the list is empty. *)

val const : Q.t -> t
(** [const q] return the expressions representing the constant [q]. *)

val x : int -> t
(** [x i] returns the i-th variable x_i in a linear expression.

    Raise [Invalid_argument] if [i < 1]. *)

(** {2 Getters} *)

val dim : t -> int
(** [dim expr] returns the largest index [n] such that [q_n <> 0] in
    [q_n x_n + ... + q_1 x_1 + q_0], or 0 if the constant term [q_0] is the only
    (possibly zero) coefficient. *)

val leading_coeff : t -> Q.t
(** [leading_coeff expr] returns the coefficient of the highest-indexed variable
    in [expr]. This value is guaranteed to be non-zero if [expr] contains any
    variables. For a constant expression, it returns the constant term, which
    may be zero. *)

val coeff : t -> int -> Q.t
(** [coeff expr i] returns the coefficient of the i-th variable in [expr], or
    the constant term if [i = 0]. The value may be zero.

    Raise [Invalid_argument] if [i < 0]. *)

(** {2 Operators} *)

val equal : t -> t -> bool
(** [equal expr1 expr2] returns [true] if [expr1 = expr2] and [false] otherwise.
*)

val mul_by : Q.t -> t -> t
(** [mul_by q expr] returns the linear expression obtained by multiplying every
    coefficient of [expr] by [q]. *)

val add : t -> t -> t
(** [add expr1 expr2] returns the linear expression obtained by adding
    corresponding coefficients of [expr1] and [expr2]. *)

val sub : t -> t -> t
(** [sub expr1 expr2] returns the linear expression obtained by subtracting the
    coefficients of [expr2] from [expr1]. *)

val substitute : t -> int -> t -> t
(** [substitute expr1 i expr2] substitutes the i-th variable of [expr1] with the
    linear expression [expr2].

    Raise [Invalid_argument] if [i < 1]. *)

(** {2 Functions} *)

val eval : t -> Point.t -> Q.t
(** [eval expr point] evaluates the linear expression [expr] at the given
    [point].

    Raise [Invalid_argument] if the dimension of [expr] is greater than the
    dimension of [point]. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string expr] returns a string representation of the linear expression
    [expr]. *)

val print : t -> unit
(** [print expr] prints the linear expression [expr] to standard output. *)
