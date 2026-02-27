open Linear

type t
(** Łukasiewicz μ-term *)

(** {2 Constructors} *)

val var : int -> t
(** [var i] returns the μ-term corresponding to the variable [x_i].

    Raise [Invalid_argument] if [i < 1]. *)

val const : Q.t -> t
(** [const q] returns the μ-term equal to [q].

    Raise [Invalid_argument] if [q] is not in the interval [[0,1]]. *)

val scalar_mult : Q.t -> t -> t
(** [scalar_mult q term] returns the μ-term obtained by multiplying [term] by
    the scalar coefficient [q].

    Raise [Invalid_argument] if [q] is not in the interval [[0,1]]. *)

val weak_disj : t -> t -> t
(** [weak_disj term1 term2] returns the Łukasiewicz weak disjunction of [term1]
    and [term2]. *)

val weak_conj : t -> t -> t
(** [weak_conj term1 term2] returns the Łukasiewicz weak conjunction of [term1]
    and [term2]. *)

val strong_disj : t -> t -> t
(** [strong_disj term1 term2] returns the Łukasiewicz strong disjunction of
    [term1] and [term2]. *)

val strong_conj : t -> t -> t
(** [strong_conj term1 term2] returns the Łukasiewicz strong conjunction of
    [term1] and [term2]. *)

val mu : int -> t -> t
(** [mu x_index term] constructs the least fix point term μx.term, where
    [x_index] denotes the bound variable.

    Raise [Invalid_argument] if [i < 1]. *)

val nu : int -> t -> t
(** [nu x_index term] constructs the greatest fix point term vx.term, where
    [x_index] denotes the bound variable.

    Raise [Invalid_argument] if [i < 1]. *)

(** {2 Functions} *)

val eval : int -> t -> Point.t -> Cond_lin_expr.t

(** {2 Print} *)

val to_string : t -> string
val print : t -> unit
