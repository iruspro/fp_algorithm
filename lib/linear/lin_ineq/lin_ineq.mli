type t
(** Linear inequality in normal form [x_n □ q_m x_m + ... + q_1 x_1 + q_0],
    where [n > m], or [0 □ const]. *)

type rel = Lt | Le | Gt | Ge  (** Order relation. *)

(** {2 Constructors} *)

val construct : Lin_expr.t -> Lin_expr.t -> rel -> t
(** [construct lhs rhs rel] constructs and normalizes the inequality
    [lhs rel rhs] into the form [x_n □ rhs'] where [n] is the largest variable
    index in [rhs - lhs]. If the leading coefficient of [rhs - lhs] is negative,
    the relation is reversed. For constant inequalities ([dim = 0]), returns
    [0 □ (rhs - lhs)]. *)

(** {2 Getters} *)

val lhs : t -> Lin_expr.t
(** [lhs ineq] returns the left-hand side of the inequality in normal form. *)

val rhs : t -> Lin_expr.t
(** [rhs ineq] returns the right-hand side of the inequality in normal form. *)

val rel : t -> rel
(** [rel ineq] returns the order relation of the inequality. *)

val dim : t -> int
(** [dim ineq] returns the dimension of [ineq], i.e. the largest variable index
    occurring in it. *)

(** {2 Operators} *)

val negate : t -> t
(** [negate ineq] returns the logical negation of [ineq], i.e. [<] becomes [≥],
    [≤] becomes [>], etc. *)

val substitute : t -> int -> Lin_expr.t -> t
(** [substitute ineq i expr] substitutes the i-th variable of [ineq] with the
    linear expression [expr].

    Raise [Invalid_argument] if [i < 1]. *)

(** {2 Functions} *)

val is_satisfied : t -> Point.t -> bool
(** [is_satisfied ineq point] evaluates the inequality at [point] and returns
    [true] if it holds, and [false] otherwise. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string ineq] returns a string representation of [ineq] in the form
    [lhs □ rhs], e.g. ["x₂ ≤ 3x₁ + 1"]. *)

val print : t -> unit
(** [print ineq] is [print_string (to_string ineq)]. *)
