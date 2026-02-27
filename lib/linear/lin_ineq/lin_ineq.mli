type t
(** Linear inequality in normal form [x_n □ q_m x_m + ... + q_1 x_1 + q_0],
    where [n > m], or [0 □ const]. *)

type rel = Lt | Le | Gt | Ge  (** Order relation. *)

(** {2 Constructors} *)

val construct : Lin_expr.t -> Lin_expr.t -> rel -> t
(** [construct lhs rhs rel] constructs a linear inequality in normal form. *)

(** {2 Getters} *)

val lhs : t -> Lin_expr.t
(** [lhs ineq] returns the left-hand side of the inequality in normal form. *)

val rhs : t -> Lin_expr.t
(** [rhs ineq] returns the right-hand side of the inequality in normal form. *)

val rel : t -> rel
(** [rel ineq] returns the order relation of the inequality. *)

(** {2 Operators} *)

val negate : t -> t
(** [negate ineq] returns the inequality obtained by negating [ineq]. *)

(** {2 Functions} *)

val is_satisfied : t -> Point.t -> bool
(** [is_satisfied ineq point] evaluates the inequality at [point] and returns
    [true] if it holds, and [false] otherwise. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string ineq] returns a string representation of the inequality. *)

val print : t -> unit
(** [print ineq] prints the inequality to standard output. *)
