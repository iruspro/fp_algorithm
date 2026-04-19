type t
(** Conditioned linear expression of the form [C ⊢ (e_1, ..., e_n)]. *)

(** {2 Constructor} *)

val construct : Lin_ineq.t list -> Lin_expr_vec.t -> t
(** [construct constraints exprs] constructs a conditioned linear expression
    [C ⊢ (e_1, ..., e_n)] from a list of linear inequalities [constraints]
    representing the set [C] and a linear expression vector [exprs] representing
    [(e_1, ..., e_n)]. *)

(** {2 Getters} *)

val constraints : t -> Lin_ineq.t list
(** [constraints cle] returns the set of constraints [C] of the conditioned
    linear expression [C ⊢ (e_1, ..., e_n)] as a list of linear inequalities. *)

val exprs : t -> Lin_expr_vec.t
(** [exprs cle] returns the linear expression vector [(e_1, ..., e_n)] of the
    conditioned linear expression [C ⊢ (e_1, ..., e_n)]. *)

val expr : t -> int -> Lin_expr.t
(** [expr cle k] returns the [k]-th linear expression of [cle] (1-indexed). *)

val codim : t -> int
(** [codim cle] returns the number of expressions in [cle]. *)

(** {2 Functions} *)

val add_constraint : t -> Lin_ineq.t -> t
(** [add_constraint cle ineq] returns a conditioned linear expression obtained
    from [cle] by adding the linear inequality [ineq] to its set of constraints.
*)

val add_constraints : t -> Lin_ineq.t list -> t
(** [add_constraints cle ineqs] returns a conditioned linear expression obtained
    from [cle] by adding all linear inequalities in [ineqs] to its set of
    constraints. *)

val with_expr : t -> Lin_expr.t -> int -> t
(** [with_expr cle e k] returns a copy of [cle] with its [k]-th expression
    replaced by [e] (1-indexed). The original [cle] is not modified. *)

val with_exprs : t -> Lin_expr_vec.t -> t
(** [with_exprs cle es] returns a copy of [cle] with all expressions replaced by
    [es], preserving the constraints. *)

val substitute_constraints : t -> int -> Lin_expr.t -> t
(** [substitute_constraints cle i sub] returns a copy of [cle] with variable
    [x_i] replaced by [sub] in all constraints. The expressions are unchanged.
*)

val substitute_from : t -> int -> int -> Lin_expr.t -> t
(** [substitute_from cle from i expr] returns a copy of [cle] with variable
    [x_i] replaced by [expr] in components [from, ..., n] (1-indexed),
    preserving the constraints.

    Raise [Invalid_argument] if [i < 1]. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string cle] returns a string representation of [cle] in the form
    [{{\n...\n\} ⊢ (e_1, ..., e_n)]. An empty constraint set is represented as
    [{\}]. *)

val print : t -> unit
(** [print cle] is [print_string (to_string cle)]. *)
