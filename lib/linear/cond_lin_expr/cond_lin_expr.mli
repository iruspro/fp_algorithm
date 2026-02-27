type t
(** Conditioned linear expression of the form [C ⊢ e]. *)

(** {2 Constructor} *)

val construct : Lin_ineq.t list -> Lin_expr.t -> t
(** [construct constraints expr] constructs a conditioned linear expression
    [C ⊢ e] from a list of linear inequalities [constraints] representing the
    set [C] and a linear expression [expr] representing [e]. *)

(** {2 Getters} *)

val constraints : t -> Lin_ineq.t list
(** [constraints cle] returns the set of constraints [C] of the conditioned
    linear expression [C ⊢ e] as a list of linear inequalities. *)

val expr : t -> Lin_expr.t
(** [expr cle] returns the linear expression [e] of the conditioned linear
    expression [C ⊢ e]. *)

(** {2 Functions} *)

val with_expr : t -> Lin_expr.t -> t
(** [with_expr cle expr] returns a copy of [cle] with its expression replaced by
    [expr]. *)

(** {2 Print} *)

val to_string : t -> string
(** [to_string cle] returns a string representation of the conditioned linear
    expression [cle]. *)

val print : t -> unit
(** [print cle] prints the conditioned linear expression [cle] to standard
    output. *)
