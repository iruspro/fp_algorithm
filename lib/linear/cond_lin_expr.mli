type t

(** {2 Constructor} *)

val construct : Lin_ineq.t list -> Lin_expr.t -> t

(** {2 Getters} *)

val constraints : t -> Lin_ineq.t list
val expr : t -> Lin_expr.t

(** {2 Functions} *)

val with_expr : t -> Lin_expr.t -> t
(** [with_expr cle expr] returns a copy of [cle] with its expression replaced by
    [expr]. *)

val flip_last : t -> t

(** {2 Print} *)

val to_string : t -> string
val print : t -> unit
