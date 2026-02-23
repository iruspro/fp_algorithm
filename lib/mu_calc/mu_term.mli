open Linear

type t =
  | Var of int (* x_i *)
  | Zero
  | One
  | Scm of Q.t * t (* scalar multiplication *)
  | Lwd of t * t (* Lukasiewicz weak disjunction *)
  | Lwc of t * t (* Lukasiewicz weak conjunction *)
  | Lsd of t * t (* Lukasiewicz strong disjunction *)
  | Lsc of t * t (* Lukasiewicz strong conjunction *)
  | Mu of t (* lfp *)
  | Nu of t (* gfp *)

val n_free_vars : t -> int
val eval : t -> Point.t -> Cond_lin_expr.t
val to_string : t -> string
val print : t -> unit
