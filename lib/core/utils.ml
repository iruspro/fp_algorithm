open Linear

let complement expr = Lin_expr.sub (Linear.Lin_expr.const Q.one) expr

(* (** [flip_expr expr i] performs the substitution [x_i -> 1 - x_i] in the linear
    expression [expr].

    Raise [Invalid_argument] if [i < 1]. *)
let flip_expr expr i =
  if i < 1 then invalid_arg "Utils.flip_expr: variable index must be >= 1"
  else
    let expr' = Lin_expr.sub (Lin_expr.const Q.one) (Lin_expr.x i) in
    Lin_expr.substitute expr i expr' *)

let flip_last _ _ = failwith ""
(* let point = Point.flip_last point in *)
(* Cond_lin_expr.flip_last (local_alg point) *)

let complement_ _ _ = failwith ""
(* let cle = local_alg point in
  let expr = Lin_expr.complement (Cond_lin_expr.expr cle) in
  Cond_lin_expr.with_expr cle expr *)

let dual local_alg = complement_ (flip_last local_alg)
