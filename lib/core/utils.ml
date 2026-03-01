open Linear

let complement expr = Lin_expr.sub (Lin_expr.const Q.one) expr

(** f(x_1, ..., x_n) -> f(x_1, ..., 1 - x_n) *)
let flip_last local_alg point =
  let n = Point.dim point in
  let sub = Lin_expr.sub (Lin_expr.const Q.one) (Lin_expr.x n) in

  let flip_expr expr = Lin_expr.substitute expr n sub in
  let flip_ineq ineq = Lin_ineq.substitute ineq n sub in

  let cle = local_alg point in
  let constraints = List.map flip_ineq (Cond_lin_expr.constraints cle)
  and expr = flip_expr (Cond_lin_expr.expr cle) in
  Cond_lin_expr.construct constraints expr

(** f -> 1 - f *)
let flip local_alg (point : Point.t) =
  let cle = local_alg point in
  Cond_lin_expr.with_expr cle (complement (Cond_lin_expr.expr cle))

let dual local_alg point =
  let flip_point point =
    match Point.as_reversed_list point with
    | [] -> point
    | r_n :: rs -> Point.from_list (Q.sub Q.one r_n :: rs)
  in
  let point = flip_point point in
  flip (flip_last local_alg) point
