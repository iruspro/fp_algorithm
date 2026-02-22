type t = Point.t -> Cond_lin_expr.t

let flip_last (local_alg : t) point =
  let point = Point.flip_last point in
  Cond_lin_expr.flip_last (local_alg point)

let complement (local_alg : t) point =
  let cle = local_alg point in
  let expr = Lin_expr.complement (Cond_lin_expr.expr cle) in
  Cond_lin_expr.with_expr cle expr

let dual (local_alg : t) = complement (flip_last local_alg)
