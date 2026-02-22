open Linear
open Algorithm

let local_alg_1 point =
  let x = Lin_expr.x 1 1 and one = Lin_expr.one 1 in
  let expr1 = Lin_expr.from_list [ Q.( // ) 1 2; Q.( // ) 1 8 ]
  and constraints1 =
    [
      Lin_ineq.construct (Lin_expr.zero 1) x Lin_ineq.LessEqual;
      Lin_ineq.construct x
        (Lin_expr.mul_by (Q.( // ) 1 2) one)
        Lin_ineq.LessThan;
    ]
  and expr2 = Lin_expr.from_list [ Q.( // ) 7 8; Q.( // ) 5 64 ]
  and constraints2 =
    [
      Lin_ineq.construct
        (Lin_expr.mul_by (Q.( // ) 1 2) one)
        x Lin_ineq.LessEqual;
      Lin_ineq.construct x
        (Lin_expr.mul_by (Q.( // ) 3 4) one)
        Lin_ineq.LessThan;
    ]
  and expr3 = Lin_expr.from_list [ Q.( // ) 15 16; Q.( // ) 5 128 ]
  and constraints3 =
    [
      Lin_ineq.construct
        (Lin_expr.mul_by (Q.( // ) 3 4) one)
        x Lin_ineq.LessEqual;
      Lin_ineq.construct x one Lin_ineq.LessEqual;
    ]
  in
  if
    List.fold_left ( && ) true
      (List.map (fun cons -> Lin_ineq.is_satisfied cons point) constraints1)
  then Cond_lin_expr.construct constraints1 expr1
  else if
    List.fold_left ( && ) true
      (List.map (fun cons -> Lin_ineq.is_satisfied cons point) constraints2)
  then Cond_lin_expr.construct constraints2 expr2
  else if
    List.fold_left ( && ) true
      (List.map (fun cons -> Lin_ineq.is_satisfied cons point) constraints3)
  then Cond_lin_expr.construct constraints3 expr3
  else invalid_arg "Algorithm.local_alg_1: point out of domain"

let%test "lfp 1-dim" =
  let f' = lfp local_alg_1 in
  let cle = f' (Point.from_list []) in
  let expr = Cond_lin_expr.expr cle
  and result = Lin_expr.from_list [ Q.( // ) 1 4 ] in
  Lin_expr.dim expr = 0 && Lin_expr.equal expr result

let%test "gfp 1-dim" =
  let f' = gfp local_alg_1 in
  let cle = f' (Point.from_list []) in
  let expr = Cond_lin_expr.expr cle
  and result = Lin_expr.from_list [ Q.( // ) 5 8 ] in
  Lin_expr.dim expr = 0 && Lin_expr.equal expr result
