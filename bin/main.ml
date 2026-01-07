open Fp_algorithm

let local_alg1 point =
  let open Q in
  let x = [ one; zero ] in
  let ineqs1 =
    [
      Lin_ineq.construct 1 (Lin_expr.zero 1) x Lin_ineq.LessEqual;
      Lin_ineq.construct 1 x [ Q.zero; 1 // 3 ] Lin_ineq.LessThan;
    ]
  and ineqs2 =
    [
      Lin_ineq.construct 1 [ zero; 1 // 3 ] x Lin_ineq.LessEqual;
      Lin_ineq.construct 1 x [ Q.zero; 2 // 3 ] Lin_ineq.LessThan;
    ]
  and ineqs3 =
    [
      Lin_ineq.construct 1 [ zero; 2 // 3 ] x Lin_ineq.LessEqual;
      Lin_ineq.construct 1 x [ zero; one ] Lin_ineq.LessEqual;
    ]
  in
  if
    List.fold_left ( && ) true
      (List.map (fun ineq -> Lin_ineq.is_satisfied ineq point) ineqs1)
  then Cond_lin_expr.construct ineqs1 [ 1 // 2; 1 // 3 ]
  else if
    List.fold_left ( && ) true
      (List.map (fun ineq -> Lin_ineq.is_satisfied ineq point) ineqs2)
  then Cond_lin_expr.construct ineqs2 [ 1 // 4; 7 // 12 ]
  else if
    List.fold_left ( && ) true
      (List.map (fun ineq -> Lin_ineq.is_satisfied ineq point) ineqs3)
  then Cond_lin_expr.construct ineqs3 [ zero; 5 // 6 ]
  else failwith "Bad point"

let () =
  let cle = Algorithm.eval 1 local_alg1 (Point.from_array [||]) in
  Lin_ineq.print_many (Cond_lin_expr.constraints cle);
  Lin_expr.print (Cond_lin_expr.expr cle)
