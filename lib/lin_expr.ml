type t = Q.t list (* [q_n; ...; q_1; q_0] *)

let zero dim = List.init (succ dim) (fun _ -> Q.zero)

let eval (expr : t) (point : Point.t) =
  let rec aux acc = function
    | [ q_0 ], [] -> Q.add acc q_0
    | _, [] | [], _ ->
        failwith "Point and linear expression have different dimensions"
    | q :: qs, r :: rs -> aux (Q.add acc (Q.mul q r)) (qs, rs)
  in
  aux Q.zero (expr, point)

let sub_last (expr1 : t) (expr2 : t) : t =
  let substituted =
    match (expr1, expr2) with
    | [], [] -> []
    | [], _ | _, [] -> failwith "Linear expressions have different dimensions"
    | q_n :: qs, r_n :: rs ->
        let rec aux acc = function
          | [], [] -> acc
          | _, [] | [], _ ->
              failwith "Linear expressions have different dimensions"
          | q :: qs, r :: rs -> aux (Q.add (Q.mul q_n r) q :: acc) (qs, rs)
        in
        aux [ Q.mul q_n r_n ] (qs, rs)
  in
  List.rev substituted

let mul_by c = List.map (fun q -> Q.mul c q)

(* Tests *)

(* zero *)
let%test "zero" = zero 0 = [ Q.zero ] && zero 2 = [ Q.zero; Q.zero; Q.zero ]

(* eval *)
let%test "eval" =
  let expr1 = zero 5
  and point1 = List.init 5 (fun _ -> Q.of_int (Random.full_int 100)) in
  let expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] and point2 = [ Q.of_int 2; Q.of_int 10 ] in
  let expr3 = [ Q.of_int 0; Q.of_int 10 ]
  and point3 = List.init 1 (fun _ -> Q.of_int (Random.full_int 100)) in
  eval expr1 point1 = Q.zero
  && eval expr2 point2 = Q.of_int 25
  && eval expr3 point3 = Q.of_int 10

(* sub_last *)
let%test "sub_last" =
  let expr1 = [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 5 ] in
  sub_last expr1 expr2 = [ Q.of_int 5; Q.of_int 12; Q.of_int 26 ]

(* mul_by *)
let%test "mul_by" =
  let expr = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  mul_by (Q.of_int 10) expr = [ Q.of_int 10; Q.of_int 20; Q.of_int 30 ]
