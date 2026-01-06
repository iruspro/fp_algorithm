type t = Q.t list (* [q_n; ...; q_1; q_0] *)

let zero dim : t = List.init (succ dim) (fun _ -> Q.zero)

let eval (expr : t) (point : Point.t) =
  let rec aux acc = function
    | [ q_0 ], [] -> Q.add acc q_0
    | _, [] | [], _ ->
        failwith "Point and linear expression have different dimensions"
    | q :: qs, r :: rs -> aux (Q.add acc (Q.mul q r)) (qs, rs)
  in
  aux Q.zero (expr, point)

let sub_last (expr1 : t) (expr2 : t) : t =
  let rec aux acc q_n = function
    | [], [] -> acc
    | _, [] | [], _ -> failwith "Linear expressions have different dimensions"
    | q :: qs, r :: rs -> aux (Q.add (Q.mul q_n r) q :: acc) q_n (qs, rs)
  in
  List.rev
    (match (expr1, expr2) with
    | [], [] ->
        failwith "A linear expression must have at least one coefficient"
    | [], _ | _, [] -> failwith "Linear expressions have different dimensions"
    | q_n :: qs, r_n :: rs -> aux [ Q.mul q_n r_n ] q_n (qs, rs))

let reduce_dim = function
  | [] -> failwith "A linear expression must have at least one coefficient"
  | [ _ ] ->
      failwith
        "Cannot reduce the dimension of a constant-only linear expression"
  | _ :: (xs : t) -> xs

let add (expr1 : t) (expr2 : t) : t =
  let rec aux acc = function
    | [], [] -> acc
    | _, [] | [], _ -> failwith "Linear expressions have different dimensions"
    | q :: qs, r :: rs -> aux (Q.add q r :: acc) (qs, rs)
  in
  List.rev (aux [] (expr1, expr2))

let mul_by c (expr : t) : t = List.map (fun q -> Q.mul c q) expr

(* Tests *)

(* zero *)
let%test "zero" = zero 2 = [ Q.zero; Q.zero; Q.zero ]

(* eval *)
let%test "eval" =
  (* 0 at random point *)
  let expr1 = zero 5
  and point1 = List.init 5 (fun _ -> Q.of_int (Random.full_int 100)) in
  (* y + 2x + 3 at (2; 10)*)
  let expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point2 = Point.from_list [ Q.of_int 2; Q.of_int 10 ] in
  (* 0*x + 10 at random point *)
  let expr3 = [ Q.of_int 0; Q.of_int 10 ]
  and point3 = List.init 1 (fun _ -> Q.of_int (Random.full_int 100)) in
  eval expr1 point1 = Q.zero
  && eval expr2 point2 = Q.of_int 17
  && eval expr3 point3 = Q.of_int 10

(* sub_last *)
let%test "sub_last" =
  (* 5 y + 2 x + 1 with 2y + 2x + 5 -> 10 y + 12x + 26 *)
  let expr1 = [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = [ Q.of_int 2; Q.of_int 2; Q.of_int 5 ] in
  sub_last expr1 expr2 = [ Q.of_int 10; Q.of_int 12; Q.of_int 26 ]

(* reduce_dim *)
let%test "reduce_dim" =
  (* 0*y + 5x + 1 -> 5x + 1 *)
  reduce_dim [ Q.zero; Q.of_int 5; Q.of_int 1 ] = [ Q.of_int 5; Q.of_int 1 ]

(* add *)
let%test "add" =
  (* (y + x + 1) + (y + 2x + 3) = 2y + 3x + 4 *)
  let expr1 = List.init 3 (fun _ -> Q.of_int 1)
  and expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  add expr1 expr2 = [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ]

(* mul_by *)
let%test "mul_by" =
  (* 10 * (y + 2x + 3) = 10y + 20x + 30 *)
  let expr = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  mul_by (Q.of_int 10) expr = [ Q.of_int 10; Q.of_int 20; Q.of_int 30 ]
