type t = Q.t list (* [q_n; ...; q_1; q_0] *)

let zero dim : t = List.init (succ dim) (fun _ -> Q.zero)

let one dim : t =
  List.init (succ dim) (fun i -> if i = dim then Q.one else Q.zero)

let eval (expr : t) point =
  let rec aux acc = function
    | [ q_0 ], [] -> Q.add acc q_0
    | _, [] | [], _ ->
        failwith "Point and linear expression have different dimensions"
    | q :: qs, r :: rs -> aux (Q.add acc (Q.mul q r)) (qs, rs)
  in
  aux Q.zero (expr, Point.as_list point)

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

let find_supremum_term terms point =
  let rec aux c_term c_min = function
    | [] -> c_term
    | term :: terms ->
        let c_val = eval term point in
        if Q.lt c_val c_min then aux (Some term) c_val terms
        else aux c_term c_min terms
  in
  aux None Q.inf terms

let equal (expr1 : t) (expr2 : t) =
  List.fold_left ( && ) true (List.map2 (fun q r -> Q.equal q r) expr1 expr2)

let add (expr1 : t) (expr2 : t) : t =
  let rec aux acc = function
    | [], [] -> acc
    | _, [] | [], _ -> failwith "Linear expressions have different dimensions"
    | q :: qs, r :: rs -> aux (Q.add q r :: acc) (qs, rs)
  in
  List.rev (aux [] (expr1, expr2))

let mul_by c (expr : t) : t = List.map (fun q -> Q.mul c q) expr

(* Print *)
let int_to_subscript n =
  let subs = [| "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉" |] in
  let rec aux acc = function
    | 0 -> acc
    | n ->
        let digit = n mod 10 in
        aux (subs.(digit) ^ acc) (n / 10)
  in
  if n = 0 then subs.(0) else aux "" n

let q_to_sign q = if Q.geq q Q.zero then "+" else "-"

let q_to_coeff q =
  let end_ = if Z.equal (Q.den q) Z.one then "" else " " in
  if Q.equal (Q.abs q) Q.one then "" else Q.to_string (Q.abs q) ^ end_

let to_string (expr : t) =
  let dim = List.length expr - 1 in
  if dim = 0 then Q.to_string (List.hd expr)
  else
    let first = ref true in
    let rec aux acc = function
      | [] -> acc
      | q :: qs when Q.equal q Q.zero -> aux acc qs
      | [ q_0 ] ->
          aux (acc ^ " " ^ q_to_sign q_0 ^ " " ^ Q.to_string (Q.abs q_0)) []
      | q :: qs when List.length qs = dim || !first ->
          let i = List.length qs in
          let sign = if Q.lt q Q.zero then "-" else "" in
          first := false;
          aux (acc ^ sign ^ q_to_coeff q ^ "x" ^ int_to_subscript i) qs
      | q :: qs ->
          let i = List.length qs in
          aux
            (acc ^ " " ^ q_to_sign q ^ " " ^ q_to_coeff q ^ "x"
           ^ int_to_subscript i)
            qs
    in
    aux "" expr

let print expr = print_endline (to_string expr)

(* Tests *)

(* zero *)
let%test "zero" = equal (zero 2) [ Q.zero; Q.zero; Q.zero ]

(* one *)
let%test "one" = equal (one 2) [ Q.zero; Q.zero; Q.one ]

(* eval *)
let%test "eval" =
  (* 0 at random point *)
  let expr1 = zero 5
  and point1 =
    Point.from_array (Array.init 5 (fun _ -> Q.of_int (Random.full_int 100)))
  in
  (* y + 2x + 3 at (2; 10.5)*)
  let expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point2 = Point.from_array [| Q.of_int 2; Q.( // ) 21 2 |] in
  (* 0*x + 10 at random point *)
  let expr3 = [ Q.of_int 0; Q.of_int 10 ]
  and point3 =
    Point.from_array (Array.init 1 (fun _ -> Q.of_float (Random.float 100.)))
  in
  Q.equal (eval expr1 point1) Q.zero
  && Q.equal (eval expr2 point2) (Q.( // ) 35 2)
  && Q.equal (eval expr3 point3) (Q.of_int 10)

(* sub_last *)
let%test "sub_last" =
  (* 5 y + 2 x + 1 with 2y + 2x + 5 -> 10 y + 12x + 26 *)
  let expr1 = [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = [ Q.of_int 2; Q.of_int 2; Q.of_int 5 ] in
  equal (sub_last expr1 expr2) [ Q.of_int 10; Q.of_int 12; Q.of_int 26 ]

(* reduce_dim *)
let%test "reduce_dim" =
  (* 0*y + 5x + 1 -> 5x + 1 *)
  equal
    (reduce_dim [ Q.zero; Q.of_int 5; Q.of_int 1 ])
    [ Q.of_int 5; Q.of_int 1 ]

(* add *)
let%test "add" =
  (* (y + x + 1) + (y + 2x + 3) = 2y + 3x + 4 *)
  let expr1 = List.init 3 (fun _ -> Q.of_int 1)
  and expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  equal (add expr1 expr2) [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ]

(* mul_by *)
let%test "mul_by" =
  (* 10 * (y + 2x + 3) = 10y + 20x + 30 *)
  let expr = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  equal (mul_by (Q.of_int 10) expr) [ Q.of_int 10; Q.of_int 20; Q.of_int 30 ]
