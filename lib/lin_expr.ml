type t = Q.t list (* [q_n, ..., q_1, q_0] *)

let zero n = List.init n (fun _ -> Q.zero)

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
