type t = { expr : Q.t list (* [q_n; ...; q_1; q_0] *); dim : int }

(* Constants *)
let dim_mismatch_err =
  "dimension mismatch (expressions must have the same dimension)"

let negative_dim_err = "dimension cannot be negative"

(* Constructors *)
let from_list list =
  let len = List.length list in
  if len = 0 then
    invalid_arg
      "Lin_expr.from_list: empty coefficient list (expected [q_n; ...; q_0])"
  else { expr = list; dim = len - 1 }

(* Getters *)
let as_list { expr; _ } = expr
let dim { dim; _ } = dim

let leading_coeff { expr; _ } =
  (* invariant: expr is non-empty *)
  List.hd expr

let not_leading_coeffs { expr; _ } =
  (* invariant: expr is non-empty *)
  List.tl expr

(* Special expressions *)
let zero dim =
  if dim < 0 then invalid_arg ("Lin_expr.zero: " ^ negative_dim_err)
  else from_list (List.init (succ dim) (fun _ -> Q.zero))

let one dim =
  if dim < 0 then invalid_arg ("Lin_expr.one: " ^ negative_dim_err)
  else
    from_list
      (List.init (succ dim) (fun i -> if i = dim then Q.one else Q.zero))

let x dim i =
  if dim < 1 then invalid_arg "Lin_expr.x: dimension must be >= 1"
  else if i < 1 then invalid_arg "Lin_expr.x: variable index must be >= 1"
  else if i > dim then invalid_arg "Lin_expr.x: variable index must be <= dim"
  else
    from_list
      (List.init (succ dim) (fun j -> if j = dim - i then Q.one else Q.zero))

(* Operators *)
let equal expr1 expr2 =
  if dim expr1 <> dim expr2 then
    invalid_arg ("Lin_expr.equal: " ^ dim_mismatch_err)
  else
    List.fold_left ( && ) true
      (List.map2 (fun q r -> Q.equal q r) (as_list expr1) (as_list expr2))

let mul_by c expr =
  { expr with expr = List.map (fun q -> Q.mul c q) (as_list expr) }

let add expr1 expr2 =
  if dim expr1 <> dim expr2 then
    invalid_arg ("Lin_expr.add: " ^ dim_mismatch_err)
  else
    let rec aux acc = function
      | [], [] -> List.rev acc
      | q :: qs, r :: rs -> aux (Q.add q r :: acc) (qs, rs)
      | _, [] | [], _ -> assert false
    in
    from_list (aux [] (as_list expr1, as_list expr2))

let sub expr1 expr2 =
  if dim expr1 <> dim expr2 then
    invalid_arg ("Lin_expr.sub: " ^ dim_mismatch_err)
  else add expr1 (mul_by Q.minus_one expr2)

(* Functions *)
let eval expr point =
  if dim expr <> Point.dim point then
    invalid_arg
      "Lin_expr.eval: dimension mismatch (expression and point must have the \
       same dimension)"
  else
    let rec aux acc = function
      | [ q_0 ], [] -> Q.add acc q_0
      | q :: qs, r :: rs -> aux (Q.add acc (Q.mul q r)) (qs, rs)
      | _, [] | [], _ -> assert false
    in
    aux Q.zero (as_list expr, Point.as_reversed_list point)

let sub_last expr1 expr2 : t =
  if dim expr1 <> dim expr2 then
    invalid_arg ("Lin_expr.sub_last: " ^ dim_mismatch_err)
  else if dim expr1 = 0 then expr1
    (* no substitution is possible for constant expressions *)
  else
    let q_n = leading_coeff expr1 and r_n = leading_coeff expr2 in
    let rec aux acc = function
      | [], [] -> List.rev acc
      | q :: qs, r :: rs -> aux (Q.add (Q.mul q_n r) q :: acc) (qs, rs)
      | _, [] | [], _ -> assert false
    in
    (* invariant: as_list expr1 is non-empty *)
    from_list
      (aux
         [ Q.mul q_n r_n ]
         (not_leading_coeffs expr1, not_leading_coeffs expr2))

let reduce_dim expr =
  if dim expr = 0 then
    invalid_arg
      "Lin_expr.reduce_dim: Cannot reduce the dimension of a constant-only \
       linear expression"
  else from_list (not_leading_coeffs expr)

let extend_dim expr v = from_list (v :: as_list expr)

let find_supremum_term terms point =
  let rec aux c_term c_min = function
    | [] -> c_term
    | term :: terms ->
        let c_val = eval term point in
        if Q.lt c_val c_min then aux (Some term) c_val terms
        else aux c_term c_min terms
  in
  aux None Q.inf terms

let complement expr =
  let one = one (dim expr) in
  sub one expr

let flip_last expr =
  if dim expr = 0 then expr
  else
    let q_n = leading_coeff expr and coeffs' = not_leading_coeffs expr in
    let rec aux acc = function
      | [] -> List.rev acc
      | [ q_0 ] -> aux (Q.add q_0 q_n :: acc) []
      | q :: qs -> aux (q :: acc) qs
    in
    from_list (aux [ Q.neg q_n ] coeffs')

(* Print *)
let to_string expr =
  let is_const expr =
    let rec aux acc = function
      | [ _ ] -> acc
      | q :: qs -> aux (acc && Q.equal Q.zero q) qs
      | [] -> assert false
    in
    aux true expr
  in

  let q_to_sign q = if Q.geq q Q.zero then "+" else "-" in

  let q_to_coeff q =
    let sep = if Z.equal (Q.den q) Z.one then "" else " " in
    if Q.equal (Q.abs q) Q.one then "" else Q.to_string (Q.abs q) ^ sep
  in

  let int_to_subscript n =
    let subs = [| "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉" |] in
    let rec aux acc = function
      | 0 -> acc
      | n ->
          let digit = n mod 10 in
          aux (subs.(digit) ^ acc) (n / 10)
    in
    if n = 0 then subs.(0) else aux "" n
  in

  if is_const (as_list expr) then
    Q.to_string (List.hd (List.rev (as_list expr)))
  else
    let first = ref true in
    let rec aux acc = function
      | [] -> acc
      | q :: qs when Q.equal q Q.zero -> aux acc qs (* Skip zeros *)
      | [ q_0 ] ->
          (* Add non-zero constant to non-constant linear expression *)
          aux (acc ^ " " ^ q_to_sign q_0 ^ " " ^ Q.to_string (Q.abs q_0)) []
      | q :: qs when List.length qs = dim expr || !first ->
          (* Add first element of linear expression *)
          first := false;

          let i = List.length qs and sign = if Q.lt q Q.zero then "-" else "" in
          aux (acc ^ sign ^ q_to_coeff q ^ "x" ^ int_to_subscript i) qs
      | q :: qs ->
          (* Add i-th element of linear expression *)
          let i = List.length qs in
          aux
            (acc ^ " " ^ q_to_sign q ^ " " ^ q_to_coeff q ^ "x"
           ^ int_to_subscript i)
            qs
    in
    aux "" (as_list expr)

let print expr = print_string (to_string expr)
