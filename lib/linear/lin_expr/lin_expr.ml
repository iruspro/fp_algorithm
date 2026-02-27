open Common

type t = { expr : Q.t list (* [q_n; ...; q_1; q_0] *) }

(* CONSTRUCTORS *)
let from_list list =
  let rec normalize = function
    (* Remove leading zeros *)
    | [ q_0 ] -> [ q_0 ]
    | q :: qs when not (Q.equal q Q.zero) -> q :: qs
    | _ :: qs -> normalize qs
    | [] ->
        invalid_arg
          "Lin_expr.from_list: empty coefficient list (expected [q_n; ...; \
           q_0])"
  in
  { expr = normalize list }

let const q = from_list [ q ]

let x i =
  if i < 1 then invalid_arg "Lin_expr.x: variable index must be >= 1"
  else
    let zeros = List.init i (fun _ -> Q.zero) in
    from_list (Q.one :: zeros)

(* GETTERS *)

(** [as_list expr] returns the non empty list of coefficients of [expr] in the
    form [q_n; ...; q_1; q_0], where [q_n <> 0] if [n > 0]. *)
let as_list expr = expr.expr

(** [as_rev_list expr] returns the list of coefficients of [expr] in the form
    [q_0; q_1; ...; q_n], where [q_n <> 0] if [n > 0] *)
let as_rev_list expr = List.rev expr.expr

let dim expr = List.length expr.expr - 1

let leading_coeff expr =
  match as_list expr with q :: _ -> q | [] -> assert false

let coeff expr i =
  if i < 0 then invalid_arg "Lin_expr.coeff: coefficient index must be >= 0"
  else
    let dim = dim expr in
    let rec aux j = function
      | [] -> Q.zero (* i > dim case*)
      | q :: _ when j = dim - i -> q
      | _ :: qs -> aux (succ j) qs
    in
    aux 0 (as_list expr)

(* OPERATORS *)
let equal expr1 expr2 =
  let rec aux = function
    | [], [] -> true
    | [], _ | _, [] -> false
    | q :: _, r :: _ when not (Q.equal q r) -> false
    | _ :: qs, _ :: rs -> aux (qs, rs)
  in
  aux (as_rev_list expr1, as_rev_list expr2)

let mul_by q expr = from_list (List.map (fun q_i -> Q.mul q q_i) (as_list expr))

let add expr1 expr2 =
  let rec add_last expr = function
    | [] -> expr
    | r :: rs -> add_last (r :: expr) rs
  in
  let rec aux acc = function
    | [], [] -> acc
    | qs, [] | [], qs -> add_last acc qs
    | q :: qs, r :: rs -> aux (Q.add q r :: acc) (qs, rs)
  in
  from_list (aux [] (as_rev_list expr1, as_rev_list expr2))

let sub expr1 expr2 = add expr1 (mul_by Q.minus_one expr2)

let substitute expr1 i expr2 =
  if i < 1 then invalid_arg "Lin_expr.substitute: variable index must be >= 1"
  else
    let q_i = coeff expr1 i in
    add (sub expr1 (mul_by q_i (x i))) (mul_by q_i expr2)

(* FUNCTIONS *)
let eval expr point =
  let rec aux acc = function
    | [], [] | [], _ -> acc
    | _, [] ->
        invalid_arg
          "Lin_expr.eval: dimension mismatch (expression and point must have \
           the same dimension)"
    | q :: qs, r :: rs -> aux (Q.add acc (Q.mul q r)) (qs, rs)
  in
  match as_rev_list expr with
  | q_0 :: qs -> Q.add q_0 (aux Q.zero (qs, Point.as_list point))
  | [] -> assert false

(* PRINT *)
let to_string expr =
  let q_to_sign q = if Q.geq q Q.zero then "+" else "-" in

  let q_to_coeff q =
    let sep = if Z.equal (Q.den q) Z.one then "" else " " in
    if Q.equal (Q.abs q) Q.one then "" else Q.to_string (Q.abs q) ^ sep
  in

  let dim = dim expr in
  if dim = 0 then Q.to_string (leading_coeff expr)
  else
    let rec aux acc = function
      | [] -> acc
      | q :: qs when Q.equal q Q.zero -> aux acc qs (* Skip zeros *)
      | [ q_0 ] ->
          (* Add non-zero constant to non-constant linear expression *)
          aux (acc ^ " " ^ q_to_sign q_0 ^ " " ^ Q.to_string (Q.abs q_0)) []
      | q :: qs when List.length qs = dim ->
          (* Add first element of non-constant linear expression *)
          let sign = if Q.lt q Q.zero then "-" else "" in
          aux (acc ^ sign ^ q_to_coeff q ^ "x" ^ Pprint.int_to_subscript dim) qs
      | q :: qs ->
          (* Add i-th element of linear expression *)
          let i = List.length qs in
          aux
            (acc ^ " " ^ q_to_sign q ^ " " ^ q_to_coeff q ^ "x"
           ^ Pprint.int_to_subscript i)
            qs
    in
    aux "" (as_list expr)

let print expr = print_string (to_string expr)
