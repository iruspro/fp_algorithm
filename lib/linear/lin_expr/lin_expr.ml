type t = { expr : Q.t list (* [q_n; ...; q_1; q_0] *); dim : int }

(* Constants *)
let dim_mismatch_err =
  "dimension mismatch (expressions must have the same dimension)"

(* Constructors *)
let from_list list =
  let len = List.length list in
  if len = 0 then
    invalid_arg
      "Linear_expr.from_list: empty coefficient list (expected [q_n; ...; q_0])"
  else { expr = list; dim = len - 1 }

(* Getters *)
let as_list { expr; _ } = expr
let dim { dim; _ } = dim

(* Special expressions *)
let zero dim = { expr = List.init (succ dim) (fun _ -> Q.zero); dim }

let one dim =
  {
    expr = List.init (succ dim) (fun i -> if i = dim then Q.one else Q.zero);
    dim;
  }

let x dim i =
  {
    expr = List.init (succ dim) (fun j -> if j = dim - i then Q.one else Q.zero);
    dim;
  }

(* Operators *)
let equal expr1 expr2 =
  if expr1.dim <> expr2.dim then
    invalid_arg ("Linear_expr.equal: " ^ dim_mismatch_err)
  else
    List.fold_left ( && ) true
      (List.map2 (fun q r -> Q.equal q r) expr1.expr expr2.expr)

let mul_by c expr = { expr with expr = List.map (fun q -> Q.mul c q) expr.expr }

let add expr1 expr2 =
  if expr1.dim <> expr2.dim then
    invalid_arg ("Linear_expr.add: " ^ dim_mismatch_err)
  else
    let rec aux acc = function
      | [], [] -> List.rev acc
      | q :: qs, r :: rs -> aux (Q.add q r :: acc) (qs, rs)
      | _, [] | [], _ -> assert false
    in
    { expr1 with expr = aux [] (expr1.expr, expr2.expr) }

let sub expr1 expr2 =
  if expr1.dim <> expr2.dim then
    invalid_arg ("Linear_expr.sub: " ^ dim_mismatch_err)
  else add expr1 (mul_by Q.minus_one expr2)

(* Functions *)
let eval expr point =
  if expr.dim <> Point.dim point then
    invalid_arg
      "Linear_expr.eval: dimension mismatch (expression and point must have \
       the same dimension)"
  else
    let rec aux acc = function
      | [ q_0 ], [] -> Q.add acc q_0
      | q :: qs, r :: rs -> aux (Q.add acc (Q.mul q r)) (qs, rs)
      | _, [] | [], _ -> assert false
    in
    aux Q.zero (expr.expr, Point.as_reversed_list point)

let sub_last expr1 expr2 : t =
  if expr1.dim <> expr2.dim then
    invalid_arg ("Linear_expr.sub_last: " ^ dim_mismatch_err)
  else if expr1.dim = 0 then expr1
    (* no substitution is possible for constant expressions *)
  else
    (* invariant: expr1.expr is non-empty *)
    let q_n = List.hd expr1.expr and r_n = List.hd expr2.expr in
    let rec aux acc = function
      | [], [] -> List.rev acc
      | q :: qs, r :: rs -> aux (Q.add (Q.mul q_n r) q :: acc) (qs, rs)
      | _, [] | [], _ -> assert false
    in
    (* invariant: expr1.expr is non-empty *)
    let expr = aux [ Q.mul q_n r_n ] (List.tl expr1.expr, List.tl expr2.expr) in
    { expr1 with expr }

let reduce_dim expr =
  if expr.dim = 0 then
    invalid_arg
      "Lin_expr.reduce_dim: Cannot reduce the dimension of a constant-only \
       linear expression"
  else
    {
      expr = List.tl expr.expr (* invariant: expr1.expr is non-empty *);
      dim = pred expr.dim;
    }

let extend_dim expr v = { expr = v :: expr.expr; dim = succ expr.dim }

let find_supremum_term terms point =
  let rec aux c_term c_min = function
    | [] -> c_term
    | term :: terms ->
        let c_val = eval term point in
        if Q.lt c_val c_min then aux (Some term) c_val terms
        else aux c_term c_min terms
  in
  aux None Q.inf terms

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

  if is_const expr.expr then Q.to_string (List.hd (List.rev expr.expr))
  else
    let first = ref true in
    let rec aux acc = function
      | [] -> acc
      | q :: qs when Q.equal q Q.zero -> aux acc qs (* Skip zeros *)
      | [ q_0 ] ->
          (* Add non-zero constant to non-constant linear expression *)
          aux (acc ^ " " ^ q_to_sign q_0 ^ " " ^ Q.to_string (Q.abs q_0)) []
      | q :: qs when List.length qs = expr.dim || !first ->
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
    aux "" expr.expr

let print expr = print_string (to_string expr)
