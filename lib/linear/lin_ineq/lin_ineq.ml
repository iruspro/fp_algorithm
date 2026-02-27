type rel = Lt | Le | Gt | Ge
type t = { lhs : Lin_expr.t; rhs : Lin_expr.t; rel : rel; dim : int }

let rev_rel = function Lt -> Gt | Le -> Ge | Gt -> Lt | Ge -> Le
let neg_rel = function Lt -> Ge | Le -> Gt | Gt -> Le | Ge -> Lt

let compare val1 val2 = function
  | Lt -> Q.lt val1 val2
  | Le -> Q.leq val1 val2
  | Gt -> Q.gt val1 val2
  | Ge -> Q.geq val1 val2

(* CONSTRUCTORS *)
let construct lhs rhs rel =
  (* lhs □ rhs <=> 0 □ rhs - lhs: q_s x_s + ... + q_1 x_1 + q_0 <=> 
     -q_s x_s □ q_{s-1} x_{s-1} + ... q_1 x_1 + q_0 <=> 
      x_s □ -1/q_s (q_{s-1} x_{s-1} + ... q_1 x_1 + q_0) *)
  let rhs = Lin_expr.sub rhs lhs in
  let dim = Lin_expr.dim rhs and leading_coeff = Lin_expr.leading_coeff rhs in
  if dim = 0 then { lhs = Lin_expr.const Q.zero; rhs; rel; dim }
  else
    let x =
      Lin_expr.x dim
      (* x_s *)
    in
    let rhs = Lin_expr.sub rhs (Lin_expr.mul_by leading_coeff x) in
    (* Invariant: leading_coeff <> 0 if dim > 0 *)
    let rhs = Lin_expr.mul_by (Q.neg (Q.inv leading_coeff)) rhs
    and rel = if Q.geq (Q.neg leading_coeff) Q.zero then rel else rev_rel rel in
    { lhs = x; rhs; rel; dim }

(* GETTERS *)
let lhs ineq = ineq.lhs
let rhs ineq = ineq.rhs
let rel ineq = ineq.rel
let dim ineq = ineq.dim

(* OPERATORS *)
let negate ineq = construct (lhs ineq) (rhs ineq) (neg_rel (rel ineq))

let substitute ineq i expr =
  if i < 1 then invalid_arg "Lin_ineq.substitute: variable index must be >= 1"
  else if i > dim ineq then ineq
  else if i = dim ineq then
    (* Invariant: dim rhs < dim ineq if dim ineq >= 1 *)
    let lhs = Lin_expr.substitute (lhs ineq) i expr in
    construct lhs (rhs ineq) (rel ineq)
  else
    (* Invariant: lhs = x_{dim ineq} *)
    let rhs = Lin_expr.substitute (rhs ineq) i expr in
    construct (lhs ineq) rhs (rel ineq)

(* FUNCTIONS *)
let is_satisfied ineq point =
  let val1 = Lin_expr.eval (lhs ineq) point
  and val2 = Lin_expr.eval (rhs ineq) point in
  compare val1 val2 (rel ineq)

(* PRINT *)
let rel_to_string = function Lt -> "<" | Le -> "≤" | Gt -> ">" | Ge -> "≥"

let to_string ineq =
  Lin_expr.to_string (lhs ineq)
  ^ " "
  ^ rel_to_string (rel ineq)
  ^ " "
  ^ Lin_expr.to_string (rhs ineq)

let print ineq = print_string (to_string ineq)

(* TESTS *)
(* construct *)
(* let%test "construct 1 dim" =
  let ineq = construct 1 [ Q.one ] [ Q.zero ] LessThan in
  n_type ineq = WithoutLast && rel ineq = LessThan

let%test "construct n dim WithoutLast" =
  let ineq1 (* 5y + 3x + 2 > 5y + 2x + 1 -> 3x + 2 > 2x + 1 *) =
    construct 2
      [ Q.of_int 5; Q.of_int 3; Q.of_int 2 ]
      [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
      GreaterThan
  and ineq2 (* 0*y + 3x + 2 > 2x + 1 -> 0*y + 3x + 2 > 2x + 1 *) =
    construct 2
      [ Q.of_int 0; Q.of_int 3; Q.of_int 2 ]
      [ Q.of_int 0; Q.of_int 2; Q.of_int 1 ]
      GreaterThan
  in
  Q.equal (List.hd (lhs ineq1)) Q.zero
  && Q.equal (List.hd (rhs ineq1)) Q.zero
  && n_type ineq1 = WithoutLast
  && Q.equal (List.hd (lhs ineq2)) Q.zero
  && Q.equal (List.hd (rhs ineq2)) Q.zero
  && n_type ineq2 = WithoutLast

let%test "construct n dim Last_ without change rel" =
  (* 6y + 3x + 2 V 5y + 2x + 1 -> y V -x - 1  *)
  let rels = [| LessThan; LessEqual; GreaterThan; GreaterEqual |]
  and n_types =
    [| LastLessThan; LastLessEqual; LastGreaterThan; LastGreaterEqual |]
  in
  let result = ref true in
  let _ =
    for i = 0 to Array.length rels - 1 do
      let rel_ = rels.(i) and n_type_ = n_types.(i) in
      let ineq =
        construct 2
          [ Q.of_int 6; Q.of_int 3; Q.of_int 2 ]
          [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
          rel_
      in
      result :=
        !result
        && Lin_expr.equal (lhs ineq) [ Q.one; Q.zero; Q.zero ]
        && Lin_expr.equal (rhs ineq) [ Q.zero; Q.of_int (-1); Q.of_int (-1) ]
        && rel ineq = rel_
        && n_type ineq = n_type_
    done
  in
  !result

let%test "construct n dim Last_ with change rel" =
  (* 4y + 3x + 2 V 5y + 2x + 1 -> y /\ x + 1  *)
  let rels = [| LessThan; LessEqual; GreaterThan; GreaterEqual |]
  and c_rels = [| GreaterThan; GreaterEqual; LessThan; LessEqual |]
  and n_types =
    [| LastGreaterThan; LastGreaterEqual; LastLessThan; LastLessEqual |]
  in
  let result = ref true in
  let _ =
    for i = 0 to Array.length rels - 1 do
      let rel1 = rels.(i) and rel2 = c_rels.(i) and n_type_ = n_types.(i) in
      let ineq =
        construct 2
          [ Q.of_int 4; Q.of_int 3; Q.of_int 2 ]
          [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
          rel1
      in
      result :=
        !result
        && Lin_expr.equal (lhs ineq) [ Q.one; Q.zero; Q.zero ]
        && Lin_expr.equal (rhs ineq) [ Q.zero; Q.one; Q.one ]
        && rel ineq = rel2
        && n_type ineq = n_type_
    done
  in
  !result

(* is_satisfied *)
let%test "is_satisfied always true" =
  (* 2 x_n + ... + 2 x_1 + 2 > x_n + ... + x_1 + 1*)
  let result = ref true in
  let dims = [| 1; 5; 10; 100 |] and n_cases = 100 in
  let rec aux acc dim lhs rhs rel = function
    | [] -> acc
    | point :: points ->
        let acc =
          match rel with
          | GreaterThan | GreaterEqual ->
              acc && is_satisfied (construct dim lhs rhs rel) point
          | LessThan | LessEqual ->
              acc || is_satisfied (construct dim lhs rhs rel) point
        in
        aux acc dim lhs rhs rel points
  in
  let _ =
    for i = 1 to pred (Array.length dims) do
      let dim = dims.(i) in
      let lhs = List.init (succ dim) (fun _ -> Q.of_int 2)
      and rhs = List.init (succ dim) (fun _ -> Q.of_int 1) in
      let points =
        List.init n_cases (fun _ ->
            Point.from_array
              (Array.init dim (fun _ -> Q.of_int (Random.full_int 100))))
      in
      result :=
        aux true dim lhs rhs GreaterThan points
        && aux true dim lhs rhs GreaterEqual points
        && (not (aux false dim lhs rhs LessEqual points))
        && not (aux false dim lhs rhs LessThan points)
    done
  in
  !result

let%test "is_satisfied equals" =
  (* 3 y + 2 x + 3 V 0 at (0, -1) *)
  let dim = 2 in
  let lhs = [ Q.of_int 3; Q.of_int 2; Q.of_int 3 ]
  and zero = Lin_expr.zero dim
  and point = Point.from_array [| Q.of_int 0; Q.of_int (-1) |] in
  is_satisfied (construct dim lhs zero LessEqual) point
  && is_satisfied (construct dim lhs zero GreaterEqual) point
  && (not (is_satisfied (construct dim lhs zero LessThan) point))
  && not (is_satisfied (construct dim lhs zero GreaterThan) point)

(* find_unsatisfied *)
let%test "find_unsatisfied success" =
  (* -2 y - 2 x - 2 > 0  at (1, 1)*)
  let dim = 2 in
  let zero = Lin_expr.zero dim in
  let fail =
    construct dim
      (List.init (succ dim) (fun _ -> Q.of_int (-2)))
      zero GreaterThan
  in
  let ineqs =
    [
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 2))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      fail;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 4))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-1)))
        zero GreaterThan;
    ]
  and point = Point.from_array [| Q.of_int 1; Q.of_int 1 |] in
  let res = find_unsatisfied ineqs point in
  match res with
  | None -> false
  | Some ineq -> true && Lin_expr.equal (lhs ineq) (lhs fail)

let%test "find_unsatisfied fail" =
  let dim = 2 in
  let zero = Lin_expr.zero dim in
  let ineqs =
    [
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 2))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-2)))
        zero LessThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-1)))
        zero LessThan;
    ]
  and point = Point.from_array [| Q.of_int 1; Q.of_int 1 |] in
  let res = find_unsatisfied ineqs point in
  match res with None -> true | Some _ -> false

(* negate *)
let%test "negate LessThan -> GreaterEqual and WithoutLast" =
  let lhs1 = [ Q.one ]
  and rhs1 = [ Q.zero ]
  and lhs2 = [ Q.one; Q.zero ]
  and rhs2 = [ Q.zero; Q.zero ] in
  let input1 = construct 1 lhs1 rhs1 LessThan
  and input2 = construct 2 lhs2 rhs2 LessThan in
  let ineq1 = negate input1 and ineq2 = negate input2 in
  rel ineq1 = GreaterEqual
  && n_type ineq1 = WithoutLast
  && rel ineq2 = GreaterEqual
  && n_type ineq2 = LastGreaterEqual

let%test "negate LessEqual -> GreaterThan" =
  let lhs = [ Q.one; Q.one ] and rhs = [ Q.zero; Q.zero ] in
  let input = construct 2 lhs rhs LessEqual in
  let ineq = negate input in
  rel ineq = GreaterThan && n_type ineq = LastGreaterThan

let%test "negate GreaterThan -> LessEqual" =
  let lhs = [ Q.one; Q.one ] and rhs = [ Q.zero; Q.zero ] in
  let input = construct 2 lhs rhs GreaterThan in
  let ineq = negate input in
  rel ineq = LessEqual && n_type ineq = LastLessEqual

let%test "negate GreaterEqual -> LessThan" =
  let lhs = [ Q.one; Q.one ] and rhs = [ Q.zero; Q.zero ] in
  let input = construct 2 lhs rhs GreaterEqual in
  let ineq = negate input in
  rel ineq = LessThan && n_type ineq = LastLessThan

(* extract_rh_sides *)
let%test "extract_rh_sides" =
  let ineqs =
    [
      construct 2 [ Q.zero; Q.zero; Q.zero ] [ Q.zero; Q.zero; Q.one ] LessThan;
      construct 2 [ Q.zero; Q.zero; Q.zero ] [ Q.zero; Q.one; Q.zero ] LessThan;
    ]
  in
  let rh_sides = extract_rh_sides ineqs in
  List.mem [ Q.zero; Q.zero; Q.one ] rh_sides
  && List.mem [ Q.zero; Q.one; Q.zero ] rh_sides *)
