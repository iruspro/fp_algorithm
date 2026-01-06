type rel = LessThan | LessEqual | GreaterThan | GreaterEqual

type n_type =
  | WithoutLast
  | LastGreaterThan
  | LastGreaterEqual
  | LastLessThan
  | LastLessEqual

type t = { lhs : Lin_expr.t; rhs : Lin_expr.t; rel : rel; n_type : n_type }

let lhs ineq = ineq.lhs
let rhs ineq = ineq.rhs
let rel ineq = ineq.rel
let n_type ineq = ineq.n_type

let change_rel = function
  | LessThan -> GreaterThan
  | LessEqual -> GreaterEqual
  | GreaterThan -> LessThan
  | GreaterEqual -> LessEqual

let rel_to_n_type = function
  | LessThan -> LastLessThan
  | LessEqual -> LastLessEqual
  | GreaterThan -> LastGreaterThan
  | GreaterEqual -> LastGreaterEqual

let compare val1 val2 rel =
  match rel with
  | LessThan -> val1 < val2
  | LessEqual -> val1 <= val2
  | GreaterThan -> val1 > val2
  | GreaterEqual -> val1 >= val2

let construct dim lhs rhs rel =
  let x_n = Q.one :: Lin_expr.zero (pred dim) in
  match (lhs, rhs) with
  | [ _ ], [ _ ] -> { lhs; rhs; rel; n_type = WithoutLast }
  | q_n :: qs, r_n :: rs when (q_n = Q.zero && r_n = Q.zero) || q_n = r_n ->
      { lhs = Q.zero :: qs; rhs = Q.zero :: rs; rel; n_type = WithoutLast }
  | q_n :: qs, r_n :: rs when q_n > r_n ->
      let lhs = Lin_expr.mul_by (Q.sub q_n r_n) x_n
      and rhs =
        Q.zero :: Lin_expr.add rs (Lin_expr.mul_by (Q.of_int (-1)) qs)
      in
      { lhs; rhs; rel; n_type = rel_to_n_type rel }
  | q_n :: qs, r_n :: rs (* q_n < r_n case *) ->
      let lhs = Lin_expr.mul_by (Q.sub r_n q_n) x_n
      and rhs = Q.zero :: Lin_expr.add qs (Lin_expr.mul_by (Q.of_int (-1)) rs)
      and rel = change_rel rel in
      { lhs; rhs; rel; n_type = rel_to_n_type rel }
  | _ -> failwith "A linear expression must have at least one coefficient"

let make_constraints dim lhs all_rhs rel =
  let rec aux acc = function
    | [] -> acc
    | rhs :: all_rhs -> aux (construct dim lhs rhs rel :: acc) all_rhs
  in
  aux [] all_rhs

let is_satisfied ineq point =
  let val1 = Lin_expr.eval (lhs ineq) point
  and val2 = Lin_expr.eval (rhs ineq) point in
  compare val1 val2 (rel ineq)

let rec find_unsatisfied ineqs point =
  match ineqs with
  | [] -> None
  | ineq :: ineqs ->
      if not (is_satisfied ineq point) then Some ineq
      else find_unsatisfied ineqs point

let negate ineq =
  let rel =
    match rel ineq with
    | LessThan -> GreaterEqual
    | LessEqual -> GreaterThan
    | GreaterThan -> LessEqual
    | GreaterEqual -> LessThan
  and n_type =
    match n_type ineq with
    | WithoutLast -> WithoutLast
    | LastGreaterThan -> LastLessEqual
    | LastGreaterEqual -> LastLessThan
    | LastLessThan -> LastGreaterEqual
    | LastLessEqual -> LastGreaterThan
  in
  { ineq with rel; n_type }

let extract_rh_sides = List.map (fun ineq -> ineq.rhs)

(* Tests *)
(* construct *)
let%test "construct 1 dim" =
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
  List.hd (lhs ineq1) = Q.zero
  && List.hd (rhs ineq1) = Q.zero
  && n_type ineq1 = WithoutLast
  && List.hd (lhs ineq2) = Q.zero
  && List.hd (rhs ineq2) = Q.zero
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
        && lhs ineq = [ Q.one; Q.zero; Q.zero ]
        && rhs ineq = [ Q.zero; Q.of_int (-1); Q.of_int (-1) ]
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
        && lhs ineq = [ Q.one; Q.zero; Q.zero ]
        && rhs ineq = [ Q.zero; Q.one; Q.one ]
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
  match res with None -> false | Some ineq -> true && lhs ineq = lhs fail

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
  && List.mem [ Q.zero; Q.one; Q.zero ] rh_sides
