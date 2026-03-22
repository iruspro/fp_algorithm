open Lin_expr_vec

let lin = Lin_expr.from_list

(* CONSTRUCTORS *)
let%test "from_array" =
  let v = from_array [| lin [ Q.one; Q.zero ]; lin [ Q.of_int 2 ] |] in
  Lin_expr.equal (expr v 1) (lin [ Q.one; Q.zero ])
  && Lin_expr.equal (expr v 2) (lin [ Q.of_int 2 ])

let%test "from_array empty raises" =
  try
    let _ = from_array [||] in
    false
  with Invalid_argument _ -> true

let%test "const" =
  let v = const [| Q.one; Q.zero; Q.of_int 3 |] in
  Lin_expr.equal (expr v 1) (Lin_expr.const Q.one)
  && Lin_expr.equal (expr v 2) (Lin_expr.const Q.zero)
  && Lin_expr.equal (expr v 3) (Lin_expr.const (Q.of_int 3))

let%test "const empty raises" =
  try
    let _ = const [||] in
    false
  with Invalid_argument _ -> true

(* GETTERS *)
let%test "dim" =
  let v = from_array [| lin [ Q.one; Q.zero ]; Lin_expr.x 3 |] in
  dim v = 3

let%test "dim const" =
  let v = const [| Q.one; Q.of_int 2 |] in
  dim v = 0

let%test "codim" =
  let v = from_array [| lin [ Q.one ]; Lin_expr.x 3; Lin_expr.x 1 |] in
  codim v = 3

let%test "codim single" =
  let v = from_array [| Lin_expr.const Q.zero |] in
  codim v = 1

let%test "expr out of bounds low" =
  let v = from_array [| lin [ Q.one ] |] in
  try
    let _ = expr v 0 in
    false
  with Invalid_argument _ -> true

let%test "expr out of bounds high" =
  let v = from_array [| lin [ Q.one ] |] in
  try
    let _ = expr v 2 in
    false
  with Invalid_argument _ -> true

(* OPERATORS *)
let%test "with_expr" =
  let v = from_array [| lin [ Q.one ]; lin [ Q.of_int 2 ] |] in
  let v' = with_expr v 2 (Lin_expr.x 3) in
  Lin_expr.equal (expr v' 1) (lin [ Q.one ])
  && Lin_expr.equal (expr v' 2) (Lin_expr.x 3)

let%test "with_expr does not mutate original" =
  let v = from_array [| lin [ Q.one ]; lin [ Q.of_int 2 ] |] in
  let _ = with_expr v 1 (Lin_expr.const Q.zero) in
  Lin_expr.equal (expr v 1) (lin [ Q.one ])

let%test "with_expr out of bounds low" =
  let v = from_array [| lin [ Q.one ] |] in
  try
    let _ = with_expr v 0 (Lin_expr.const Q.zero) in
    false
  with Invalid_argument _ -> true

let%test "with_expr out of bounds high" =
  let v = from_array [| lin [ Q.one ] |] in
  try
    let _ = with_expr v 2 (Lin_expr.const Q.zero) in
    false
  with Invalid_argument _ -> true

let%test "substitute" =
  (* v = (x₁ + 1, x₂), substitute x₁ := 3 in component 1 => (4, x₂) *)
  let v = from_array [| lin [ Q.one; Q.one ]; Lin_expr.x 2 |] in
  let v' = substitute v 1 1 (Lin_expr.const (Q.of_int 3)) in
  Lin_expr.equal (expr v' 1) (Lin_expr.const (Q.of_int 4))
  && Lin_expr.equal (expr v' 2) (Lin_expr.x 2)

let%test "substitute does not mutate original" =
  let v = from_array [| lin [ Q.one; Q.one ]; Lin_expr.x 2 |] in
  let _ = substitute v 1 1 (Lin_expr.const Q.zero) in
  Lin_expr.equal (expr v 1) (lin [ Q.one; Q.one ])

let%test "substitute k out of bounds" =
  let v = from_array [| lin [ Q.one ] |] in
  try
    let _ = substitute v 2 1 (Lin_expr.const Q.zero) in
    false
  with Invalid_argument _ -> true

let%test "substitute_from affects components from k" =
  (* (x₁ + x₂, 3x₂, x₂ + 1), substitute x₂ := 5 from component 2 *)
  let v =
    from_array
      [|
        lin [ Q.one; Q.one; Q.zero ];
        lin [ Q.of_int 3; Q.zero; Q.zero ];
        lin [ Q.one; Q.zero; Q.one ];
      |]
  in
  let v' = substitute_from v 2 2 (Lin_expr.const (Q.of_int 5)) in
  Lin_expr.equal (expr v' 1) (lin [ Q.one; Q.one; Q.zero ])
  && Lin_expr.equal (expr v' 2) (Lin_expr.const (Q.of_int 15))
  && Lin_expr.equal (expr v' 3) (Lin_expr.const (Q.of_int 6))

let%test "substitute_from all" =
  let v = from_array [| Lin_expr.x 1; lin [ Q.one; Q.one ] |] in
  let v' = substitute_from v 1 1 (Lin_expr.const Q.zero) in
  Lin_expr.equal (expr v' 1) (Lin_expr.const Q.zero)
  && Lin_expr.equal (expr v' 2) (Lin_expr.const Q.one)

let%test "substitute_from does not mutate original" =
  let v = from_array [| Lin_expr.x 1; Lin_expr.x 1 |] in
  let _ = substitute_from v 1 1 (Lin_expr.const Q.zero) in
  Lin_expr.equal (expr v 1) (Lin_expr.x 1)

let%test "substitute_from i < 1 raises" =
  let v = from_array [| Lin_expr.x 1 |] in
  try
    let _ = substitute_from v 1 0 (Lin_expr.const Q.zero) in
    false
  with Invalid_argument _ -> true

let%test "substitute i < 1 raises" =
  let v = from_array [| lin [ Q.one ] |] in
  try
    let _ = substitute v 1 0 (Lin_expr.const Q.zero) in
    false
  with Invalid_argument _ -> true

(* FUNCTIONS *)
let%test "eval" =
  (* v = (x₁ + 1, 2x₁), point = (3) => (4, 6) *)
  let v = from_array [| lin [ Q.one; Q.one ]; lin [ Q.of_int 2; Q.zero ] |] in
  let point = Point.from_list [ Q.of_int 3 ] in
  let result = eval v point in
  Q.equal result.(0) (Q.of_int 4) && Q.equal result.(1) (Q.of_int 6)

let%test "eval const" =
  let v = const [| Q.of_int 5; Q.of_int 7 |] in
  let point = Point.from_list [ Q.one ] in
  let result = eval v point in
  Q.equal result.(0) (Q.of_int 5) && Q.equal result.(1) (Q.of_int 7)

let%test "eval dim mismatch raises" =
  let v = from_array [| Lin_expr.x 3 |] in
  let point = Point.from_list [ Q.one ] in
  try
    let _ = eval v point in
    false
  with Invalid_argument _ -> true

(* PRINT *)
let%test "to_string" =
  let v = from_array [| Lin_expr.x 1; Lin_expr.const (Q.of_int 2) |] in
  to_string v = "(x₁, 2)"

let%test "to_string single" =
  let v = from_array [| Lin_expr.const Q.zero |] in
  to_string v = "(0)"
