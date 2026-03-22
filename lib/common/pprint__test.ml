open Pprint

let%expect_test "int_to_subscript: zero" =
  print_string (int_to_subscript 0);
  [%expect {| ₀ |}]

let%expect_test "int_to_subscript: single digit" =
  print_string (int_to_subscript 7);
  [%expect {| ₇ |}]

let%expect_test "int_to_subscript: multi-digit" =
  print_string (int_to_subscript 42);
  [%expect {| ₄₂ |}]

let%expect_test "int_to_subscript: large number" =
  print_string (int_to_subscript 1234567890);
  [%expect {| ₁₂₃₄₅₆₇₈₉₀ |}]

let%expect_test "int_to_subscript: negative raises" =
  (try ignore (int_to_subscript (-1))
   with Invalid_argument msg -> print_string msg);
  [%expect {| Pprint.int_to_subscript: argument must be non-negative |}]
