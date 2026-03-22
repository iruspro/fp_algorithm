let int_to_subscript n =
  if n < 0 then
    invalid_arg "Pprint.int_to_subscript: argument must be non-negative";
  let subs = [| "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉" |] in
  let rec aux acc = function
    | 0 -> acc
    | n ->
        let digit = n mod 10 in
        aux (subs.(digit) ^ acc) (n / 10)
  in
  if n = 0 then subs.(0) else aux "" n
