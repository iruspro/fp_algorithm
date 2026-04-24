open Core
open Linear
open Mu_calc

let make_system dim terms : Local_alg_vec.t =
 fun point ->
  let cles = Array.map (fun t -> Mu_term.eval dim t point) terms in
  let all_constraints =
    Array.fold_left (fun acc cle -> Cle.constraints cle @ acc) [] cles
  in
  let exprs = Lin_expr_vec.from_array (Array.map Cle.expr cles) in
  Cle_vec.construct all_constraints exprs

(* n=10, m=2: sequential chain with self-dependencies and ⊔

   Each x_{m+k} depends on itself and the next bound variable x_{m+k+1},
   plus a free variable. Self-dependency forces lfp to do real work.
   Every 3rd component uses ⊔ (max) which can force lfp iteration
   when the branch taken at d=0 differs from the fixed point.

   x₃  = (1/3 x₃ ⊕ 1/4 x₁) ⊕ 1/4 x₄
   x₄  = (1/4 x₄ ⊕ 1/5 x₂) ⊕ 1/3 x₅
   x₅  = ((1/3 x₅ ⊕ 1/4 x₁) ⊕ 1/4 x₆) ⊔ 1/6
   x₆  = (1/4 x₆ ⊕ 1/3 x₂) ⊕ 1/5 x₇
   x₇  = (1/3 x₇ ⊕ 1/4 x₁) ⊕ 1/4 x₈
   x₈  = ((1/4 x₈ ⊕ 1/5 x₂) ⊕ 1/3 x₉) ⊔ 1/6
   x₉  = (1/3 x₉ ⊕ 1/3 x₁) ⊕ 1/5 x₁₀
   x₁₀ = (1/4 x₁₀ ⊕ 1/4 x₂) ⊕ 1/4 x₁₁
   x₁₁ = ((1/3 x₁₁ ⊕ 1/5 x₁) ⊕ 1/3 x₁₂) ⊔ 1/6
   x₁₂ = 1/4 ⊕ 1/3 x₁₂                         (anchor)
*)

let () =
  let m = 2 in
  let codim = 10 in
  let dim = m + codim in

  let terms =
    Array.init codim (fun j ->
        let k = j + 1 in
        let self_var = m + k in
        if k = codim then
          (* Anchor *)
          Mu_term.(
            strong_disj
              (const (Q.( // ) 1 4))
              (scalar_mult (Q.( // ) 1 3) (var self_var)))
        else
          let next_var = self_var + 1 in
          let free_var = 1 + (k mod m) in
          (* Varying coefficients *)
          let self_coeff = if k mod 2 = 1 then Q.( // ) 1 3 else Q.( // ) 1 4 in
          let free_coeff = Q.( // ) 1 (3 + (k mod 4)) in
          let next_coeff = Q.( // ) 1 (3 + ((k + 1) mod 3)) in
          let base =
            Mu_term.(
              strong_disj
                (strong_disj
                   (scalar_mult self_coeff (var self_var))
                   (scalar_mult free_coeff (var free_var)))
                (scalar_mult next_coeff (var next_var)))
          in
          if k mod 3 = 0 then
            (* ⊔ with constant — may force lfp iteration *)
            Mu_term.(weak_disj base (const (Q.( // ) 1 6)))
          else base)
  in

  let f = make_system dim terms in
  let point = Point.from_list [ Q.( // ) 1 2; Q.( // ) 1 3 ] in

  Printf.printf "System: dim=%d, codim=%d, point=%s\n%!" dim codim
    (Point.to_string point);
  Printf.printf "Computing...\n%!";

  let t0 = Sys.time () in
  let result = System_solver.lfp_system dim codim f point in
  (* let result = Simple_system_solver.solver dim codim f point in *)
  let dt = Sys.time () -. t0 in

  Printf.printf "Done in %.4f s\n\n" dt;

  for k = 1 to codim do
    let expr = Cle_vec.expr result k in
    let value = Lin_expr.eval expr point in
    Printf.printf "  x_%d = %s  (= %s at point)\n" (m + k)
      (Lin_expr.to_string expr) (Q.to_string value)
  done;

  Printf.printf "\nConstraints: %d\n" (List.length (Cle_vec.constraints result))
