type t = { constraints : Lin_ineq.t list; expr : Lin_expr.t }

let construct constraints expr = { constraints; expr }
let constraints cle = cle.constraints
let expr cle = cle.expr
