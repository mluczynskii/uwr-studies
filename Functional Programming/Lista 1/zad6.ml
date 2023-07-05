let ctrue t f = t
let cfalse t f = f

let cand b1 b2 t f =
  b1 (b2 t f) f
  
let cor b1 b2 t f =
  b1 t (b2 t f)

let cbool_of_bool (x : bool) =
  if x then ctrue else cfalse

let bool_of_cbool b =
  b true false
