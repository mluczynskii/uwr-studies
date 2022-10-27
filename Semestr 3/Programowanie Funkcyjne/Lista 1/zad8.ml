type cbool = { cbool : 'a. 'a -> 'a -> 'a }
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let cand (b1 : cbool) (b2 : cbool) =
  { cbool = fun tt ff -> b1.cbool (b2.cbool tt ff) ff }

let cor (b1 : cbool) (b2 : cbool) = 
  { cbool = fun tt ff -> b1.cbool tt (b2.cbool tt ff) }

let cbool_of_bool (b : bool) =
  { cbool = fun tt ff -> if b then tt else ff }

let bool_of_cbool (b : cbool) =
  b.cbool true false

let zero =
  { cnum = fun f x -> x }

let succ (n : cnum) =
  { cnum = fun f x -> f (n.cnum f x) }

let add (n : cnum) (m : cnum) =
  { cnum = fun f x -> m.cnum f (n.cnum f x) }

let mul (n : cnum) (m : cnum) =
  { cnum = fun f x -> m.cnum (n.cnum f) x }

let is_zero (n : cnum) =
  { cbool = fun tt ff -> n.cnum (fun x -> ff) tt }

let rec cnum_of_int (n : int) =
  { cnum = fun f x -> if n = 0 then x else (succ (cnum_of_int (n-1))).cnum f x }

let int_of_cnum (n : cnum) =
  n.cnum (fun x -> x + 1) 0