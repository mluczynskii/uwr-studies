let f x = x + 0
(* id: int -> int *)

let g (a : ('a -> 'b)) (b : ('c -> 'a)) (c : 'c) : 'b =
  a(b(c))
(* ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)

let h a b =
  a
(* 'a -> 'b -> 'a *)

let i a b = 
  if true then a else b
(* 'a -> 'a -> 'a *)