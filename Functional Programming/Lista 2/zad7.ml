type 'a tree =
| Leaf
| Node of 'a tree * 'a * int * 'a tree

let height t =
  match t with
  | Leaf             -> 0
  | Node(_, _, h, _) -> h

let make a b x =
  let ah = height a and bh = height b in
  if ah >= bh then Node(a, x, bh + 1, b) else Node(b, x, ah + 1, a)

let rec merge a b =
  match a with
  | Leaf -> b
  | Node(al, ax, _, ar) ->
    match b with
    | Leaf -> a
    | Node(bl, bx, _, br) ->
      if ax > bx then make bl (merge br a) bx else make al (merge ar b) ax

let insert x t =
  merge (make Leaf Leaf x) t

let delete t =
  match t with
  | Leaf -> None
  | Node(l, x, h, r) -> 
    Some(x, merge l r)

