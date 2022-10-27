type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = 
  { clist = fun f z -> z }

let ccons x xs =
  { clist = fun f z -> f x (xs.clist f z) }

let map f xs =
  { clist = fun g z -> xs.clist (fun x y -> g (f x) y) z }

let append xs ys =
  { clist = fun f z -> xs.clist f (ys.clist f z) }

let clist_to_list xs =
  xs.clist (fun x y -> x :: y) []

let list_to_clist xs =
  { clist = fun f z -> 
    List.fold_right (fun x acc -> (f x acc)) xs z }
