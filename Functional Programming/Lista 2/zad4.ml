let rec merge cmp xs ys =
  match xs, ys with
  | xs, [] -> xs
  | [], ys -> ys
  | (x :: xr), (y :: yr) -> 
    if cmp x y then x :: (merge cmp xr ys) else y :: (merge cmp xs yr)

let rec merge_tail cmp xs ys =
  let rec f xs ys acc =
    match xs, ys with
    | xs, [] -> (List.rev acc) @ xs
    | [], ys -> (List.rev acc) @ ys
    | (x :: tx), (y :: ty) -> 
      if cmp x y then f tx ys (x :: acc) else f xs ty (y :: acc) 
  in f xs ys []

let rec halve xs =
  match xs with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | a :: b :: xs -> let p = halve xs in (a :: fst p, b :: snd p)

let rec mergesort cmp xs =
  match xs with
  | [] -> []
  | [x] -> [x]
  | _ -> let p = halve xs in merge cmp (mergesort cmp (fst p)) (mergesort cmp (snd p)) 