let my_length xs =
  List.fold_left (fun acc x -> acc + 1) 0 xs

let my_rev xs =
  List.fold_left (fun acc x -> x :: acc) [] xs

let my_map xs f =
  List.fold_right (fun x acc -> f(x) :: acc) xs []

let my_append xs ys =
  List.fold_right (fun x acc -> x :: acc) xs ys

let my_rev_append xs ys =
  List.fold_left (fun acc x -> x :: acc) ys xs

let my_filter xs f = 
  List.fold_right (fun x acc -> if f(x) then x :: acc else acc) xs []

let my_rev_map xs f =
  List.fold_left (fun acc x -> f(x) :: acc) [] xs