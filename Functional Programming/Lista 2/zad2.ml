let rec sublists xs=
  match xs with
  | [] -> [[]]
  | (x :: xs) -> 
    let ys = sublists xs in List.fold_right (fun e acc -> (x :: e) :: acc) ys ys