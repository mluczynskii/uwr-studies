exception Break
let for_all p xs =
  let f acc x = if p x then true else raise Break in 
  try List.fold_left f true xs with Break -> false

let mult_list xs =
  let f acc x = if x = 0 then raise Break else x * acc in
  try List.fold_left f 1 xs with Break -> 0

let sorted (head :: tail) =
  let f (prev, _) x = if x >= prev then (x, true) else raise Break in 
  try snd (List.fold_left f (head, true) tail) with Break -> false



