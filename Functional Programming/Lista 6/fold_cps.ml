let rec fold_left_cps f acc xs cont =
  match xs with 
  | []      -> cont acc 
  | x :: xs ->
    let ncont = fun acc -> fold_left_cps f acc xs cont in
    f acc x ncont 

let fold_left f acc xs =
  let op = fun acc x cont -> cont (f acc x) in 
  fold_left_cps op acc xs Fun.id
    
let for_all p xs =
  let f = fun acc x cont ->
    if p x then cont true else false in 
  fold_left_cps f true xs Fun.id

let mult_list xs =
  let f = fun acc x cont ->
    if x = 0 then 0 else cont (acc * x) in 
  fold_left_cps f 1 xs Fun.id

let sorted (head :: tail) =
  let f = fun acc x cont ->
    if x < acc then false else cont x in 
  fold_left_cps f head tail (fun x -> true)