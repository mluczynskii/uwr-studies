type 'a context =
| Root
| Up of 'a context * 'a

type 'a zlist = 'a context * 'a list

let of_list xs = Root * xs

let to_list (z, xs) =
  match z with
  | Root     -> xs
  | Up(z, x) -> to_list (z, x :: xs)
  
let elem (z, xs) =
  match xs with
  | []      -> None
  | x :: xs -> Some x
  
let move_left (z, xs) =
  match z with
  | Root     -> failwith "gluurb"
  | Up(z, x) -> (z, x :: xs)
  
let move_right (z, xs) = 
  match xs with
  | []      -> failwith "gluurb"
  | x :: xs -> (Up(z, x), xs)

let insert x (z, xs) =
  let new_z = Up(z, x) in (new_z, xs)

let remove (z, xs) =
  match z with
  | Root     -> failwith "gluurb"
  | Up(z, x) -> (z, xs)
