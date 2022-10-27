let ctrue t f = t
let cfalse t f = f

let zero = 
  fun f x -> x

let succ n =
  fun (f : ('a -> 'a)) (x : 'a) : 'a ->
    f (n f x)

let add n m =
  fun f x : 'a ->
    m f (n f x)

let mul n m =
  fun f x : 'a ->
    m (n f) x

let is_zero n = 
  let f x = cfalse in n f ctrue

let rec cnum_of_int n =
  if n = 0 then zero else succ (cnum_of_int (n-1))

let int_of_cnum n =
  let f x = (x+1) in n f 0 

