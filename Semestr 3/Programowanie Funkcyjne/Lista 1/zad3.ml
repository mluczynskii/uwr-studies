let hd (s : (int -> 't)) : 't = 
  s(0)

let tl (s : (int -> 't)) : (int -> 't) = 
  fun (index : int) : 't -> 
    s(index+1)

let add (x : int) (s : (int -> int)) : (int -> int) = 
  fun (index : int) : int ->
    s(index) + x

let map (f : ('a -> 'b)) (s : (int -> 'a)) : (int -> 'b) =
  fun (index : int) : 'b ->
    f(s(index))

let map2 (f : ('a -> ('b -> 'c))) (s1 : (int -> 'a)) (s2 : (int -> 'b)) : (int -> 'c) =
  fun (index : int) : 'c ->
    f(s1(index))(s2(index))

let replace (n : int) (a : 't) (s : (int -> 't)) : (int -> 't) =
  fun (index : int) : 't ->
    if index = n then a else s(index)

let take_every (n : int) (s : (int -> 't)) : (int -> 't) =
  fun (index : int) : 't ->
    s(index * n)

let example_stream : (int -> int) = 
  fun (index : int) : int ->
    index