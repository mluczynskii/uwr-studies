let scan (f : ('a -> 'b -> 'a)) (a : 'a) (s : (int -> 'b)) : (int -> 'a) =
  let rec new_stream (index : int) : 'a =
    if index == 0 then f a (s(0)) else f (new_stream(index - 1)) (s(index))
  in new_stream

let example_stream =
  fun (index : int) : int ->
    index

let test =
  scan(+)(0)(example_stream)