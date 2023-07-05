let rec tabulate (s : (int -> 'a)) ?(l = 0) (r : int)  : ('a list) =
  if l > r then [] else s(l) :: tabulate s ~l:(l+1) r

let example_stream (x : int) = x 