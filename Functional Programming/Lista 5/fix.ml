
let rec fix f x = f (fix f) x

let rec fix_with_limit n f x =
  if n = 0 then failwith "Depth limit reached" else
    f (fix_with_limit (n-1) f) x

let fix_with_memo f x =
  let init = Hashtbl.create 42 in
  let rec fix f x = 
    match Hashtbl.find_opt init x with
    | Some value -> value
    | None       -> 
      let value = f (fix f) x in
      Hashtbl.add init x value; value
    in fix f x
