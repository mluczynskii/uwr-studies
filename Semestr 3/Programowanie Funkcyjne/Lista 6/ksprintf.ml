type (_, _) format =
| Lit : string -> ('a, 'a) format
| Int : (int -> 'a, 'a) format 
| Str : (string -> 'a, 'a) format 
| Cat : ('a, 'b) format * ('b, 'c) format -> ('a, 'c) format

let rec aux : type a b. (a, b) format -> (string -> b) -> string -> a =
  fun dir cont prefix ->
    match dir with 
    | Lit s     -> cont (prefix ^ s)
    | Int       -> fun n -> cont (prefix ^ Int.to_string n)
    | Str       -> fun s -> cont (prefix ^ s)
    | Cat(f, g) -> aux f (aux g cont) prefix

let ksprintf dir cont = aux dir cont "" 

let sprintf dir = ksprintf dir Fun.id

