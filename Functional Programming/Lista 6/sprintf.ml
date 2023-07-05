type ('a, 'b) format = (string -> 'b) -> (string -> 'a)
let lit (str : string) : ('a, 'a) format =
  fun (cont : string -> 'a) -> 
    fun s -> cont (s ^ str) 
    
let int : (int -> 'a, 'a) format =
  fun (cont : string -> 'a) ->
    fun s n -> cont (s ^ Int.to_string n)

let str : (string -> 'a, 'a) format =
  fun (cont : string -> 'a) ->
    fun s str -> cont (s ^ str) 

let (^^) f g = fun cont -> f (g cont)

let sprintf (dir : ('a, string) format) : 'a = (dir Fun.id) ""


