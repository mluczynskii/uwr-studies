module type RandomMonad = sig
  type 'a t 
  val return : 'a -> 'a t 
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t 
  val run : int -> 'a t -> 'a
end 

module RS : RandomMonad = struct 
  type 'a t = int -> ('a * int) 
  let next seed =
    let x = 16807 * (seed mod 127773) - 2836 * (seed / 127773) in 
    if x > 0 then x else x + 2147483647

  let return (x : 'a) : 'a t = 
    fun seed -> (x, seed)

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t = 
    fun seed -> let (value, nseed) = x seed in
    f value nseed 

  let random : int t = 
    fun seed -> (next seed, next seed)

  let run (seed : int) (x : 'a t) : 'a = 
    let (value, _) = x seed in value
end

module Shuffle(R : RandomMonad) : sig 
  val shuffle : 'a list -> ('a list) R.t 
end = struct 
  let (let*) = R.bind

  let choose xs =
    let* rand = R.random in 
    R.return (List.nth xs (rand mod (List.length xs)))

  let rec remove elem xs =
    match xs with 
    | [] -> [] 
    | x :: xs ->
      if x = elem then xs else x :: (remove elem xs)

  let shuffle xs =
    let rec f xs acc =
      match xs with 
      | [] -> R.return acc
      | _ ->
        let* x = choose xs in 
        f (remove x xs) (x :: acc)
      in 
    f xs []
end

