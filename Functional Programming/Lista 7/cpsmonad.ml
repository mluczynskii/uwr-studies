module type Monad = sig 
  type 'a t
  val return : 'a -> 'a t 
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
end 

module Err : sig 
  include Monad 
  val fail : 'a t 
  val catch : 'a t -> (unit -> 'a t) -> 'a t 
  val run : 'a t -> 'a option
end = struct
  type ('a, 'b) cont = 'a -> 'b
  type 'a t = {run : 'b. ('a, 'b option) cont -> 'b option}

  let return x = {run = fun cont -> cont x}
  let fail = {run = fun _ -> None}
  let run m = m.run (fun x -> Some x)
  let catch m f = 
    match run m with 
    | None       -> f ()
    | Some value -> m 
  let (>>=) m f =
    match m.run (fun x -> Some (f x)) with 
    | None       -> fail 
    | Some value -> value
end

module BT : sig
  include Monad 
  val fail : 'a t 
  val run : 'a t -> 'a Seq.t 
  val flip : bool t
end = struct 
  type ('a, 'b) cont = 'a -> 'b 
  type 'a t = {run : 'b. ('a, 'b Seq.t) cont -> 'b Seq.t}

  let return x = {run = fun cont -> cont x}
  let fail = {run = fun _ -> Seq.empty}
  let run m = m.run (fun x -> Seq.return x)
  let (>>=) m f = {run = fun cont ->
    run m |> 
    Seq.map f |> 
    Seq.flat_map (fun x -> x.run cont)}
  let flip = {run = fun cont ->
    let xs = List.to_seq [true;false] in 
    Seq.flat_map cont xs}
end

module St (State : sig type t end) : sig 
  include Monad 
  val get : State.t t 
  val set : State.t -> unit t 
  val run : State.t -> 'a t -> 'a
end = struct 
  type ('a, 'b) cont = 'a -> 'b 
  type 'b ans = State.t -> 'b * State.t
  type 'a t = {run : 'b. ('a, 'b ans) cont -> 'b ans}

  let return x = {run = fun cont -> cont x}
  let get = {run = fun cont state -> cont state state}
  let set s = {run = fun cont _ -> cont () s}
  let run s m = 
    let cont = fun a state -> (a, state) in 
    let (v, _) = m.run cont s in v
  let (>>=) m f = {run = fun cont s ->
    let (a, snew) = m.run (fun a state -> (a, state)) s in 
    (f a).run cont snew}
end