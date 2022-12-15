module SBT (State : sig type t end) : sig
  type 'a t 
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
  val get : State.t t 
  val put : State.t -> unit t 
  val run : State.t -> 'a t -> 'a Seq.t 
  val fail : 'a t 
  val flip : bool t  
end = struct 
  type 'a t = State.t -> ('a * State.t) Seq.t
  let return x s = Seq.return (x, s)
  let (>>=) m f s =
    Seq.flat_map (fun (a, state) -> f a state) (m s)
  let get s   = Seq.return (s, s)
  let put s _ = Seq.return ((), s)
  let run s m = Seq.map (fun (a, _) -> a) (m s)
  let fail _  = Seq.empty 
  let flip (s : State.t) = List.to_seq [(true, s);(false, s)]
end