module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type t

  val apply : t -> key -> key
  val id : t
  val invert : t -> t
  val swap : key -> key -> t
  val compose : t -> t -> t
  val compare : t -> t -> int
end

module Make(Key : OrderedType) =
struct
  module Mapa = Map.Make(Key)

  type key = Key.t
  type t = {
    perm : key Mapa.t ;
    rev_perm : key Mapa.t
  }

  let apply sigma key =
    match Mapa.find_opt key sigma.perm with
    | None   -> key
    | Some x -> x

  let id = {
    perm = Mapa.empty ;
    rev_perm = Mapa.empty
  }

  let invert sigma = {
    perm = sigma.rev_perm ;
    rev_perm = sigma.perm
  }

  let swap x y = 
    if x = y then id else let sigma = Mapa.add x y Mapa.empty |> Mapa.add y x in {
      perm = sigma ;
      rev_perm = sigma
    }

  let compose s p =
    let f pom = fun key a b ->
      match a, b with
      | _, None   -> a
      | _, Some y ->
        let x = apply pom y in
        if x = key then None else Some x
      in
      { perm = Mapa.merge (f s) s.perm p.perm ;
        rev_perm = Mapa.merge (f (invert p)) p.rev_perm s.rev_perm }

  let compare s p =
    Mapa.compare Key.compare s.perm p.perm 

end