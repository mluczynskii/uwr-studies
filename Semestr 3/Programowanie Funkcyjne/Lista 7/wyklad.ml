(** Monada błędu -- obliczenie może się udać (konstruktor Some), albo
 * nie (konstruktor None) *)
module Err : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Błąd *)
  val fail  : 'a t
  (** Przechwytywanie błędu *)
  val catch : 'a t -> (unit -> 'a t) -> 'a t
  
  val run : 'a t -> 'a option
end = struct
  type 'a t = 'a option

  let return x = Some x
  let bind m f =
    match m with
    | None   -> None
    | Some x -> f x

  let fail = None
  let catch m f =
    match m with
    | Some x -> Some x
    | None   -> f ()

  let run m = m
end

(** Obliczenia z nawrotami *)
module BT : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Brak wyniku *)
  val fail : 'a t
  (** Niedeterministyczny wybór -- zwraca true, a potem false *)
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  (* Obliczenie typu 'a to leniwa lista wszystkich możliwych wyników *)
  type 'a t = 'a Seq.t

  let return x = List.to_seq [ x ]
  let rec bind m f = Seq.flat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

(* ========================================================================= *)
(* Przykładowy kod wykorzystujący monadę BT -- znajdowanie wszystkich trójek *)
(* pitagorejskich                                                            *)
(* ========================================================================= *)
let (let* ) = BT.bind

let rec select a b =
  if a >= b then BT.fail
  else
    let* c = BT.flip in
    if c then BT.return a
    else select (a+1) b

let triples n =
  let* a = select 1 n in
  let* b = select a n in
  let* c = select b n in
  if a*a + b*b = c*c then BT.return (a, b, c)
  else BT.fail
(* ========================================================================= *)

(** Monada stanu -- obliczenia z ukrytą komórką mutowalnego stanu *)
module St(State : sig type t end) : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Pobierz stan *)
  val get : State.t t
  (** Ustaw stan *)
  val set : State.t -> unit t

  val run : State.t -> 'a t -> 'a
end = struct
  (* Obliczenie reprezentujemy jako funkcję z bieżącej wartości stanu w parę
   * wynik-nowy stan *)
  type 'a t = State.t -> 'a * State.t

  let return x s = (x, s)
  let bind m f s =
    let (x, s) = m s in
    f x s

  let get s = (s, s)
  let set s _ = ((), s)

  let run s m = fst (m s)
end
