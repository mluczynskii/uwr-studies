(** Abstrakcyjny typ odpowiedzi *)
type ('z,'i,'o) ans

(** Obliczenie w CPS-ie wewnątrz pojedynczego procesu.
  Ma cztery parametry:
  'a - typ obliczenia (typ oczekiwany przez kontynuację)
  'z - typ zwracanej wartości przez cały proces
  'i - typ elementów strumienia wejściowego
  'o - typ elementów strumienia wyjściowego *)
type ('a,'z,'i,'o) proc = ('a -> ('z,'i,'o) ans) -> ('z,'i,'o) ans

(** Wysyłanie pojedynczej wartości do strumienia wyjściowego *)
val send : 'o -> (unit,'z,'i,'o) proc

(** Odebranie pojedynczej wartości ze strumienia wejściowego *)
val recv : ('i,'z,'i,'o) proc

(** Zastępuje bieżący proces dwoma nowymi połączonymi ze sobą *)
val (>|>) : ('z,'z,'i,'m) proc -> ('z,'z,'m,'o) proc -> ('a,'z,'i,'o) proc

(** Uruchamia proces. Na wejściu znajdują się kolejne wiersze wejścia
  standardowego, natomiast wysyłane napisy są przekazywane na wyjście
  standardowe. *)
val run : ('a,'a,string,string) proc -> 'a

val map : ('i -> 'o) -> ('a, 'z, 'i, 'o) proc

val filter : ('i -> bool) -> ('a, 'z, 'i, 'i) proc

val nats_from : int -> ('a, 'z, 'i, int) proc

val sieve : ('a, 'a, int, int) proc