type ('z,'i) in_channel =
| In of (('z,'i) out_channel -> 'z)

and ('z,'o) out_channel =
| Out of ('o -> ('z,'o) in_channel -> 'z)

type ('z,'i,'o) ans = ('z,'i) in_channel -> ('z,'o) out_channel -> 'z

type ('a,'z,'i,'o) proc = ('a -> ('z,'i,'o) ans) -> ('z,'i,'o) ans

let send o k in_ch (Out send) =
  send o (In(fun out_ch -> k () in_ch out_ch))

let recv k (In recv) out_ch =
  recv (Out(fun v in_ch -> k v in_ch out_ch))

let exit_k x _ _ = x

let (>|>) p1 p2 _ in_ch out_ch =
  p2 exit_k
    (In(fun m_ch -> p1 exit_k in_ch m_ch))
    out_ch

let rec stdin_ch =
  In (fun (Out send) -> send (read_line ()) stdin_ch)

let rec stdout_ch =
  Out (fun str (In recv) -> print_endline str; recv stdout_ch)

let run p = p exit_k stdin_ch stdout_ch

let rec map f cont =
  recv (fun v -> send (f v) (fun () -> map f cont))

let rec filter p cont =
  recv (fun v -> if p v then send v (fun () -> filter p cont) else filter p cont)

let rec nats_from n cont =
  send n (fun () -> nats_from (n+1) cont)

let rec sieve cont =
  recv (fun v -> send v (fun () -> 
    (filter (fun n -> n mod v <> 0) >|> sieve) cont))