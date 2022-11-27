type 'a cell =
| Value   of 'a
| Delay   of (unit -> 'a)
| Busy

type 'a my_lazy = ('a cell) ref

let force x =
  match !x with
  | Value value -> value
  | Delay f -> 
    x := Busy; 
    let value = f () in
    x := Value value; value
  | Busy -> failwith "Value is already being computed"

let fix f = 
  let rec nval = ref (Delay (fun () -> f nval)) in nval

type 'a lazy_list = 'a node my_lazy 
and 'a node = 
| Nil
| Cons of 'a * 'a lazy_list

let rec filter p xs =
  match (force xs) with
  | Nil -> fix (fun l -> Nil)
  | Cons (x, xs) when p x ->
    fix (fun l -> Cons (x, filter p xs))
  | Cons (_, xs) ->
    filter p xs

let rec take_while p xs =
  match (force xs) with
  | Cons (x, xs) when p x ->
    fix (fun l -> Cons (x, take_while p xs))
  | _ -> fix (fun l -> Nil)

let rec for_all p xs =
  match (force xs) with
  | Nil -> true
  | Cons (x, xs) -> 
    p x && for_all p xs

let rec nats_from n = fix (fun l -> Cons (n, nats_from (n+1)))

let primes =
  let nat_list = nats_from 2 in
  let is_prime n =
    nat_list
    |> take_while (fun p -> p * p <= n) 
    |> for_all (fun p -> n mod p <> 0) in
  fix (fun l -> Cons (2, filter is_prime (nats_from 3)))

let _ =
  let rec print_primes xs =
    match (force xs) with
    | Nil -> ()
    | Cons (x, xs) ->
      print_endline (Int.to_string x);
      print_primes xs 
    in 
  let x = primes |> take_while (fun p -> p <= 100) in 
  print_primes x
  
