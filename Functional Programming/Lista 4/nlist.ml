type 'a nlist =
| Nil
| Zero of ('a * 'a) nlist
| One  of 'a * ('a * 'a) nlist
| Two of 'a * 'a * ('a * 'a) nlist

let rec cons : 'a. 'a -> 'a nlist -> 'a nlist =
  fun x xs ->
  match xs with
  | Nil        -> One(x, Nil)
  | Zero xs    -> One(x, xs)
  | One(y, xs) -> Two(x, y, xs)
  | Two(y, z, xs) -> One(x, cons (y, z) xs)

let rec view : 'a. 'a nlist -> ('a * 'a nlist) option =
  function
  | Nil     -> None
  | Zero xs ->
    begin match view xs with
    | None             -> None
    | Some((x, y), xs) -> Some(x, One(y, xs))
    end
  | One(x, xs)    -> Some(x, Zero xs)
  | Two(x, y, xs) -> Some(x, One(y, xs)) 

let rec nth : 'a. int -> 'a nlist -> 'a =
  fun n xs ->
  match xs with
  | Nil -> raise Not_found
  | Zero xs ->
    let (x, y) = nth (n/2) xs in
    if n mod 2 = 0 then x else y
  | One(x, xs) ->
    if n = 0 then x
    else nth (n-1) (Zero xs)
  | Two(x, y, xs) ->
    if n = 0 then x 
    else nth (n-1) (One(y, xs))
