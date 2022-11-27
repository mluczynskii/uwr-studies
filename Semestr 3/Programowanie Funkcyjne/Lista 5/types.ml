type empty = |

type _ fin_type =
| Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type
| Empty  : empty fin_type
| Unit   : unit fin_type
| Bool   : bool fin_type
| Pair   : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type

let rec all_values : type a. a fin_type -> a Seq.t =
  function t ->
    match t with
    | Either(a, b)    -> 
      let s1 = all_values a and s2 = all_values b in
      let left = Seq.map (fun a -> Either.left a) s1 and
      right = Seq.map (fun b -> Either.right b) s2 in
      Seq.append left right
    | Empty      -> Seq.empty
    | Unit       -> Seq.cons () Seq.empty
    | Bool       -> Seq.cons true (Seq.cons false Seq.empty)
    | Pair(a, b) -> 
      let s1 = all_values a and s2 = all_values b in
      Seq.product s1 s2
      



