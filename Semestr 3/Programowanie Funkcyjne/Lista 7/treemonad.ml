type symbol = string
type 'v term =
| Var of 'v
| Sym of symbol * ('v term) list

type 'a t = 'a term

(* nowa zmienna *) 
let return x = Var x 

(* podstawienie za zmienne *)
let rec (>>=) m f =
  match m with 
  | Var v       -> f v 
  | Sym (s, xs) ->
    Sym (s, List.map (fun m -> m >>= f) xs)