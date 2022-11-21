type formula = 
| False 
| Variable of char
| Implication of formula * formula 

let rec string_of_formula f =
  match f with
  | False              -> "⊥"
  | Variable c         -> String.make 1 c
  | Implication (fi, psi) ->
    match fi with
    | Implication (_, _)  -> "(" ^ string_of_formula fi ^ ") -> " ^ string_of_formula psi
    | _                   -> string_of_formula fi ^ " -> " ^ string_of_formula psi 

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = {assumptions : formula list; consequence : formula}

let assumptions thm = thm.assumptions

let consequence thm = thm.consequence

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = {assumptions = [f]; consequence = f}

let imp_i f thm = 
  {assumptions = List.filter (fun x -> x <> f) (assumptions thm);
   consequence = Implication(f, consequence thm)}

let remove_duplicates xs =
  List.fold_right (fun x acc -> x :: List.filter (fun y -> y <> x) acc) xs []

let imp_e th1 th2 =
  match consequence th1 with
  | Implication (fi, psi) ->
    if fi <> consequence th2 then failwith "invalid arguments" else
    {assumptions = remove_duplicates ((assumptions th1) @ (assumptions th2));
     consequence = psi}
  | _ -> failwith "invalid arguments"

let bot_e f thm =
  match consequence thm with 
  | False -> 
    {assumptions = (assumptions thm);
     consequence = f}
  | _ -> failwith "invalid arguments"
