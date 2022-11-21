type goal = (string * Logic.formula) list * Logic.formula

type tree =
| Hole    of goal
| Theorem of Logic.theorem
| Imp_e   of tree * goal * tree
| Imp_i   of tree * goal 
| Bot_e   of tree * goal

type context =
| Root 
| Left  of context * goal * tree
| Right of context * tree * goal
| Child of context * goal

type proof_tree = context * tree

type proof =
| Complete   of Logic.theorem
| Incomplete of context * goal

let proof goal =
  Incomplete (Root, goal)

let goal proof =
  match proof with
  | Incomplete(_, goal) -> Some goal 
  | _                   -> None

let qed proof =
  match proof with
  | Complete theorem -> theorem
  | _                -> failwith "There are still holes in the proof"

type answer =
| Success of context * goal
| Failure of context * tree

let rec search_down (context, node) =
  match node with 
  | Hole goal       -> Success (context, goal)
  | Theorem theorem -> Failure (context, node)
  | Imp_e (left, x, right) ->
    let new_context = Right (context, left, x) in
    search_down (new_context, right)
  | Imp_i (middle, x) ->
    let new_context = Child (context, x) in 
    search_down (new_context, middle)
  | Bot_e (middle, x) ->
    let new_context = Child (context, x) in 
    search_down (new_context, middle)

let rec search_up (context, node) =
  match context with
  | Root  -> (Root, node)
  | Right (context, left, x) ->
    let new_context = Left (context, x, node) in 
    (new_context, left)
  | Left (context, x, right) ->
    let parent = Imp_e (node, x, right) in 
    search_up (context, parent)
  | Child (context, x) ->
    let parent = Imp_i (node, x) in 
    search_up (context, parent)

let next proof =
  match proof with 
  | Incomplete (context, goal) ->
    let rec cycle (context, node) =
      let entry_point = search_up (context, node) in 
      match search_down entry_point with 
      | Success (context, goal) -> (context, goal)
      | Failure (context, node) -> cycle (context, node)
    in
    let node = Hole goal in cycle (context, node)
  | _ -> failwith "The proof has already been completed"

let intro name proof =
  match proof with
  | Incomplete (context, (named_assumptions, formula)) ->
    begin match formula with
    | Implication (fi, psi) ->
      let new_assumptions = (name, fi) :: named_assumptions in
      let new_context = Child (context, (named_assumptions, formula)) in
      let new_goal = (new_assumptions, psi) in
      Incomplete (new_context, new_goal)
    | _ -> failwith "Active goal is not an implication" end
  | _ -> failwith "Proof has already been completed"

