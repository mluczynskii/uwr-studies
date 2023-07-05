type 'a lazy_tree = {
  left  : ('a lazy_tree) Lazy.t;
  elem  : 'a;
  right : ('a lazy_tree) Lazy.t;
}

let rec make_node (a, b) (c, d) =
  let elem  = (a+c, b+d) in
  let left  = lazy (make_node (a, b) (a+c, b+d)) in
  let right = lazy (make_node (a+c, b+d) (c, d)) in
  { left = left; elem = elem; right = right }

let q_tree = make_node (0, 1) (1, 0)
