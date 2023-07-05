type 'a tree

val height t : int

val make a b x : 'a tree -> 'a tree -> 'a -> 'a tree

val rec merge a b : 'a tree -> 'a tree -> 'a tree

val insert x t : 'a -> 'a tree -> 'a tree

val delete t : 'a tree -> 'a * 'a tree

