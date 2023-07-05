let rec insert elem xs =
  match xs with
  | [] -> [[elem]]
  | hd :: tl -> (elem :: xs) :: (List.map (fun l -> hd :: l) (insert elem tl))

let rec perms xs =
  match xs with
  | [] -> [[]]
  | hd :: tl -> List.concat_map (insert hd) (perms tl)