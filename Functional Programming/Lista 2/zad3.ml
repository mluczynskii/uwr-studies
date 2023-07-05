let rec suffixes xs =
  match xs with
  | [] -> [[]]
  | (x :: xr) -> xs :: suffixes xr

let prefixes xs =
  List.rev_map (List.rev) (suffixes (List.rev xs))