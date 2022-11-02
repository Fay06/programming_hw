open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = 
  match lst with
  |[] -> 0
  |h::t ->
    let (_,acc) =
    let check element value accum = if element = value then accum + 1 else accum in
    fold (fun (value,acc) ele -> (value, check ele value acc)) (target,0) lst in
    acc;;

let uniq lst = 
  fold_right (fun ele acc -> if count_occ acc ele = 0 then ele::acc else acc) lst [];;

let assoc_list lst = 
  fold_right (fun ele acc -> (ele, count_occ lst ele)::acc) (uniq lst) [];;

let ap fns args = 
  match args with 
  |[] -> []
  |h::t ->
    fold_right (fun ele acc -> (map ele args) @ acc) fns [];;
