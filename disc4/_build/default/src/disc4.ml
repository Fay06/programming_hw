(*The following functions have been provided
  for your convenience*) 

  let rec map f xs = match xs with
  | [] -> []
  | x :: xt -> (f x)::(map f xt)
  
  let rec foldl f a xs = match xs with
  | [] -> a
  | x :: xt -> foldl f (f a x) xt
  
  let rec foldr f xs a = match xs with
  | [] -> a
  | x :: xt -> f x (foldr f xt a) 

  (* You may want to use these functions for stalin_sort_right *)
  let rec rev lst = match lst with 
    | [] -> [] 
    | x :: xt -> (rev xt) @ [x] 

  let rec get_last_element lst = 
    match lst with 
    | [x] -> x 
    | _ :: xt -> get_last_element xt 
    | [] -> failwith "empty list has no elements"
  

  (*This record type will be used for the 
  update_database problem*) 
  type student_information = 
    { 
        name : string;
        age : int; 
        gpa : float;
    } 

  (*Implement these functions*)
  let mul_thresh lst thresh = 
    foldl 
    (fun (less, greaterequal) x -> if x < thresh then (less * x, greaterequal) else (less, greaterequal * x))
    (1,1)
    lst;;
  
  let rec multi_map f lst = 
    map (fun inner_lst -> map f inner_lst) lst;;
  
  let update_database lst = 
    map (fun (a, b, c) -> {name = a; age = b; gpa = c;}) lst;;
  
  let stalin_sort lst = 
      match lst with
      |[]->[]
      |(h::t) -> 
      let (_,currlst) = 
        foldl (fun (prev_elem, currlst) x -> if x >= prev_elem then (x, currlst @ [x]) else (prev_elem, currlst)) 
        (h, []) 
        lst in 
        currlst;;

  let stalin_sort_right lst = 
      match lst with
      |[]->[]
      |(h::t) -> 
      let (_,currlst) = 
        foldr (fun x (prev_elem, currlst) -> if x <= prev_elem then (x, x::currlst) else (prev_elem, currlst)) 
        lst 
        (get_last_element lst, []) in 
        currlst;;
  