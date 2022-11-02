open Funs

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  | IntLeaf -> 0
  | IntNode (y, l, r) -> 1 + int_size l + int_size r;;

let rec int_max t = 
  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (y, l, r) -> 
    match r with 
    | IntLeaf -> y 
    | IntNode (ry, rl, rr) -> int_max r;;

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
  | Leaf
  | Node of 'a * 'a atree * 'a atree;;
type 'a compfn = 'a -> 'a -> int;;
type 'a ptree = 'a compfn * 'a atree;;

let empty_ptree f : 'a ptree = (f,Leaf);;

(* Implement the functions below. *)

let rec ainsert x f t =
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node (y, l, r) when f x y = 1 -> Node (y, l, ainsert x f r)
  | Node (y, l, r) when f x y = 0 -> t
  | Node (y, l, r) -> Node (y, ainsert x f l, r);;

let pinsert x t = 
  match t with (a, b) -> (a, ainsert x a b);;

let rec amem x f t =
  match t with
  | Leaf -> false
  | Node (y, l, r) when f x y = 1 -> amem x f r
  | Node (y, l, r) when f x y = 0 -> true
  | Node (y, l, r) -> amem x f l;;

let pmem x t = 
  match t with (a, b) -> amem x a b;;

let pinsert_all lst t = 
  fold_right (fun ele acc -> pinsert ele acc) lst t;;

let rec list_aux t lst = 
  match t with
  | Leaf -> lst
  | Node (y, l, r) -> 
    let left = list_aux l lst in
     let value = left @ [y] in 
        list_aux r value;;

let rec p_as_list t = 
  match t with (a, b) -> list_aux b [];;

let pmap f t = 
  let lst = p_as_list t in
    let newlst = map f lst in
      match t with (a, b) -> pinsert_all newlst (empty_ptree a);;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = ((string * int) list) list

let empty_table () : lookup_table = [];;

let push_scope (table: lookup_table) : lookup_table = []::table;;

let pop_scope (table: lookup_table) : lookup_table = 
  match table with
    |[] -> failwith "No scopes remain!"
    |h::t -> t;;

let add_var name value (table: lookup_table) : lookup_table = 
  match table with
    |[] -> failwith "There are no scopes to add a variable to!"
    |h::t -> ((name,value)::h)::t;;

let find_aux lst target =
  fold_right (fun ele acc -> match ele with |(a,b) -> if a = target then (true,b) else acc) lst (false, 0);; 

let rec lookup name (table: lookup_table) = 
  match table with 
    |[] -> failwith "Variable not found!"
    |h::t ->
      let result = find_aux h name in
      match result with
      |(a,b) -> if a then b else lookup name t;;

(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
  | Circ of { radius: float; center: pt }
  | Square of { length: float; upper: pt }
  | Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s = 
  match s with
  | Circ {radius = r; center = c} -> r *. r *. 3.14
  | Square {length = l; upper = u} -> l *. l
  | Rect {width = w; height = h; upper = u} -> w *. h;;

let filter f lst = 
  fold_right (fun ele acc -> if f ele then ele::acc else acc) lst [];;
