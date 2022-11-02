open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let f acc curr = 
    let check a c =
      match c with (first, trans, last) -> 
        if first = curr && trans = s then last::a else a
    in Sets.union (List.fold_left check [] nfa.delta) acc
  in List.fold_left f [] qs;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec aux qs =
    let update = move nfa qs None in
      let new_qs = List.fold_left (fun acc ele -> if Sets.elem ele qs then acc else ele::acc) [] update in
        match new_qs with
        |[] -> qs
        |_ -> aux (Sets.union qs new_qs)
  in aux qs;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let rec accept_aux curr chars =
    match chars with
    |[] -> 
      let curr = e_closure nfa curr in 
        List.fold_left (fun acc ele -> if Sets.elem ele nfa.fs then true else acc) false curr
    |h::t -> 
      let curr = e_closure nfa curr in
        let curr = move nfa curr (Some h) in
         accept_aux curr t
  in accept_aux [nfa.q0] (explode s);;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc ele -> (e_closure nfa (move nfa qs (Some ele)))::acc) [] nfa.sigma;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc ele -> (qs, Some ele, (e_closure nfa (move nfa qs (Some ele))))::acc) [] nfa.sigma;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (Sets.intersection nfa.fs qs) <> [] then [qs] else [];;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  |[] -> dfa
  |h::t -> 
    if Sets.elem h dfa.qs then nfa_to_dfa_step nfa dfa t
    else let dfa = {sigma = dfa.sigma; qs = h::dfa.qs; q0 = dfa.q0; fs = dfa.fs;
      delta = Sets.union dfa.delta (List.fold_left (fun acc ele -> Sets.union acc [ele]) dfa.delta (new_trans nfa h))} in
      nfa_to_dfa_step nfa dfa (Sets.union t (List.fold_left (fun acc ele -> Sets.union acc [ele]) dfa.qs (new_states nfa h)));;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = {sigma = nfa.sigma; qs = []; q0 = e_closure nfa [nfa.q0]; fs = []; delta = []} in
    let dfa = nfa_to_dfa_step nfa dfa [e_closure nfa [nfa.q0]] in
    {sigma = dfa.sigma; qs = dfa.qs; q0 = dfa.q0; delta = dfa.delta;
    fs = List.fold_left (fun acc ele -> let final = new_finals nfa ele in if final = [] then acc else Sets.union final acc) [] dfa.qs};;
