
(*Auxiliary functions for Assignment 1~4 *)

(*remove duplicates in the list*)
let rm_dup lst = 
  let cmp x lst =
    if List.mem x lst then lst 
    else x :: lst in
  List.fold_right cmp lst [] ;;
  
(*return the intersection set of a and b*)  
let set_intersection a b =
  let rec intersect a b =
    match a with
      [] -> []
    |x::t -> if List.mem x b then x::intersect t b
        else intersect t b in
  intersect a b;;

(* Assignement 1 *)
let subset a b =
(*remove duplicate eliment in the set of a and b*)
  let a = (rm_dup a) 
  and b = (rm_dup b) in 
(*check if an element of a is in the set b by recursively*)
  let rec subs a b =
    match a with
      [] -> true
    | x::t -> if not (List.mem x b) then false
        else subs t b in
  subs a b;;
    
(* Assignement 2 *)
let equal_sets a b =
  (subset a b) && (subset b a);;

(* Assignement 3 *)
let set_union a b =
  rm_dup (a @ b);;

(* Assignement 4 *)
let set_symdiff a b = 
(* new a is a union of the set a and b*)
  let a = (set_union a b) 
(* new b is a intersection of the set a and b*)
  and b = rm_dup (set_intersection a b) in 
(* return the set (new a - new b) *)
  let rec diff a b = 
    match a with 
      [] -> []
    | x::t -> if List.mem x b then diff t b
        else x::diff t b in 
  diff a b;;

(* Assignment 5 *)
(*Not possible*)
let rec self_member s=
  true;;

(* Assignment 6 *)
let rec computed_fixed_point eq f x =
  if eq (f x) x then x
  else computed_fixed_point eq f (f x);;  
    
(* Assignment 7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
  
let equal_second_elem_sets a b =
  let sa = snd a and sb = snd b in
  equal_sets sa sb;;

let get_reachable_symbols g = 
  let tmprules = fst g in

  let rec helper_function g =
    let rules = fst g and reachable_symbols = snd g in 
    if rules = [] then (rules, reachable_symbols) 
    else 
      let tmp_rule = List.hd rules in
      let rest_rules = List.tl rules in
      let tmp_symbol = fst tmp_rule in
      let right_hand_side = snd tmp_rule in
      if subset [tmp_symbol] reachable_symbols then 
        let nonterminal = List.filter (fun x -> 
            match x with
            | T _ -> false 
            | N nt -> true) right_hand_side in
        let filtnonterminal = 
        List.map (fun (N x) -> x) nonterminal in
        helper_function (rest_rules, set_union reachable_symbols filtnonterminal)
      else
        helper_function (rest_rules, reachable_symbols) in

  (tmprules, snd (helper_function g));;


let filter_reachable g = 
  let start_symbol = fst g 
  and rules = snd g in 
  let rec reachable_symbols =
    snd (computed_fixed_point equal_second_elem_sets get_reachable_symbols (rules, [start_symbol])) in
  let filtered_rules = 
    List.filter (fun x -> subset [fst x] reachable_symbols) rules in
  (start_symbol, filtered_rules);;

