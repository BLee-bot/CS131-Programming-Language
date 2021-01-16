
(* Assignment 8 *)
let my_subset_test0 = subset [1;2;3;3;3] [1;2;3]
let my_subset_test1 = not (subset [1;4;5] [1;2;4])

let my_equal_sets_test0 = equal_sets [1;3;3;3] [1;3;1;3]
let my_equal_sets_test1 = not (equal_sets [1;3;4;5] [100;4;3;1;3])

let my_set_union_test0 = equal_sets (set_union [4;5] [1;2;3]) [4;5;1;2;3]
let my_set_union_test1 = equal_sets (set_union [1] [1;2;3]) [1;2;3]

let my_set_symdiff_test0 = equal_sets (set_symdiff [1] [1;2;3]) [2;3]
let my_set_symdiff_test1 = equal_sets (set_symdiff [1;2;3;3;3] [1;2;3]) []

let my_computed_fixed_point_test0 = 
  computed_fixed_point (=) (fun x -> x *. x) 2. = infinity
let my_computed_fixed_point_test1 = 
  computed_fixed_point (=) (fun x -> 0.5 *. sqrt x) 2. = 0.25

type my_cat_nonterminals =
  | Fur | Purr | Claw | Cat

let my_cat_rules =
   [Fur, [T">"; N Fur; T"<"];
    Fur, [N Cat];
    Fur, [N Fur; N Purr];
    Fur, [N Claw];
    Fur, [N Cat; N Claw];
    Purr, [N Purr; N Fur];
    Fur, [T"Fur-end"];
    Purr, [T"Purr-ty"];
    Purr, [T"Purr-fect"];
    Cat, [T"Cat-ch"];
    Claw, [T"Claw-ver"];]

let my_cat_grammar = Fur, my_cat_rules

let my_cat_test0 = filter_reachable my_cat_grammar = my_cat_grammar
let my_cat_test1 = filter_reachable (Cat, List.tl my_cat_rules) = (Cat, [Cat, [T"Cat-ch"]])
