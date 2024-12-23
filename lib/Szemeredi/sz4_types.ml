(*

#use"lib/Szemeredi/sz4_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz4 is short for "fourth stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type fan = F of int list list ;; 

type point = {
    base_set : finite_int_set;
    max_width: width;
    added_constraints: constraint_t list
} ;;

type mold = {
    solutions : (int list) list;
    mandatory_elements : int list;
} ;;

type explanation =
   Free
  |Extension 
  |Filled_complement of int list 
  |Decomposition of finite_int_set * finite_int_set * (int list) 
  |Breaking_point of int * int * int 
  |Width_one_expl 
  |Segment_cut of int * int;; 


