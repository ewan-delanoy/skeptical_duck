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
    excluded_full_constraints: constraint_t list;
    added_partial_constraints: constraint_t list
} ;;

module type MOLD_STATE_TYPE = sig 

type t

end ;;

module Mold_state:MOLD_STATE_TYPE = struct

type t = unit ;; 

end ;;

type mold = {
    solutions : (int list) list;
    forced_elements : int list;
} ;;



