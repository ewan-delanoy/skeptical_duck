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

type state_molecule = SA of (int * fan) list ;; 

type lightweight_mold_state = U1 ;;
type heavyweight_mold_state = U2 ;; 

type mold = {
    solutions : (int list) list;
    mandatory_elements : int list;
} ;;

type mold_with_state = MWS of 
   mold * lightweight_mold_state * heavyweight_mold_state ;; 




