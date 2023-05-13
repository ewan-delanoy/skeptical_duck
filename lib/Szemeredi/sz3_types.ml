(*

#use"lib/Szemeredi/sz3_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type old_upper_bound_for_constraints = UBC of int * width ;; 

type upper_bound_on_breadth = Unrestricted |Up_to of int ;;  

type extension_data = int list ;; 

type solution = int list ;; 

type mold = M of (solution list) * extension_data ;;

type old_key = finite_int_set * old_upper_bound_for_constraints ;;  
  
type key = finite_int_set * upper_bound_on_breadth ;; 

type old_peek_result = 
    P_Success of mold  
   |P_Failure
   |P_Unfinished_computation of old_key list ;;

type severity = Stern | Relaxed ;; 

type breadth_range = Br_Unrestricted |Br_Up_to of int ;; 
