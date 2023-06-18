(*

#use"lib/Szemeredi/sz3_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".


*)

type width = W of int ;; 

type breadth = B of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 


type extension_data = int list ;; 

type solution = int list ;; 

type fan = F of extension_data list ;; 

type mold = M of (solution list) * (extension_data list) ;;

type upper_bound_on_breadth = Unrestricted |Up_to of breadth ;;

type upper_bound_on_constraint = UBC of width * upper_bound_on_breadth ;; 

type key = Key of finite_int_set * upper_bound_on_constraint ;; 

type medium_hook = Mh_cumulative of int | Mh_select of int * int *int | Mh_fork of int * int *int  ;; 

type simplified_key = int * (int list) * int * int ;;

type partially_polished = PP of (simplified_key * (medium_hook * mold)) list ;; 