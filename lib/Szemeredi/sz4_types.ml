(*

#use"lib/Szemeredi/sz4_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz4 is short for "fourth stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type extension_data = int list ;; 

type solution = int list ;; 

type point = P of finite_int_set * width ;; 

type quantify_constraints =
    Some_constraints
    |All_constraints ;; 

type point_with_breadth = 
  No_constraint of finite_int_set
 |Usual of quantify_constraints * point * int ;; 
  

type handle = 
     Has_no_constraints
    |Select of int * int * int   
    |Rightmost_overflow of int * int * int 
    |Rightmost_pivot of width
    |Fork of int * int * int ;;

type helper = 
  Help_with_solution of point_with_breadth * solution 
 |Help_with_links of point_with_breadth * (int list) ;; 

type fan = F of int list list ;; 

type small_mold = SM of (solution list) * fan ;; 

type mold = BM of extension_data * (int * small_mold) list ;;

type common_table = CT of  (point_with_breadth * (handle * mold)) list ;; 
  
type diagnosis =
   Missing_fan of string * point_with_breadth * int * fan 
  |Missing_solution of string * point_with_breadth * solution
  |Missing_subcomputation of string * point_with_breadth 
  |Missing_switch_in_fork of int * point_with_breadth ;;
      
type fan_related_requirement = FRR of (int * fan) list ;;   

type canonized_requirement = CR of point_with_breadth * int * fan ;; 

type point_with_requirements = PWR of point_with_breadth * ((int * fan) list) ;;