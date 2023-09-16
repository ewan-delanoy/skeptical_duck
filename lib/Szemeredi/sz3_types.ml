(*

#use"lib/Szemeredi/sz3_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type extension_data = int list ;; 

type solution = int list ;; 

type point = P of finite_int_set * width ;; 

type point_with_extra_constraints = PEC of point * (constraint_t list);;

type point_with_breadth = PWB of point * int ;; 

type handle = 
     Discrete
    |Select of int * int * int   
    |Rightmost_overflow of int * int * int 
    |Rightmost_pivot of width
    |Fork of int * int * int ;;

type helper = 
  Help_with_solution of point_with_breadth * solution 
 |Help_with_links of point_with_breadth * (int list) ;; 

type fan = F of int list list ;; 

type torsion = T of (int*fan) list ;; 

type torsionfree_mold = TFM of (solution list) * extension_data ;;

type mold = MM of (solution list) * extension_data * torsion ;;    

type diagnosis  = 
 Missing_treatment of point_with_breadth 
|Incomplete_treatment of point_with_breadth 
|Missing_links of point_with_breadth * (int list)
|Finished of handle  * mold * bool ;;    
 
type grocery = {
  helpers : helper list;
  pair_level : ((width * int list) * (int -> int -> handle * mold)) list;
  triple_level : ((width * int list * int) * (int -> handle * mold)) list;
  low_level : (point_with_breadth * (handle * mold)) list;
} ;; 
