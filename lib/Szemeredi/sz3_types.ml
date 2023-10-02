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

type piece_of_help = {
   beneficiary : point_with_breadth ;
   extra_solutions : (int * solution list) list;
   imposed_fans : (int *fan) list;
   extra_groove_for_fork : int list;
} ;; 




type small_mold = SM of (solution list) * fan ;; 

type mold = BM of extension_data * (int * small_mold) list ;;

type grocery = {
  helpers : piece_of_help list;
  pair_level : ((width * int list) * (int -> int -> handle * mold)) list;
  triple_level : ((width * int list * int) * (int -> handle * mold)) list;
  low_level : (point_with_breadth * (handle * mold)) list;
} ;;
  