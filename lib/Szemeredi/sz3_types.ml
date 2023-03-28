(*

#use"lib/Szemeredi/sz3_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".


*)

type breadth = B of int ;; 

type size = S of int ;;

type point = 
  Empty_point
 |P of int *  (int list) * breadth * size  ;;

type constraint_t = C of int list ;; 

type extension_data = int list ;; 

type qualified_point = Q of point * (constraint_t list) * extension_data;;

type solution = int list ;; 

type mold = M of (solution list) * ( qualified_point list) ;;
  
type superficial_result = 
  Atomic
| Decomposable of point * extension_data  
| Contraction of point * constraint_t  
| Fork of (point * extension_data) list ;;

type bulk_result = BR of superficial_result * mold ;;  

type half = Lower_half | Upper_half ;;

type kind_of_missing_part = KMP of int ;; 

type index_of_missing_data = IMD of int ;;

type visualization_result =
    VR1 of ((breadth * size) * superficial_result) list 
   |VR2 of ((breadth * size) * solution list) list 
   |VR3 of ((breadth * size) * int) list 
   |VR4 of ((breadth * size) * point) list 
   |VR5 of ((breadth * size) * constraint_t list) list 
   |VR6 of ((breadth * size) * extension_data) list ;; 
   
   