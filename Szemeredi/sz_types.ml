(*

#use"Szemeredi/sz_types.ml";;

*)

type hook = 
     Passive_repeat 
   | Boundary_increment 
   | Fork 
   | Jump ;;

type point = P of int * int * int * (int list) ;;

type constraint_t = C of int list ;; 

type qualified_point = Q of point * (constraint_t list) * (int list);;

type forced_data = FD of ((int list) list) * ( qualified_point list) ;;

type closest_ancestry = ( hook * (point list)) option ;;

type bulk_result = BR of closest_ancestry * ((int list) list) * forced_data ;;  

type for_width_one = FW1 of ((int * bulk_result) list) ;;

type function_of_size = 
Width_one of for_width_one 
|Usual_fos of (int -> bulk_result) ;; 

type function_of_breadth_and_size = 
Usual_fobas of ( int -> int -> bulk_result) ;;

