(*

#use"Szemeredi/sz_types.ml";;

*)

type hook = 
Passive_repeat 
| Fork 
| Jump ;;

type point = P of int * int * int * (int list) ;;

type constraint_t = C of int list ;; 

type extension_data = int list ;; 

type qualified_point = Q of point * (constraint_t list) * extension_data;;

type solution = int list ;; 

type mold = M of (solution list) * ( qualified_point list) ;;
  
type ancestry_info = AI of (point * extension_data) list ;;
  
type superficial_result = 
  Atomic
| Decomposable of point * extension_data   
| Contraction_surface of point * constraint_t  
| Fork_surface of (point * extension_data) list
| Jump_surface of point * extension_data ;;

type bulk_result = BR of ((hook * ancestry_info) option) * mold ;;  

type function_of_size = 
Usual_fos of (int -> bulk_result) ;; 

type function_of_breadth_and_size = 
Usual_fobas of ( int -> int -> bulk_result) ;;