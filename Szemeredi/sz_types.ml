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

type extension_data = int list ;; 

type qualified_point = Q of point * (constraint_t list) * extension_data;;

type solution = int list ;; 

type forced_data = FD of (solution list) * ( qualified_point list) ;;

type ancestor_category =
Dead  
|Alive ;;

type ancestry_info = AI of (ancestor_category * point * extension_data) list ;;

type partial_result = PR of (solution list) * forced_data  ;;

type bulk_result = BR of ((hook * ancestry_info) option) * partial_result ;;  

type function_of_size = 
Usual_fos of (int -> bulk_result) ;; 

type function_of_breadth_and_size = 
Usual_fobas of ( int -> int -> bulk_result) ;;