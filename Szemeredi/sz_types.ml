(*

#use"Szemeredi/sz_types.ml";;

*)

type hook_in_knowledge = 
    Boundary_increment
   | Passive_repeat  
   | Fork 
   | Jump ;;

type hungarian_adjuster =
     Leave_unchanged 
    |Adjust of int list ;; 

type point = P of int * int * int * (int list) ;;

type cons_traint = C of int ;; 

type qualified_point = Q of point * ((int list) list) * (int list);;

type sycomore_list = 
   Singleton of int list 
   | Breakpoint_with_extensions of qualified_point ;;

type forced_data = FD of ((int list) list) * ( qualified_point list) ;;

type bulk_result = BR of sycomore_list ;;  

type enhanced_bulk_result = EBR of sycomore_list * ((int list) list) * forced_data ;;  

type for_width_one = FW1 of ((int * bulk_result) list) ;;

type function_of_size = 
  Width_one of for_width_one 
  |Usual_fos of (int -> bulk_result) ;; 

type function_of_scrappers_and_size = 
  Usual_foscras of ( (int list) -> int -> bulk_result) ;;

