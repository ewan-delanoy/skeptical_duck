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

type sycomore_list = 
   Singleton of int list 
   | Breakpoint_with_extensions of point * ((int list) list) * (int list);;


type for_width_one = FW1 of ((int * sycomore_list) list) ;;

type function_of_size = 
  Width_one of for_width_one 
  |Usual_fos of (int -> sycomore_list) ;; 

type function_of_scrappers_and_size = 
  Usual_foscras of ( (int list) -> int -> sycomore_list) ;;