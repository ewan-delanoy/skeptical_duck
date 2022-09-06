(*

#use"Szemeredi/sz_types.ml";;

*)

type hook_in_knowledge = 
    Boundary_increment
   | Passive_repeat  
   | Fork 
   | Jump ;;

type rubber_core_list = L of string ;;

type rubber_list =
     Short_list of int list list 
    |Rubber of rubber_core_list * (int list) ;;

type rubber_definition = 
     Constraining of rubber_core_list * (int list list) 
    |Merger of (int list list) * ((rubber_core_list * (int list)) list)
    |Short_name of int list list ;;

type parametrized_uniform_subrange = {
   usr_positive_exceptions : int list ;
   usr_negative_exceptions : int list ;
   usr_modulus : int ;
   usr_usual :  int list ;
} ;; 

type  parametrized_subrange = {
   ps_exceptions : (int * (int list)) list ;
   ps_usual : parametrized_uniform_subrange ; 
} ;; 

type parametrized_ps_list = {
   pl_exceptions : (int * (int list list)) list ;
   pl_usual : parametrized_subrange list ;
} ;; 

type hungarian_adjuster =
     Leave_unchanged 
    |Adjust of int list ;; 

type level_two_t = Quick of int list ;; 

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