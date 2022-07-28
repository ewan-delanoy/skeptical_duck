(*

#use"Szemeredi/sz_types.ml";;

*)

type hook_in_knowledge = 
    Boundary_increment
   | Passive_repeat  
   | Fork ;;

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

type level_two_t = Quick ;; 
  

   
