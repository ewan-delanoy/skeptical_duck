(*

#use"Szemeredi/sz_types.ml";;

*)

type increment_in_knowledge = 
    Boundary_increment
   | Passive_repeat  
   | Fork ;;

type uniform_parametrized_subrange = {
      positive_exceptions : int list ;
      negative_exceptions : int list ;
      modulus : int ;
      usual :  int list ;
   } ;; 

type  parametrized_subrange = {
   ps_exceptions : (int * (int list)) list ;
   ps_usual : uniform_parametrized_subrange ; 
} ;; 

type parametrized_ps_list = {
   pl_exceptions : (int * (int list list)) list ;
   pl_usual : parametrized_subrange list ;
} ;; 
   
