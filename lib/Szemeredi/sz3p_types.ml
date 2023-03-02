(*

#use"lib/Szemeredi/sz3p_types.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)

type node = {
   node_name : string ;
} ;;

type upwards_division = 
     Bulk_result_by_definition 
   | List_by_rangeset of (int * int) list
   | Breadth_n_size_by_two ;;  

(*   
type downwards_division = 
   Bulk_result_to_superficial_result
 | Bulk_result_to_solution_list
 | Bulk_result_to_qualified_point_list    
 | List_to_range of (int * int)
 | Breadth_n_size_to_upper_half
 | Breadth_n_size_to_lower_half ;;  
*)

type node_system = {
  width_and_scrappers : int * (int list) ;
  divisions_successively_made : (node * upwards_division * node list) list ;
  nodes_successively_created : (node * ((upwards_division * node) option) ) list ;
  undivided_nodes : (node * ((upwards_division * node) option) ) list ; 
} ;; 
