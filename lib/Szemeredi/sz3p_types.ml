(*

#use"lib/Szemeredi/sz3p_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)


type node_kind =
    Whole 
    (* depth 1 *)
   |Superficial_result 
   |Solution_list 
   |Qualified_point_list 
   (* depth 2 *)
   |Qpl_length
   |Qpl_interval 
   (* depth 3 *)  
   |Sr_upper_half 
   |Sr_lower_half 
   |Sl_upper_half 
   |Sl_lower_half 
   |Qpll_upper_half  
   |Qpll_lower_half 
   |Qpli_upper_half  
   |Qpli_lower_half 
   (* depth 4 *)  
   |Sr_upper_half_atomized 
   |Sr_lower_half_atomized 
   |Sl_upper_half_atomized 
   |Sl_lower_half_atomized 
   |Qpll_upper_half_atomized  
   |Qpll_lower_half_atomized 
   |Qpli_upper_half_atomized  
   |Qpli_lower_half_atomized ;; 


(*
type width_and_scrappers = int * (int list) ;; 

type subfunction_without_width_and_scrappers =
    Whole 
   |Superficial_result 
   |Solution_list 
   |Qualified_point_list 
   |Qpl_length
   |Qpl_interval of int * int 
   |Sr_upper_half 
   |Sr_lower_half 
   |Sl_upper_half 
   |Sl_lower_half 
   |Qpll_upper_half  
   |Qpll_lower_half 
   |Qpli_upper_half of int * int 
   |Qpli_lower_half of int * int  ;; 

type subfunction = 
  SF of subfunction_without_width_and_scrappers * width_and_scrappers ;;   

type downwards_division = 
    Bulk_result_to_superficial_result
  | Bulk_result_to_solution_list
  | Bulk_result_to_qualified_point_list   
  | List_to_length  
  | List_to_range of (int * int)
  | Breadth_n_size_to_upper_half
  | Breadth_n_size_to_lower_half ;;  




type upwards_division = 
     Bulk_result_by_definition 
   | List_by_rangeset of (int * int) list
   | Breadth_n_size_by_two ;;  


type node_name =  string ;;

type domain_kind =
   Full_two_dimensional
  |Upper_half_two_dimensional
  |Lower_half_two_dimensional 
  |Upper_half_one_dimensional of int 
  |Lower_half_one_dimensional of int ;; 

type node_system = {
  width_and_scrappers : int * (int list) ;
  divisions_successively_made : (node_name * upwards_division * node_name list) list ;
  nodes_successively_created : (node_name * ((downwards_division * node_name) option) ) list ;
  undivided_nodes : (node_name * ((downwards_division * node_name) option) ) list ; 
  domains_for_nodes : (node_name * domain_kind) list ; 
} ;; 

*)