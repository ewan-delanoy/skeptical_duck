(*

#use"lib/Szemeredi/sz3p_types.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)

type node = {
   node_name : string ;
} ;;

type division = 
     Bulk_result_by_definition 
   | List_by_rangeset of (int * int) list
   | Breadth_n_size_by_two ;;  

type node_system = {
  width_and_scrappers : int * (int list) ;
  divisions_successively_made : (node * division * node list) list ;
  nodes_successively_created : (node * ((division * node) option) ) list ;
  undivided_nodes : (node * ((division * node) option) ) list ; 
} ;; 
