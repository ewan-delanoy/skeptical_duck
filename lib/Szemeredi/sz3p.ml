(*

#use"lib/Szemeredi/sz3p.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)

type node = Sz3p_types.node ={
   node_name : string ;
} ;;

type division = Sz3p_types.division = {
  division_name : string ; 
} ;;

type node_system = Sz3p_types.node_system = {
  width_and_scrappers : int * (int list) ;
  divisions_successively_made : (node * division * node list) list ;
  nodes_successively_created : (node * ((division * node) option) ) list ;
  undivided_nodes : (node * ((division * node) option) ) list ; 
} ;; 


let node_eq nd1 nd2 = ( (nd1.node_name) = (nd2.node_name) ) ;; 

let tripartite_division = {
  division_name = "tripartite_division"
} ;; 

let node_of_string s = { node_name = s } ;; 

let empty_node_system (i,j) = {
  width_and_scrappers = (i,j) ;
  divisions_successively_made = [] ;
  nodes_successively_created = [] ;
  undivided_nodes = [] ; 
} ;; 

let add_triple_to_divisions_successively_made syst_ref triple =
  syst_ref:={(!syst_ref) with 
  divisions_successively_made=
   triple::((!syst_ref).divisions_successively_made)
 } ;;

let add_triple_to_nodes_successively_created syst_ref triple =
  syst_ref:={(!syst_ref) with 
  nodes_successively_created=
   triple::((!syst_ref).nodes_successively_created)
 } ;; 
let remove_from_undivides_nodes syst_ref old_node = 
  syst_ref:={(!syst_ref) with 
  undivided_nodes =
  List.filter (fun (node1,_)->
    node1 <> old_node
  ) ((!syst_ref).undivided_nodes)
 } ;; 
 let add_triple_to_undivided_nodes syst_ref triple =
  syst_ref:={(!syst_ref) with 
  undivided_nodes=
   triple::((!syst_ref).undivided_nodes)
 } ;; 

let add_one_more_division old_syst old_node division new_nodes =
   let syst_ref = ref old_syst in 
   let _ = 
    (add_triple_to_divisions_successively_made syst_ref (old_node,division,new_nodes);
     List.iter (fun new_node ->
      add_triple_to_nodes_successively_created syst_ref (new_node,Some(division,old_node))
    ) new_nodes;
    remove_from_undivides_nodes syst_ref old_node;
    List.iter (fun new_node ->
      add_triple_to_undivided_nodes syst_ref (new_node,Some(division,old_node))
    ) new_nodes;
    )
  in 
  !syst_ref;;


let add_typical_division old_syst old_node appendix = 
  let old_name = old_node.node_name in 
  let new_node = {
    node_name = old_name^"_"^ appendix 
  }
  and new_division = {
    division_name = appendix ^ "_for_"^old_name
  } in 
  add_one_more_division old_syst old_node new_division [new_node]  ;; 

let create_root_node (i,j) root_node =
  {
    width_and_scrappers = (i,j) ;
    divisions_successively_made = [];
    nodes_successively_created=[(root_node,None)];
    undivided_nodes=[(root_node,None)]; 
  }
  ;;

let decompose_list_node_according_to_rangeset old_syst old_node ranges = 
  let syst_ref = ref old_syst in 
  let _ = 
    (syst_ref:=add_typical_division (!syst_ref) old_node "length"; 
  List.iter (
    fun (i_min,i_max) ->
       let s_min = string_of_int i_min 
       and s_max = string_of_int i_max in 
       let r = "range_"^s_min^"_"^s_max in 
       syst_ref:=add_typical_division (!syst_ref) old_node r 
  ) ranges) in 
 !syst_ref;;
  
let cut_breadth_size_node_in_two old_syst old_node = 
  let syst1 = add_typical_division old_syst old_node "upper_half" in 
  add_typical_division syst1 old_node "lower_half" ;; 

let cut_all_breadth_size_nodes_in_two old_syst = 
  let undivided_nodes = 
    Image.image (fun (node,_)->node) (old_syst.undivided_nodes) in 
  let syst_ref = ref old_syst in   
  let _ =List.iter (
    fun node->
      syst_ref:=cut_breadth_size_node_in_two (!syst_ref) node )
  undivided_nodes in 
  !syst_ref ;;

let node1 = node_of_string "whole" ;;

let example=create_root_node (1,[]) node1;;     

let node2 = node_of_string "superficial_result" ;;
let node3 = node_of_string "selected_solutions" ;;
let node4 = node_of_string "qpoint_list" ;;

let example2 = add_one_more_division example node1 tripartite_division [node2;node3;node4];; 

let example3 = decompose_list_node_according_to_rangeset example2 node4 [(1,1);(2,2);(3,100)] ;;

let example4 = cut_all_breadth_size_nodes_in_two example3 ;; 
