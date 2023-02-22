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
  divisions_successively_made : (node * division * node list) list ;
  nodes_successively_created : (node * division * node) list ;
  undivided_nodes : (node * division * node) list ; 
} ;; 


let node_eq nd1 nd2 = ( (nd1.node_name) = (nd2.node_name) ) ;; 

let no_division = {
  division_name = "no_division"
} ;; 

let tripartite_division = {
  division_name = "no_division"
} ;; 

let node_of_string s = { node_name = s } ;; 

let ref_for_divisions_successively_made = ref ([]: (node * division * node list) list) ;; 
let ref_for_nodes_successively_created = ref ([]: (node * division * node) list) ;; 
let ref_for_undivided_nodes = ref ([]: (node * division * node) list) ;; 

let refs () = (!ref_for_divisions_successively_made,
               !ref_for_nodes_successively_created,
               !ref_for_undivided_nodes) ;;

let add_one_more_division old_node division new_nodes =
   let _ = (ref_for_divisions_successively_made:=
     (old_node,division,new_nodes)::(!ref_for_divisions_successively_made)
   ) in 
  let _ = List.iter (fun new_node ->
    ref_for_nodes_successively_created:=
    (new_node,division,old_node)::(!ref_for_nodes_successively_created)
    ) new_nodes in 
  let _ = (
    ref_for_undivided_nodes := List.filter (fun (node1,_,_)->
        node1 <> old_node
      ) (!ref_for_undivided_nodes)
  ) in 
  List.iter (fun new_node ->
    ref_for_undivided_nodes:=
    (new_node,division,old_node)::(!ref_for_undivided_nodes)
    ) new_nodes;;

let add_typical_division old_node appendix = 
  let old_name = old_node.node_name in 
  let new_node = {
    node_name = old_name^"_"^ appendix 
  }
  and new_division = {
    division_name = appendix ^ "_for_"^old_name
  } in 
  add_one_more_division old_node new_division [new_node]  ;; 

exception Create_root_node_exn of string ;;

let create_root_node root_node =
    if ((!ref_for_divisions_successively_made),(!ref_for_nodes_successively_created))<>([],[])
    then raise(Create_root_node_exn root_node.node_name) 
    else let empty_node = node_of_string "" in  
        (ref_for_nodes_successively_created:=[(root_node,no_division,empty_node)];
          ref_for_undivided_nodes:=[(root_node,no_division,empty_node)]; 
         ) ;;

let decompose_list_node_according_to_rangeset old_node ranges = 
  let _ = add_typical_division old_node "length" in 
  List.iter (
    fun (i_min,i_max) ->
       let s_min = string_of_int i_min 
       and s_max = string_of_int i_max in 
       let r = "range_"^s_min^"_"^s_max in 
       add_typical_division old_node r 
  ) ranges ;;
  
let cut_breadth_size_node_in_two old_node = 
  (add_typical_division old_node "upper_half" ;
   add_typical_division old_node "lower_half") ;; 

let cut_all_breadth_size_nodes_in_two () = 
  let undivided_nodes = Image.image (fun (node,_,_)->node) (!ref_for_undivided_nodes) in 
  List.iter cut_breadth_size_node_in_two undivided_nodes ;;

let node1 = node_of_string "whole" ;;

create_root_node node1 ;;     

let node2 = node_of_string "superficial_result" ;;
let node3 = node_of_string "selected_solutions" ;;
let node4 = node_of_string "qpoint_list" ;;

add_one_more_division node1 tripartite_division [node2;node3;node4];; 

decompose_list_node_according_to_rangeset node4 [(1,1);(2,2);(3,100)] ;;

cut_all_breadth_size_nodes_in_two () ;; 