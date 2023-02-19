(*

#use"lib/Szemeredi/sz_preprocessing_for_third_stab.ml";;

*)

let ref_for_divisions_successively_made = ref ([]: (string * string * string list) list) ;; 
let ref_for_nodes_successively_created = ref ([]: (string * string * string) list) ;; 

let refs () = (!ref_for_divisions_successively_made,!ref_for_nodes_successively_created) ;;

let add_one_more_division old_node division new_nodes =
   let _ = (ref_for_divisions_successively_made:=
     (old_node,division,new_nodes)::(!ref_for_divisions_successively_made)
   ) in 
  List.iter (fun new_node ->
    ref_for_nodes_successively_created:=
    (new_node,division,old_node)::(!ref_for_nodes_successively_created)
    ) new_nodes ;;

exception Create_root_node_exn of string ;;

let create_root_node root_node =
    if ((!ref_for_divisions_successively_made),(!ref_for_nodes_successively_created))<>([],[])
    then raise(Create_root_node_exn root_node) 
    else ref_for_nodes_successively_created:=[(root_node,"no_division","")] ;;

let decompose_list_node_according_to_rangeset old_node ranges = 
  let _ = add_one_more_division old_node ("length_for_"^old_node) [old_node^"_length"] in 
  List.iter (
    fun (i_min,i_max) ->
       let s_min = string_of_int i_min 
       and s_max = string_of_int i_max in 
       let r = "range_"^s_min^"_"^s_max in 
       add_one_more_division old_node (r^"_for_"^old_node) [old_node^"_"^r] 
  ) ranges ;;
  

let node1_name = "whole" ;;

create_root_node node1_name ;;     

let node2_name = "superficial_result" ;;
let node3_name = "selected_solutions" ;;
let node4_name = "qpoint_list" ;;

add_one_more_division node1_name "tripartite" [node2_name;node3_name;node4_name];; 

decompose_list_node_according_to_rangeset node4_name [(1,1);(2,2);(3,100)] ;;
