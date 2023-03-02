(*

#use"lib/Szemeredi/sz3p.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)

type node = Sz3p_types.node ={
   node_name : string ;
} ;;

type upwards_division = Sz3p_types.upwards_division = 
     Bulk_result_by_definition 
   | List_by_rangeset of (int * int) list
   | Breadth_n_size_by_two ;;

type downwards_division = Sz3p_types.downwards_division = 
   Bulk_result_to_superficial_result
 | Bulk_result_to_solution_list
 | Bulk_result_to_qualified_point_list    
 | List_to_length 
 | List_to_range of (int * int)
 | Breadth_n_size_to_upper_half
 | Breadth_n_size_to_lower_half ;;  

type node_system = Sz3p_types.node_system = {
  width_and_scrappers : int * (int list) ;
  divisions_successively_made : (node * upwards_division * node list) list ;
  nodes_successively_created : (node * ((downwards_division * node) option) ) list ;
  undivided_nodes : (node * ((downwards_division * node) option) ) list ; 
} ;; 


let node_eq nd1 nd2 = ( (nd1.node_name) = (nd2.node_name) ) ;; 


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

let appendix_for_downwards_division = function 
  Bulk_result_to_superficial_result -> "superficial_result"
| Bulk_result_to_solution_list -> "selected_solutions"
| Bulk_result_to_qualified_point_list -> "qpoint_list"    
| List_to_length -> "length"
| List_to_range (i,j) ->"range_"^(string_of_int i)^"_"^(string_of_int j)
| Breadth_n_size_to_upper_half -> "upper_half"
| Breadth_n_size_to_lower_half -> "lower_half" ;;  

let extract_range = function
  List_to_range (x,y) -> Some(x,y) 
| Bulk_result_to_superficial_result 
| Bulk_result_to_solution_list 
| Bulk_result_to_qualified_point_list  
| List_to_length 
| Breadth_n_size_to_upper_half 
| Breadth_n_size_to_lower_half -> None ;; 

let upwards_version l = match List.hd l with 
  Bulk_result_to_superficial_result 
| Bulk_result_to_solution_list 
| Bulk_result_to_qualified_point_list -> Bulk_result_by_definition  
| List_to_length 
| List_to_range (_,_) -> let ranges = List.filter_map extract_range l in 
                          List_by_rangeset ranges 
| Breadth_n_size_to_upper_half 
| Breadth_n_size_to_lower_half -> Breadth_n_size_by_two ;;  


let add_two_sided_division old_syst old_node d_divisions = 
   let old_name = old_node.node_name in  
   let u_division = upwards_version d_divisions 
   and pairs = Image.image (
    fun d_division ->
       let appendix = appendix_for_downwards_division d_division in 
       let new_node = {
          node_name = old_name^"_"^ appendix 
        } in 
        (d_division,new_node) 
   ) d_divisions in 
   let new_nodes = Image.image snd pairs in 
   let syst_ref = ref old_syst in 
   let _ = 
    (add_triple_to_divisions_successively_made syst_ref (old_node,u_division,new_nodes);
     List.iter (fun (d_division,new_node) ->
      add_triple_to_nodes_successively_created syst_ref (new_node,Some(d_division,old_node))
    ) pairs;
    remove_from_undivides_nodes syst_ref old_node;
    List.iter (fun (d_division,new_node) ->
      add_triple_to_undivided_nodes syst_ref (new_node,Some(d_division,old_node))
    ) pairs;
    )
  in 
  (!syst_ref,new_nodes);;



let create_root_node (width,scrappers) root_node =
  {
    width_and_scrappers = (width,scrappers) ;
    divisions_successively_made = [];
    nodes_successively_created=[(root_node,None)];
    undivided_nodes=[(root_node,None)]; 
  }
  ;;

let decompose_list_node_according_to_rangeset old_syst old_node ranges = 
  let d_divisions = List_to_length :: (Image.image (fun (x,y)->List_to_range(x,y) ) ranges) in 
  add_two_sided_division old_syst old_node d_divisions ;;
  
let cut_breadth_size_node_in_two old_syst old_node = 
  let d_divisions = [Breadth_n_size_to_upper_half;Breadth_n_size_to_lower_half] in 
  add_two_sided_division old_syst old_node d_divisions ;;
  
let cut_all_breadth_size_nodes_in_two old_syst = 
  let undivided_nodes = 
    Image.image (fun (node,_)->node) (old_syst.undivided_nodes) in 
  let syst_ref = ref old_syst in   
  let _ =List.iter (
    fun node->
      syst_ref:=fst(cut_breadth_size_node_in_two (!syst_ref) node ))
  undivided_nodes in 
  !syst_ref ;;

let node1 = node_of_string "whole" ;;

let example=create_root_node (1,[]) node1;;     

let (example2,l_nodes2) = add_two_sided_division example node1 
  [Bulk_result_to_superficial_result;
   Bulk_result_to_solution_list;
   Bulk_result_to_qualified_point_list] ;; 

let (example3,l_nodes3) = decompose_list_node_according_to_rangeset 
      example2 (List.nth l_nodes2 2) [(1,1);(2,2);(3,100)] ;;


let example4 = cut_all_breadth_size_nodes_in_two example3 ;; 
