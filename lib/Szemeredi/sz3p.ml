(*

#use"lib/Szemeredi/sz3p.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)

(*
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

 type node_name = string ;;

 type domain_kind = Sz3p_types.domain_kind = 
 Full_two_dimensional
|Upper_half_two_dimensional
|Lower_half_two_dimensional 
|Upper_half_one_dimensional of int 
|Lower_half_one_dimensional of int ;; 

 type node_system = Sz3p_types.node_system = {
   width_and_scrappers : int * (int list) ;
   divisions_successively_made : (node_name * upwards_division * node_name list) list ;
   nodes_successively_created : (node_name * ((downwards_division * node_name) option) ) list ;
   undivided_nodes : (node_name * ((downwards_division * node_name) option) ) list ; 
   domains_for_nodes : (node_name * domain_kind) list ; 
 } ;; 


let empty_node_system (i,j) = {
  width_and_scrappers = (i,j) ;
  divisions_successively_made = [] ;
  nodes_successively_created = [] ;
  undivided_nodes = [] ; 
  domains_for_nodes = [] ;
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
 let add_pair_to_domains_for_nodes syst_ref pair =
  syst_ref:={(!syst_ref) with 
  domains_for_nodes=
   pair::((!syst_ref).domains_for_nodes)
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

let deduce_domain l = match List.hd l with 
  Bulk_result_to_superficial_result 
| Bulk_result_to_solution_list 
| Bulk_result_to_qualified_point_list   
| List_to_length 
| List_to_range (_,_) -> Full_two_dimensional
| Breadth_n_size_to_upper_half -> Upper_half_two_dimensional
| Breadth_n_size_to_lower_half -> Lower_half_two_dimensional ;; 

let add_two_sided_division old_syst old_node d_divisions = 
   let u_division = upwards_version d_divisions 
   and dom = deduce_domain d_divisions 
   and pairs = Image.image (
    fun d_division ->
       let appendix = appendix_for_downwards_division d_division in 
       let new_node =  old_node^"_"^ appendix  in 
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
    add_pair_to_domains_for_nodes syst_ref (old_node,dom)
    )
  in 
  (!syst_ref,new_nodes);;



let create_root_node (width,scrappers) = 
  let s_scrappers = (
    if scrappers = []
    then "empty"
    else "s"^(String.concat "_" (Image.image string_of_int scrappers))^"s"
  ) in 
  let root_node = "f"^(string_of_int width)^"_"^s_scrappers in
  {
    width_and_scrappers = (width,scrappers) ;
    divisions_successively_made = [];
    nodes_successively_created=[(root_node,None)];
    undivided_nodes=[(root_node,None)]; 
    domains_for_nodes=[root_node,Full_two_dimensional];
  } ;;

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

exception Apply_division_exn of node_name * downwards_division ;; 

let apply_division syst node d_division = 
   let pair = (d_division,node) in 
   match List.find_map  (
     fun (node1,opt1) -> match opt1 with 
      None -> None
     |Some(div1,node2) -> if (div1,node2)=pair then Some node1 else None
   ) syst.nodes_successively_created with 
   None -> raise(Apply_division_exn(node,d_division))
   |Some(node3) -> node3 ;;    

let apply_divisions syst node d_divisions =
    List.fold_left (apply_division syst) node d_divisions ;; 

exception Get_producing_division_exn of node_name ;; 

let get_producing_division syst node = 
      match List.find_map  (
        fun (node1,opt1) -> 
          if node1 = node 
          then opt1  
          else None
      ) syst.nodes_successively_created with 
      None -> raise(Get_producing_division_exn(node))
      |Some(div2,node2) -> (div2,node2) ;;   

exception Get_origin_exn ;; 

let get_origin syst = 
      match List.find_map  (
        fun (node1,opt1) -> match opt1 with 
         None -> Some node1
        |Some(_,_) -> None
      ) syst.nodes_successively_created with 
      None -> raise(Get_origin_exn)
      |Some(node2) -> node2 ;;   

(*      
let code_for_nonomptimized syst node =
   "let "^node^" = " ;; 
*)

let example=create_root_node (1,[]) ;;     

let origin_node = get_origin example ;;

let (example2,l_nodes2) = add_two_sided_division example origin_node 
  [Bulk_result_to_superficial_result;
   Bulk_result_to_solution_list;
   Bulk_result_to_qualified_point_list] ;; 

let qlist_node =
   apply_division example2 origin_node Bulk_result_to_qualified_point_list ;;    

let (example3,l_nodes3) = decompose_list_node_according_to_rangeset 
      example2 qlist_node [(1,1);(2,2);(3,100)] ;;

let example4 = cut_all_breadth_size_nodes_in_two example3 ;; 
*)