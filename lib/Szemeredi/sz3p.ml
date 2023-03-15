(*

#use"lib/Szemeredi/sz3p.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3p is short for "Preprocessing for third stab at Szemeredi problem".

*)


type node_kind = Sz3p_types.node_kind =
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

type division =  Sz3p_types.division =
   Bulk_result_to_superficial_result
 | Bulk_result_to_solution_list
 | Bulk_result_to_qualified_point_list   
 | List_to_length  
 | List_to_range of (int * int)
 | Breadth_n_size_to_upper_half
 | Breadth_n_size_to_lower_half 
 | Atomize of int ;;  

exception Divide_by_exn of node_kind * string ;;

module Node_Internals = struct 

  type division_data = {
    range_start : int ;
    range_end : int ;
    atomization_parameter : int ; 
  } ;; 

  let cumulate dd1 dd2 = {
    range_start = max (dd1.range_start) (dd2.range_start) ;
    range_end = max (dd1.range_end) (dd2.range_end) ;
    atomization_parameter = max (dd1.range_end) (dd2.range_end) ; 
  } ;; 
  
  let division_data_constructor (rs,re,ap) = {
    range_start = rs ;
    range_end = re ;
    atomization_parameter = ap ; 
    } ;;

  let inactive_division_data = division_data_constructor (0,0,0) ;;   


   type t = {
     kind : node_kind ;
     div_data : division_data ;
     width : int ;
     scrappers : int ;
   } ;; 
 
   let constructor (k,rs,re,ap,w,scr) = {
     kind = k ;
     div_data = division_data_constructor (rs,re,ap) ;
     width = w ;
     scrappers = scr ;
   } ;; 

   let to_uple node =
     (
       node.kind,
       node.div_data.range_start,
       node.div_data.range_end,
       node.div_data.atomization_parameter,
       node.width,
       node.scrappers
     ) ;;

   type division_kind = 
    Bulk_result_to_superficial_result_k
  | Bulk_result_to_solution_list_k
  | Bulk_result_to_qualified_point_list_k   
  | List_to_length_k  
  | List_to_range_k 
  | Breadth_n_size_to_upper_half_k
  | Breadth_n_size_to_lower_half_k 
  | Atomize_k ;;  

  
  let get_division_kind division =
    match division with 
     Bulk_result_to_superficial_result -> Bulk_result_to_superficial_result_k
   | Bulk_result_to_solution_list -> Bulk_result_to_solution_list_k
   | Bulk_result_to_qualified_point_list -> Bulk_result_to_qualified_point_list_k  
   | List_to_length -> List_to_length_k
   | List_to_range(_,_) -> List_to_range_k
   | Breadth_n_size_to_upper_half -> Breadth_n_size_to_upper_half_k
   | Breadth_n_size_to_lower_half -> Breadth_n_size_to_lower_half_k
   | Atomize(_) -> Atomize_k;;  

  

  let get_division_data = function 
  Bulk_result_to_superficial_result
  | Bulk_result_to_solution_list 
  | Bulk_result_to_qualified_point_list  
  | List_to_length 
  | Breadth_n_size_to_upper_half 
  | Breadth_n_size_to_lower_half -> inactive_division_data
  | List_to_range(rs,re) -> division_data_constructor (rs,re,0)
  | Atomize(idx) -> division_data_constructor (0,0,idx);;  


  let fill_division division_kind (_k,rs,re,ap,_w,_scr) =
     match division_kind with 
      Bulk_result_to_superficial_result_k -> Bulk_result_to_superficial_result
    | Bulk_result_to_solution_list_k -> Bulk_result_to_solution_list
    | Bulk_result_to_qualified_point_list_k -> Bulk_result_to_qualified_point_list  
    | List_to_length_k -> List_to_length
    | List_to_range_k -> List_to_range (rs,re)
    | Breadth_n_size_to_upper_half_k -> Breadth_n_size_to_upper_half
    | Breadth_n_size_to_lower_half_k -> Breadth_n_size_to_lower_half
    | Atomize_k -> Atomize(ap);;  

   

   let main_array = [
    (Whole,None);
(Superficial_result,Some(Bulk_result_to_superficial_result_k,Whole));
(Solution_list,Some(Bulk_result_to_solution_list_k,Whole));
(Qualified_point_list,Some(Bulk_result_to_qualified_point_list_k,Whole));
(Qpl_length,Some(List_to_length_k,Qualified_point_list));
(Qpl_interval,Some(List_to_range_k,Qualified_point_list));
(Sr_upper_half,Some(Breadth_n_size_to_upper_half_k,Superficial_result));
(Sr_lower_half,Some(Breadth_n_size_to_upper_half_k,Superficial_result));
(Sl_upper_half,Some(Breadth_n_size_to_upper_half_k,Solution_list));
(Sl_lower_half,Some(Breadth_n_size_to_upper_half_k,Solution_list));
(Qpll_upper_half,Some(Breadth_n_size_to_upper_half_k,Qpl_length));
(Qpll_lower_half,Some(Breadth_n_size_to_upper_half_k,Qpl_length));
(Qpli_upper_half,Some(Breadth_n_size_to_upper_half_k,Qpl_interval));
(Qpli_lower_half,Some(Breadth_n_size_to_upper_half_k,Qpl_interval));
(Sr_upper_half_atomized,Some(Atomize_k,Sr_upper_half));
(Sr_lower_half_atomized,Some(Atomize_k,Sr_lower_half));
(Sl_upper_half_atomized,Some(Atomize_k,Sl_upper_half));
(Sl_lower_half_atomized,Some(Atomize_k,Sl_lower_half));
(Qpll_upper_half_atomized,Some(Atomize_k,Qpll_upper_half));
(Qpll_lower_half_atomized,Some(Atomize_k,Qpll_lower_half));
(Qpli_upper_half_atomized,Some(Atomize_k,Qpli_upper_half));
(Qpli_lower_half_atomized,Some(Atomize_k,Qpli_lower_half));
   ] ;;

   let canonical_decomposition_opt node =
     match List.assoc node.kind main_array with 
     None -> None 
     |Some(division_kind,parent_kind) ->
       let uple = to_uple node in 
       Some(fill_division division_kind uple,
            {node with kind = parent_kind}) ;;          
         
  let division_kind_to_string = function 
     Bulk_result_to_superficial_result_k -> "Bulk_result_to_superficial_result"
   | Bulk_result_to_solution_list_k -> "Bulk_result_to_solution_list"
   | Bulk_result_to_qualified_point_list_k -> "Bulk_result_to_qualified_point_list"  
   | List_to_length_k -> "List_to_length"
   | List_to_range_k -> "List_to_range" 
   | Breadth_n_size_to_upper_half_k -> "Breadth_n_size_to_upper_half"
   | Breadth_n_size_to_lower_half_k -> "Breadth_n_size_to_lower_half"
   | Atomize_k -> "Atomize" ;;  

  let divide_by node division = 
    let node_kind = node.kind 
    and division_kind = get_division_kind division in
    match List.find_map (
      fun (child_kind,opt) -> match opt with 
       None -> None 
      |Some(div_kind,parent_kind) ->
          if (div_kind,parent_kind) <> (division_kind,node_kind) 
          then None 
          else Some(child_kind)  
    )  main_array with 
     None -> raise(Divide_by_exn(node_kind,division_kind_to_string division_kind)) 
    |Some(child_kind) -> 
      {node with
        kind = child_kind ;
        div_data = cumulate (node.div_data) (get_division_data division)
      } ;;
    

   let root_node (w,scr) = constructor (Whole,0,0,0,w,scr) ;; 


end ;;   

module type NODE_SIGNATURE = 
  sig
    type division_data = {
      range_start : int;
      range_end : int;
      atomization_parameter : int;
    }
    type t = {
      kind : node_kind;
      div_data : division_data;
      width : int;
      scrappers : int;
    }
    val canonical_decomposition_opt : t -> (division * t) option
    
    val divide_by : t -> division -> t
    val root_node : int * int -> t
  end ;;

module Node = (Node_Internals:NODE_SIGNATURE) ;; 

(*
 

module Node_Internals = struct 

   type t = {
    kind : node_kind ;
    range_start : int ;
    range_end : int ;
    atomization_parameter : int ; 
    width : int ;
    scrappers : int ;
  } ;; 

  let constructor (k,rs,re,ap,w,scr) = {
    kind = k ;
    range_start = rs ;
    range_end = re ;
    atomization_parameter = ap ; 
    width = w ;
    scrappers = scr ;
  } ;; 

let whole (w,scr) = constructor (Whole,0,0,0,w,scr) ;;
let superficial_result (w,scr) = constructor (Superficial_result,0,0,0,w,scr) ;;
let solution_list (w,scr) = constructor (Solution_list,0,0,0,w,scr) ;;
let qualified_point_list (w,scr) = constructor (Qualified_point_list,0,0,0,w,scr) ;;
let qpl_length (w,scr) = constructor (Qpl_length,0,0,0,w,scr) ;;
let qpl_interval (w,scr) (i,j) = constructor (Qpl_interval,i,j,0,w,scr) ;;
let sr_upper_half (w,scr) = constructor (Sr_upper_half,0,0,0,w,scr) ;;
let sr_lower_half (w,scr) = constructor (Sr_lower_half,0,0,0,w,scr) ;;
let sl_upper_half (w,scr) = constructor (Sl_upper_half,0,0,0,w,scr) ;;
let sl_lower_half (w,scr) = constructor (Sl_lower_half,0,0,0,w,scr) ;;
let qpll_upper_half (w,scr) = constructor (Qpll_upper_half,0,0,0,w,scr) ;;
let qpll_lower_half (w,scr) = constructor (Qpll_lower_half,0,0,0,w,scr) ;;
let qpli_upper_half (w,scr) (i,j) = constructor (Qpli_upper_half,i,j,0,w,scr) ;;
let qpli_lower_half (w,scr) (i,j) = constructor (Qpli_lower_half,i,j,0,w,scr) ;;
let sr_upper_half_atomized (w,scr) = constructor (Sr_upper_half_atomized,0,0,0,w,scr) ;;
let sr_lower_half_atomized (w,scr) = constructor (Sr_lower_half_atomized,0,0,0,w,scr) ;;
let sl_upper_half_atomized (w,scr) = constructor (Sl_upper_half_atomized,0,0,0,w,scr) ;;
let sl_lower_half_atomized (w,scr) = constructor (Sl_lower_half_atomized,0,0,0,w,scr) ;;
let qpll_upper_half_atomized (w,scr) a = constructor (Qpll_upper_half_atomized,0,0,a,w,scr) ;;
let qpll_lower_half_atomized (w,scr) a = constructor (Qpll_lower_half_atomized,0,0,a,w,scr) ;;
let qpli_upper_half_atomized (w,scr) (i,j) a = constructor (Qpli_upper_half_atomized,i,j,a,w,scr) ;;
let qpli_lower_half_atomized (w,scr) (i,j) a = constructor (Qpli_lower_half_atomized,i,j,a,w,scr) ;;


end ;;



let canonical_decomposition_opt sf = match sf with 
  Whole -> None 
 |Superficial_result -> Some(Bulk_result_to_superficial_result,Whole) 
 |Solution_list -> Some(Bulk_result_to_solution_list,Whole)
 |Qualified_point_list -> Some(Bulk_result_to_qualified_point_list,Whole)
 |Qpl_length -> Some(List_to_length,Qualified_point_list)
 |Qpl_interval  -> Some(List_to_range,Qualified_point_list)
 |Sr_upper_half -> Some(Breadth_n_size_to_upper_half,Superficial_result)
 |Sr_lower_half -> Some(Breadth_n_size_to_lower_half,Superficial_result)
 |Sl_upper_half -> Some(Breadth_n_size_to_upper_half,Solution_list)
 |Sl_lower_half -> Some(Breadth_n_size_to_lower_half,Solution_list)
 |Qpll_upper_half -> Some(Breadth_n_size_to_upper_half,Qpl_length) 
 |Qpll_lower_half -> Some(Breadth_n_size_to_lower_half,Qpl_length) 
 |Qpli_upper_half  -> Some(Breadth_n_size_to_upper_half,Qpl_interval) 
 |Qpli_lower_half  -> Some(Breadth_n_size_to_upper_half,Qpl_interval)  
 |Sr_upper_half_atomized -> Some(Breadth_n_size_to_upper_half,Qpl_interval)  
 |Sr_lower_half_atomized 
 |Sl_upper_half_atomized 
 |Sl_lower_half_atomized 
 |Qpll_upper_half_atomized  
 |Qpll_lower_half_atomized 
 |Qpli_upper_half_atomized  
 |Qpli_lower_half_atomized ;; 

;; 

type downwards_division = Sz3p_types.downwards_division = 
    Bulk_result_to_superficial_result
  | Bulk_result_to_solution_list
  | Bulk_result_to_qualified_point_list    
  | List_to_length 
  | List_to_range of (int * int)
  | Breadth_n_size_to_upper_half
  | Breadth_n_size_to_lower_half ;;  


module Downwards_division = struct 

let canonical_decomposition_opt sf = match sf with 
  Whole -> None 
 |Superficial_result -> Some(Bulk_result_to_superficial_result,Whole) 
 |Solution_list -> Some(Bulk_result_to_solution_list,Whole)
 |Qualified_point_list -> Some(Bulk_result_to_qualified_point_list,Whole)
 |Qpl_length -> Some(List_to_length,Qualified_point_list)
 |Qpl_interval (i,j) -> Some(List_to_range(i,j),Qualified_point_list)
 |Sr_upper_half -> Some(Breadth_n_size_to_upper_half,Superficial_result)
 |Sr_lower_half -> Some(Breadth_n_size_to_lower_half,Superficial_result)
 |Sl_upper_half -> Some(Breadth_n_size_to_upper_half,Solution_list)
 |Sl_lower_half -> Some(Breadth_n_size_to_lower_half,Solution_list)
 |Qpll_upper_half -> Some(Breadth_n_size_to_upper_half,Qpl_length) 
 |Qpll_lower_half -> Some(Breadth_n_size_to_lower_half,Qpl_length) 
 |Qpli_upper_half (i,j) -> Some(Breadth_n_size_to_upper_half,Qpl_interval (i,j)) 
 |Qpli_lower_half (i,j) -> Some(Breadth_n_size_to_upper_half,Qpl_interval (i,j))  ;; 

exception Apply_downwards_division_exn of downwards_division * subfunction_without_width_and_scrappers ;;


let apply_dd_bulk_result_to_superficial_result sf = match sf with 
   Whole -> Superficial_result
  |Superficial_result 
  |Solution_list 
  |Qualified_point_list 
  |Qpl_length
  |Qpl_interval (_,_)
  |Sr_upper_half 
  |Sr_lower_half 
  |Sl_upper_half 
  |Sl_lower_half 
  |Qpll_upper_half  
  |Qpll_lower_half 
  |Qpli_upper_half (_,_) 
  |Qpli_lower_half (_,_) -> raise(Apply_downwards_division_exn(Bulk_result_to_superficial_result,sf))  ;; 

let apply_dd_bulk_result_to_solution_list sf = match sf with 
  Whole -> Solution_list
 |Superficial_result 
 |Solution_list 
 |Qualified_point_list 
 |Qpl_length
 |Qpl_interval (_,_)
 |Sr_upper_half 
 |Sr_lower_half 
 |Sl_upper_half 
 |Sl_lower_half 
 |Qpll_upper_half  
 |Qpll_lower_half 
 |Qpli_upper_half (_,_) 
 |Qpli_lower_half (_,_) -> raise(Apply_downwards_division_exn(Bulk_result_to_solution_list,sf))  ;; 

let apply_dd_bulk_result_to_qualified_point_list sf = match sf with 
 Whole -> Qualified_point_list
|Superficial_result 
|Solution_list 
|Qualified_point_list 
|Qpl_length
|Qpl_interval (_,_)
|Sr_upper_half 
|Sr_lower_half 
|Sl_upper_half 
|Sl_lower_half 
|Qpll_upper_half  
|Qpll_lower_half 
|Qpli_upper_half (_,_) 
|Qpli_lower_half (_,_) -> raise(Apply_downwards_division_exn(Bulk_result_to_qualified_point_list,sf))  ;; 

let apply_dd_list_to_length sf = match sf with 
  Whole 
 |Superficial_result 
 |Solution_list 
 |Qualified_point_list 
 |Qpl_length
 |Qpl_interval (_,_)
 |Sr_upper_half 
 |Sr_lower_half 
 |Sl_upper_half 
 |Sl_lower_half 
 |Qpll_upper_half  
 |Qpll_lower_half 
 |Qpli_upper_half (_,_) 
 |Qpli_lower_half (_,_) -> raise(Apply_downwards_division_exn(Bulk_result_to_superficial_result,sf))  ;; 


end ;;  




type upwards_division = Sz3p_types.upwards_division = 
     Bulk_result_by_definition 
   | List_by_rangeset of (int * int) list
   | Breadth_n_size_by_two ;;


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