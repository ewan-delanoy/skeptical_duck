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
     scrappers : int list ;
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
(Sr_lower_half,Some(Breadth_n_size_to_lower_half_k,Superficial_result));
(Sl_upper_half,Some(Breadth_n_size_to_upper_half_k,Solution_list));
(Sl_lower_half,Some(Breadth_n_size_to_lower_half_k,Solution_list));
(Qpll_upper_half,Some(Breadth_n_size_to_upper_half_k,Qpl_length));
(Qpll_lower_half,Some(Breadth_n_size_to_lower_half_k,Qpl_length));
(Qpli_upper_half,Some(Breadth_n_size_to_upper_half_k,Qpl_interval));
(Qpli_lower_half,Some(Breadth_n_size_to_lower_half_k,Qpl_interval));
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
      scrappers : int list;
    }
    val canonical_decomposition_opt : t -> (division * t) option
    
    val divide_by : t -> division -> t
    val root_node : int * int  list -> t
  end ;;

module Node = (Node_Internals:NODE_SIGNATURE) ;; 


let halves x = Image.image 
(fun div->Node.divide_by x div)
  [Breadth_n_size_to_upper_half;Breadth_n_size_to_lower_half] ;;

let several_halves l =
   List.flatten (Image.image halves l) ;;

let example1 = Node.root_node (1,[]) ;; 

let example2 = Node.divide_by example1 Bulk_result_to_superficial_result ;; 
let example3 = Node.divide_by example1 Bulk_result_to_solution_list ;; 
let example4 = Node.divide_by example1 Bulk_result_to_qualified_point_list ;; 
let example5 = Node.divide_by example4 List_to_length ;; 
let examples6 = Int_range.scale (fun k->
  Node.divide_by example4 (List_to_range(k,k))
  ) 1 3 ;;
let examples7 = several_halves ([example5]@examples6) ;; 
let examples8 = Image.image (
   fun nd->Node.divide_by  nd (Atomize(1))
) examples7 ;;

(*
   
we start with the following task : 

1) compute f(n,scr)=main(1,[],n,scr)

We have two choices : we can either decompose the range of f or the image of f. 
By convention, the image has priority over the range set. 

So f(n,scr) is BR(f1(n,scr),M(f2(n,scr),f3(n,scr))) 

Tasks : 

compute f1 -> rf1 
compute f2 -> rf2 
compute f3 -> rf3 
join f1,f2,f3 in f







*)