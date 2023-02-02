(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_head_position_for_szemeredi_problem.ml" ;;

*)

type homemade_node_t = {
   homemade_node_name : string ; 
   out_data_type : string ;
} ;;

let code_for_homemade_node (nd:homemade_node_t) idx= 
  let s_idx = string_of_int idx in  
  let full_name = "homemade_part_"^s_idx^"_"^(nd.homemade_node_name) in 
  String.concat "\n"
  [
  "let for_"^full_name^" =  ";
  "  ((Hashtbl.create 50) : (int * int list,"; 
  " breadth -> size -> "^(nd.out_data_type)^" ) Hashtbl.t) ;;";  
  "\n";
  "let get_"^full_name^" pt = ";
  "  let (width,scrappers,breadth,n) = Point.unveil pt in ";
  "  Hashtbl.find for_homemade_part_2 (width,scrappers) breadth n ;; "
  ];;  

type delegated_node_t = {
    delegated_node_name : string ; 
    out_data_type : string ;
} ;;
 
 let code_for_delegated_node (nd:delegated_node_t) idx= 
   let s_idx = string_of_int idx in  
   let full_name = "delegated_part_"^s_idx^"_"^(nd.delegated_node_name) in 
   String.concat "\n"
   [
    "let for_"^full_name^" =  ((ref []):(int * int list) list ref) ;;  
  
    let try_precomputed_results pt =
       let (width,scrappers,_breadth,_n) = Point.unveil pt in 
       if List.mem (width,scrappers) (!for_delegated_whole) 
       then Some(BR(get_part1 pt,M(get_part2 pt,get_part3 pt)))
       else None ;;   "
   ];;  
