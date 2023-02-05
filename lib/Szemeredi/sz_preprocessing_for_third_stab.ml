(*

#use"lib/Szemeredi/sz_preprocessing_for_third_stab.ml";;

*)

type breadth = Sz_types_for_third_stab.breadth = B of int ;;
type size = Sz_types_for_third_stab.size = S of int ;;

type point = 
   Sz_types_for_third_stab.point = 
    Empty_point | P of int * int list * breadth * size ;; 

type dynamized_type_t = unit ;; 

type homemade_node_t = {
   homemade_node_name : string ; 
   out_data_type : string ;
   homemade_slow_computer : point -> dynamized_type_t ; 
} ;;

type delegated_node_t = {
    delegated_node_name : string ; 
    details : string list ;
    delegated_slow_computer : point -> dynamized_type_t ; 
} ;;

type node_t =
    Homemade of homemade_node_t 
   |Delegated of delegated_node_t 
;;

let code_for_homemade_node idx (nd:homemade_node_t) = 
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
  "  Hashtbl.find for_"^full_name^" (width,scrappers) breadth n ;; "
  ];;  


 
 let code_for_delegated_node idx (nd:delegated_node_t)= 
   let s_idx = string_of_int idx in  
   let full_name = "delegated_part_"^s_idx^"_"^(nd.delegated_node_name) in 
   String.concat "\n"
   ([
    "let for_"^full_name^" =  ((ref []):(int * int list) list ref) ;;";  
    "\n";
    "let get_"^full_name^" pt = ";
    " let (width,scrappers,breadth,n) = Point.unveil pt in ";
    " if List.mem (width,scrappers) (!for_"^full_name^"delegated_whole) ";
    "  then Some("
   ]
   @(nd.details)
   @
   [
     ")";
     "else None ;;   "
   ]);;  

let code_for_node idx = function 
  Homemade homemade_nd -> code_for_homemade_node idx homemade_nd
|Delegated delegated_nd -> code_for_delegated_node idx delegated_nd ;; 

let unlistifty_type typename =
    Cull_string.two_sided_cutting ("","list") 
      (Cull_string.trim_spaces_on_the_left typename) ;;
    

let listify_homemade_node (nd:homemade_node_t) list_size= 
   let unlistified_out_type = unlistifty_type nd.out_data_type in 
  (Int_range.scale ( 
    fun k->
      {
        homemade_node_name = (nd.homemade_node_name)^"_li"^(string_of_int k) ; 
        out_data_type = unlistified_out_type ;
        homemade_slow_computer = (fun _ -> ()) ;
      }
  ) 1 list_size) @
  [

  ] ;; 
    

