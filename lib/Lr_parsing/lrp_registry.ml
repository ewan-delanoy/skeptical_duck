(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_registry.ml";;

*)

open Lrp_types ;;

module Private = struct

let path_order = Total_ordering.silex_compare Total_ordering.silex_for_strings ;;

let path_merge = Ordered.merge path_order ;;
let path_sort = Ordered.sort path_order ;;

end ;;

let add_new_paths_to_lr0_state rgy lr0_state paths_to_be_added =
  let (Rg old_registry_content) = rgy in 
  let (_,old_paths) = List.find (fun pair->fst(pair)=lr0_state) old_registry_content in 
  let new_paths = Private.path_merge old_paths (Private.path_sort paths_to_be_added) in 
  let new_registry_content = Image.image (
      fun pair -> if fst(pair)=lr0_state then (lr0_state,new_paths) else pair
  )old_registry_content in 
 Rg new_registry_content;;

let default = Rg([(St []),[]]) ;; 

let index_of_in lr0_state (Rg l)=
   Option.get(List.find_index  (fun pair->fst(pair)=lr0_state) l)-1 ;;

let register_lr0_state rgy lr0_state = 
  let (Rg old_registry_content) = rgy in 
  let (St items)=lr0_state in 
  match List.find_index  (fun pair->fst(pair)=lr0_state) old_registry_content with 
   Some old_index -> (rgy,RSt(old_index -1,items)) 
   | None -> 
    let new_registry_content = old_registry_content @ [lr0_state,[]] in 
    (Rg new_registry_content,RSt((List.length(new_registry_content)) -2,items));; 
  