(*

#use"lib/Mad_world/Filewatching/mw_indexer.ml";;



*)

exception Invalid_instance_index of int ;;

module Private = struct 

let main_ref = ref ([]: Mw_state_index_t.t list) ;;
   


end ;;  

let create_new_instance () =
  let raf = Private.main_ref in 
  let n = List.length (!raf) 
  and starter = Mw_state_index_t.I 0 in 
  let _ = (raf := (!raf) @ [starter]) in 
  Mw_instance_index_t.I(n+1) ;;

let get_state (Mw_instance_index_t.I ii) =
  try List.nth (!(Private.main_ref)) (ii-1) with 
  _ -> raise (Invalid_instance_index(ii)) ;;
  
let make_full_instance () = 
   let idx = create_new_instance () in 
   (idx,get_state idx) ;;

let push_state instance =
  let raf = Private.main_ref in 
  let (Mw_state_index_t.I old_state) = get_state instance 
  and indexed_old_list = Int_range.index_everything (!raf) in 
  let new_state = (Mw_state_index_t.I (old_state+1)) in 
  let (Mw_instance_index_t.I ii) = instance in 
  let new_list = Image.image 
       (fun (k,st)->if k=ii then new_state else st) indexed_old_list in 
  let _ = (raf := new_list) in 
  () ;;

   
