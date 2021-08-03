(*

#use"Filewatching/fw_indexer.ml";;



*)

exception Invalid_instance_index of int ;;

module Private = struct 

let main_ref = ref ([]: Fw_state_index_t.t list) ;;
   


end ;;  

let get_state (Fw_instance_index_t.I ii) =
  try List.nth (!(Private.main_ref)) (ii-1) with 
  _ -> raise (Invalid_instance_index(ii)) ;;

let new_instance () =
  let raf = Private.main_ref in 
  let n = List.length (!raf) 
  and starter = Fw_state_index_t.I 0 in 
  let _ = (raf := (!raf) @ [starter]) in 
  (Fw_instance_index_t.I(n+1),starter) ;;

let new_state instance =
   let raf = Private.main_ref in 
   let (Fw_state_index_t.I old_state) = get_state instance 
   and indexed_old_list = Ennig.index_everything (!raf) in 
   let new_state = (Fw_state_index_t.I (old_state+1)) in 
   let (Fw_instance_index_t.I ii) = instance in 
   let new_list = Image.image 
      (fun (k,st)->if k=ii then new_state else st) indexed_old_list in 
   let _ = (raf := new_list) in 
   new_state ;;
  
   
