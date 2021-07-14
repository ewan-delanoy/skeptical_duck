(*

#use"Van_der_Waerden/Fixed_width/vdw_variable.ml";;

*)


module Private = struct 
 

let main_ref = ref ([]: (Vdw_nonempty_index_t.t * (int list list)) list) ;;

end ;;

let get x = List.assoc x (!Private.main_ref) ;;


let homogeneous_translation x translation =
    Vdw_current_max_width.homogeneous_translation 
      (get x) translation ;;
   
let mem y =
   match Option.seek (fun (x1,y1)->y1=y) (!Private.main_ref) with 
   None -> None 
   |Some(x,_) -> Some x ;;


let set x y =
      match List.assoc_opt x (!Private.main_ref) with 
      None -> (Private.main_ref := (x,y) :: (!Private.main_ref))
      |Some old_y ->
           if old_y <> y 
           then let msg = "Warning : "^
                 (Vdw_nonempty_index.to_string x)^
                 " has already been set.\n Resetting ...\n" in 
               let new_list = Image.image (
                  fun p -> let (x1,y1) = p in  if x1 = x then (x,y) else p 
               ) (!Private.main_ref) in 
               (
                  print_string msg;
                  flush stdout;
                  Private.main_ref := new_list ;
                );;    