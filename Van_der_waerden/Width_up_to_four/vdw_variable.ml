(*

#use"Van_der_Waerden/Width_up_to_four/vdw_variable.ml";;

*)


module Private = struct 
let main_ref = ref ([]: (string * (int list list)) list) ;;
end ;;

let get x = List.assoc x (!Private.main_ref) ;;

(*
let homogeneous_translatuon x translation =
    let x_content = get x in 
*)    

let set x y =
      match List.assoc_opt x (!Private.main_ref) with 
      None -> (Private.main_ref := (x,y) :: (!Private.main_ref))
      |Some old_y ->
           if old_y <> y 
           then let msg = "Warning : "^x^" has already been set.\n Resetting ...\n" in 
               let new_list = Image.image (
                  fun p -> let (x1,y1) = p in  if x1 = x then (x,y) else p 
               ) (!Private.main_ref) in 
               (
                  print_string msg;
                  flush stdout;
                  Private.main_ref := new_list ;
                );;    