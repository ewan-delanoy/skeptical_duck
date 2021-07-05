(*

#use"Van_der_Waerden/vdw_set_namer.ml";;

*)

module Private = struct 

let main_ref = ref ([] : (int * int) list) ;; 

let consume_new x =
  match List.assoc_opt x (!main_ref) with 
   None -> let _=(main_ref:=(x,1)::(!main_ref)) in 
           1
  |Some(y) -> 
      let new_list = Image.image (fun pair->
         if fst(pair)=x then (x,y+1) else pair
        )(!main_ref) in 
        let _=(main_ref:=new_list) in
       y+1 ;;

end ;;  

let consume_new = Private.consume_new ;; 

