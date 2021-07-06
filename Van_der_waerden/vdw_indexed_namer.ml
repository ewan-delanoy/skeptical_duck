(*

#use"Van_der_Waerden/vdw_indexed_namer.ml";;

*)

exception Empty_argument_in_register_new_exn ;;

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

let register_new ll=
  if ll=[] 
  then raise(Empty_argument_in_register_new_exn) else
  let x = List.length(List.hd ll) in 
  let y = consume_new x in 
  let z = "z"^(string_of_int x)^"_"^(string_of_int y) in 
  let _ = (Vdw_variable.set z ll) in 
  z ;;

end ;;  

let consume_new = Private.consume_new ;; 
let register_new = Private.register_new ;; 
