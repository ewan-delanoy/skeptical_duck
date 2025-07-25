(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/crobj_converter_combinator.ml";;

*)


module Private = struct 

       let of_list of_a l= Concrete_object_t.List(Image.image of_a l);;
       let to_list to_a crobj= Image.image to_a (Concrete_object.unwrap_list crobj);;
         


end ;;       

let of_array of_a arr= Concrete_object_t.Array(Array.to_list(Array.map of_a arr));;
let to_array to_a crobj= Array.map to_a (Concrete_object.unwrap_array crobj);;


let of_option of_a opt = match opt with
 None -> Private.of_list of_a []
 |Some x -> Private.of_list of_a [x] ;;

let to_option to_a crobj = match Private.to_list to_a crobj with 
  [] -> None 
  | x :: _ -> Some x ;;

let of_list = Private.of_list ;;
let to_list = Private.to_list ;;
   
   
let of_pair of_a of_b (a,b)=Concrete_object_t.Uple[of_a a;of_b b];;
let to_pair to_a to_b crobj=
       let (arg1,arg2,_,_,_,_,_)=Concrete_object.unwrap_bounded_uple crobj in
       (to_a arg1,to_b arg2);;
   
let of_triple of_a of_b of_c (a,b,c)=Concrete_object_t.Uple[of_a a;of_b b;of_c c];;
let to_triple to_a to_b to_c crobj=
           let (arg1,arg2,arg3,_,_,_,_)=Concrete_object.unwrap_bounded_uple crobj in
           (to_a arg1,to_b arg2,to_c arg3);;
   

let of_pair_list of_a of_b l=of_list (of_pair of_a of_b) l;;
let to_pair_list to_a to_b crobj = to_list (to_pair to_a to_b) crobj;;
   
   
