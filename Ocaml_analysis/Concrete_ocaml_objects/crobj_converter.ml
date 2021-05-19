(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_converter.ml";;

*)



exception Unwrap_int_exn of Concrete_object_t.t;;
exception Unwrap_string_exn of Concrete_object_t.t;;


let bool_of_concrete_object = Concrete_object_field.unwrap_lonely_variant [true,"True";false,"False"];;
let bool_to_concrete_object bowl = 
    if bowl 
    then Concrete_object_t.Variant("True",[]) 
    else Concrete_object_t.Variant("False",[]);;  

let int_of_concrete_object ccrt_obj =
    match ccrt_obj with 
     Concrete_object_t.Int(i)->i 
    |_->raise(Unwrap_int_exn(ccrt_obj)) ;;
let int_to_concrete_object i = Concrete_object_t.Int i ;;       

let string_of_concrete_object ccrt_obj =
       match ccrt_obj with 
       Concrete_object_t.String(encoded_s)->Encoded_string.decode encoded_s 
       |_->raise(Unwrap_string_exn(ccrt_obj)) ;;
let string_to_concrete_object s = Concrete_object_t.String(Encoded_string.encode s) ;;       