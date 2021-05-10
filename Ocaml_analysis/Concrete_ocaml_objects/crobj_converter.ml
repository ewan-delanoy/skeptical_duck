(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_converter.ml";;

*)



exception Unwrap_int_exn of Concrete_object_t.t;;
exception Unwrap_string_exn of Concrete_object_t.t;;
   

   
module Pair = struct 

   let bool =((fun bowl->
       if bowl 
       then Concrete_object_t.Variant("True",[]) 
       else Concrete_object_t.Variant("False",[])),
       Concrete_object_field.unwrap_lonely_variant [true,"True";false,"False"]
   );;
   let int =((fun i-> (Concrete_object_t.Int i)),
       (
         fun ccrt_obj ->
         match ccrt_obj with 
         Concrete_object_t.Int(i)->i 
         |_->raise(Unwrap_int_exn(ccrt_obj))
       )
   );;
   let int_list = (
       Concrete_object_field.of_list (fst int) ,
       Concrete_object_field.to_list (snd int) 
   );;
   let int_pair = (
       Concrete_object_field.of_pair (fst int) (fst int),
       Concrete_object_field.to_pair (snd int) (snd int)
   );;
   let int_triple = (
       Concrete_object_field.of_triple (fst int) (fst int) (fst int),
       Concrete_object_field.to_triple (snd int) (snd int) (snd int)
   );;
   let string =(
       (fun s -> Concrete_object_t.String(Encoded_string.encode s)),
       (fun ccrt_obj ->
       match ccrt_obj with 
       Concrete_object_t.String(encoded_s)->Encoded_string.decode encoded_s 
       |_->raise(Unwrap_string_exn(ccrt_obj)))
   );;
   let string_list = (
       Concrete_object_field.of_list (fst string) ,
       Concrete_object_field.to_list (snd string) 
   );;
   let string_list_list = (
       Concrete_object_field.of_list (fst string_list) ,
       Concrete_object_field.to_list (snd string_list) 
   );;
   let string_pair = (
       Concrete_object_field.of_pair (fst string) (fst string),
       Concrete_object_field.to_pair (snd string) (snd string)
   );;
   let string_pair_list = (
       Concrete_object_field.of_list (fst string_pair) ,
       Concrete_object_field.to_list (snd string_pair) 
   );;
   let string_triple = (
       Concrete_object_field.of_triple (fst string) (fst string) (fst string),
       Concrete_object_field.to_triple (snd string) (snd string) (snd string)
   );;
   let string_triple_list = (
       Concrete_object_field.of_list (fst string_triple) ,
       Concrete_object_field.to_list (snd string_triple) 
   );;
   
end ;;


module Of = struct 

   let bool = fst(Pair.bool) ;;
   let int = fst(Pair.int);;
   let int_list = fst(Pair.int_list);; 
   let int_pair = fst(Pair.int_pair);; 
   let int_triple = fst(Pair.int_triple);; 
   let string = fst(Pair.string);;
   let string_list = fst(Pair.string_list);; 
   let string_list_list = fst(Pair.string_list_list);; 
   let string_pair = fst(Pair.string_pair);;
   let string_pair_list = fst(Pair.string_pair_list);;
   let string_triple = fst(Pair.string_triple);; 
   let string_triple_list = fst(Pair.string_triple_list);;

end ;;

module To = struct 

   let bool = snd(Pair.bool) ;;
   let int = snd(Pair.int);;
   let int_list = snd(Pair.int_list);; 
   let int_pair = snd(Pair.int_pair);; 
   let int_triple = snd(Pair.int_triple);; 
   let string = snd(Pair.string);;
   let string_list = snd(Pair.string_list);; 
   let string_list_list = snd(Pair.string_list_list);; 
   let string_pair = snd(Pair.string_pair);;
   let string_pair_list = snd(Pair.string_pair_list);;
   let string_triple = snd(Pair.string_triple);; 
   let string_triple_list = snd(Pair.string_triple_list);;

end ;;

