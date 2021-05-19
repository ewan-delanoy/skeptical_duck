(* 

#use"Hex_analysis/hex_dimension.ml";;

*)


let eleven = Hex_dimension_t.D 11;;

let of_concrete_object crobj = 
    let (_,(arg1,_,_,_,_,_,_))=Concrete_object_automatic.unwrap_bounded_variant crobj in 
   Hex_dimension_t.D(
      Crobj_converter.int_of_concrete_object arg1
   );;

let to_concrete_object (Hex_dimension_t.D(i))= 
    Concrete_object_t.Variant("Hex_"^"dimension_t.D",
     [Crobj_converter.int_to_concrete_object(i)]);;

