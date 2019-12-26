(* 

#use"Hex_analysis/hex_molecular_linker.ml";;

*)


module Private = struct

let tr = ((fun x->Hex_molecular_linker_t.M(x)),
          (fun (Hex_molecular_linker_t.M(x))->x),Hex_atomic_linker.cmp);;
end;;


let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_molecular_linker_t.M(
      Concrete_object_field.to_list Hex_atomic_linker.of_concrete_object arg1
   );;

let safe_set l= Functor_for_sets.safe_set Private.tr l;;

let support (Hex_molecular_linker_t.M(l))= 
   Hex_cell_set.fold_merge (Image.image Hex_atomic_linker.support l);;

let to_concrete_object (Hex_molecular_linker_t.M(l))=
   Concrete_object_t.Variant("Hex_"^"molecular_linker_t.M",
     [Concrete_object_field.of_list Hex_atomic_linker.to_concrete_object l]);;

