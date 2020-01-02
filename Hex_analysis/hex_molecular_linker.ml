(* 

#use"Hex_analysis/hex_molecular_linker.ml";;

*)


exception Nondisjoint_atoms of Hex_atomic_linker_t.t * Hex_atomic_linker_t.t;;

module Private = struct

let tr = ((fun x->Hex_molecular_linker_t.M(x)),
          (fun (Hex_molecular_linker_t.M(x))->x),Hex_atomic_linker.cmp);;


end;;

let constructor unordered_l= 
  let l= Ordered.sort Hex_atomic_linker.cmp unordered_l in 
  let temp1=Image.image (fun atl->(atl,Hex_atomic_linker.support atl)) l in 
  let temp2=Uple.list_of_pairs temp1 in 
  match Option.seek(fun ((atl1,supp1),(atl2,supp2))->
    not(Hex_cell_set.does_not_intersect supp1 supp2)
  ) temp2 with 
  None -> Hex_molecular_linker_t.M(l)
  |Some((atl1,supp1),(atl2,supp2))->raise(Nondisjoint_atoms(atl1,atl2));;

let fold_intersect = Functor_for_sets.fold_intersect Private.tr ;;



let is_included_in = Functor_for_sets.is_included_in Private.tr ;;

let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_molecular_linker_t.M(
      Concrete_object_field.to_list Hex_atomic_linker.of_concrete_object arg1
   );;

let setminus = Functor_for_sets.setminus Private.tr ;;

let support (Hex_molecular_linker_t.M(l))= 
   Hex_cell_set.fold_merge (Image.image Hex_atomic_linker.support l);;


let to_concrete_object (Hex_molecular_linker_t.M(l))=
   Concrete_object_t.Variant("Hex_"^"molecular_linker_t.M",
     [Concrete_object_field.of_list Hex_atomic_linker.to_concrete_object l]);;

let to_readable_string (Hex_molecular_linker_t.M(l))=
   String.concat "|" (Image.image Hex_atomic_linker.to_readable_string l);;