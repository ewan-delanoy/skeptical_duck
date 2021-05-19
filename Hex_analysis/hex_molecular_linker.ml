(* 

#use"Hex_analysis/hex_molecular_linker.ml";;

*)


exception Nondisjoint_atoms of Hex_atomic_linker_t.t * Hex_atomic_linker_t.t;;

module Private = struct

let tr = ((fun x->Hex_molecular_linker_t.M(x)),
          (fun (Hex_molecular_linker_t.M(x))->x),Hex_atomic_linker.cmp);;

let to_readable_string (Hex_molecular_linker_t.M(l))=
   String.concat "|" (Image.image Hex_atomic_linker.to_readable_string l);;


end;;

let active_complement (Hex_molecular_linker_t.M(l))= 
   Hex_cell_set.fold_merge (Image.image Hex_atomic_linker.active_complement l);;


let constructor unordered_l= 
  let l= Ordered.sort Hex_atomic_linker.cmp unordered_l in 
  let temp1=Image.image (fun atl->(atl,Hex_atomic_linker.support atl)) l in 
  let temp2=Uple.list_of_pairs temp1 in 
  match Option.seek(fun ((atl1,supp1),(atl2,supp2))->
    not(Hex_cell_set.does_not_intersect supp1 supp2)
  ) temp2 with 
  None -> Hex_molecular_linker_t.M(l)
  |Some((atl1,supp1),(atl2,supp2))->raise(Nondisjoint_atoms(atl1,atl2));;

let filter f (Hex_molecular_linker_t.M l)= 
  Hex_molecular_linker_t.M(List.filter f l);;

let fold_intersect = Functor_for_sets.fold_intersect Private.tr ;;

let fold_merge = Functor_for_sets.fold_merge Private.tr ;;


let insert = Functor_for_sets.insert Private.tr ;;

let is_included_in = Functor_for_sets.is_included_in Private.tr ;;

let mem = Functor_for_sets.mem Private.tr ;;


let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_automatic.unwrap_bounded_variant crobj in 
   Hex_molecular_linker_t.M(
      Crobj_converter_combinator.to_list Hex_atomic_linker.of_concrete_object arg1
   );;

let pair cell1 cell2 = constructor [Hex_atomic_linker.pair (cell1,cell2)];;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string ap);;    

let setminus = Functor_for_sets.setminus Private.tr ;;

let support (Hex_molecular_linker_t.M(l))= 
   Hex_cell_set.fold_merge (Image.image Hex_atomic_linker.support l);;

let test_for_passive_to_active_conversion cell (Hex_molecular_linker_t.M(l))= 
   match Option.find_and_stop (Hex_atomic_linker.test_for_passive_to_active_conversion cell) l with 
   None -> None 
   |Some(other_cell,atm0) -> 
        let new_l = List.filter (fun atm -> atm <> atm0 ) l in 
        Some (other_cell,Hex_molecular_linker_t.M(new_l));;


let to_concrete_object (Hex_molecular_linker_t.M(l))=
   Concrete_object_t.Variant("Hex_"^"molecular_linker_t.M",
     [Crobj_converter_combinator.of_list Hex_atomic_linker.to_concrete_object l]);;

let to_readable_string = Private.to_readable_string;; 

let use_ally_move_to_simplify_one cell (Hex_molecular_linker_t.M(l))=
   let simplified_l=List.filter (Hex_atomic_linker.nonfulfilment_by_ally_move cell) l in 
   Hex_molecular_linker_t.M(simplified_l);;  

