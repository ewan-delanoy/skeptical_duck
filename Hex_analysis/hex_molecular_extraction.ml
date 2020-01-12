(* 

#use"Hex_analysis/hex_molecular_extraction.ml";;

*)

module Private = struct 

let extract_from_set mlclr passive_part =
    let remaining_ones =
      Hex_cell_set.setminus passive_part (Hex_molecular_linker.support mlclr) in 
    Hex_molecular_extraction_t.E(mlclr,remaining_ones);;


let first_part_in_readable_string remaining_ones =
   let (Hex_cell_set_t.S l)=remaining_ones in 
   if l=[]
   then ""
   else (Hex_cell_set.to_string remaining_ones)^",";;

let support  (Hex_molecular_extraction_t.E(mlclr,remaining_ones))=
    Hex_cell_set.merge (Hex_molecular_linker.support mlclr) remaining_ones;;


end ;; 

let apply_to_immediate_dangers l=
     let supports=Image.image (fun (x,xtracn)->
       Hex_cell_set.insert x (Private.support xtracn)
     ) l in     
     let mandatory_set = Hex_cell_set.fold_intersect supports in 
     let common_part = Hex_molecular_linker.fold_intersect (Image.image 
     (fun (x,Hex_molecular_extraction_t.E(mlclr,_))->mlclr) l) in 
      Private.extract_from_set common_part mandatory_set ;; 

(*
let extract_from_strategy  mlclr fles =
    Private.extract_from_set mlclr (Hex_flattened_end_strategy_field.passive_part fles);;
*)

let of_concrete_object crobj= 
   let (arg1,arg2,_,_,_,_,_)=Concrete_object_field.unwrap_bounded_uple crobj in 
   Hex_molecular_extraction_t.E
   (
    Hex_molecular_linker.of_concrete_object arg1,
    Hex_cell_set.of_concrete_object(arg2)
   );;

let to_concrete_object (Hex_molecular_extraction_t.E(mlclr,remaining_ones)) =
   Concrete_object_t.Uple [
      Hex_molecular_linker.to_concrete_object mlclr;
      Hex_cell_set.to_concrete_object(remaining_ones)
   ]  ;;

let to_readable_string (Hex_molecular_extraction_t.E(mlclr,remaining_ones))=
  (Private.first_part_in_readable_string remaining_ones)^" common part :"^
    (Hex_molecular_linker.to_readable_string mlclr);;

let trivial_case mlclr = Hex_molecular_extraction_t.E(mlclr,Hex_cell_set_t.S[]);;

