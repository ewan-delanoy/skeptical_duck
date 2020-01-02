(* 

#use"Hex_analysis/hex_molecular_extraction.ml";;

*)

module Private = struct 

let first_part_in_readable_string remaining_ones =
   let (Hex_cell_set_t.S l)=remaining_ones in 
   if l=[]
   then ""
   else (Hex_cell_set.to_string remaining_ones)^",";;

   

end ;; 

let extract_from_set mlclr passive_part =
    let remaining_ones =
      Hex_cell_set.setminus passive_part (Hex_molecular_linker.support mlclr) in 
    Hex_molecular_extraction_t.E(mlclr,remaining_ones);;

let extract_from_strategy  mlclr fles =
    extract_from_set mlclr (fles.Hex_flattened_end_strategy_t.passive_part);;

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

