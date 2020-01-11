(* 

#use"Hex_analysis/hex_extended_molecular.ml";;

*)

module Private = struct  

let salt = "Hex_"^"extended_molecular_t.";;

let molecular_part_label            = salt ^ "molecular_part";;
let nonmolecular_passive_part_label = salt ^ "nonmolecular_passive_part";;
let active_part_label               = salt ^ "active_part";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_extended_molecular_t.molecular_part = Hex_molecular_linker.of_concrete_object (g molecular_part_label);
      nonmolecular_passive_part = Hex_cell_set.of_concrete_object (g nonmolecular_passive_part_label);
      active_part = Hex_cell_set.of_concrete_object (g active_part_label);
   };;

let to_concrete_object extmol =
 
   Concrete_object_t.Record([
     molecular_part_label,Hex_molecular_linker.to_concrete_object(extmol.Hex_extended_molecular_t.molecular_part);
     nonmolecular_passive_part_label, Hex_cell_set.to_concrete_object(extmol.Hex_extended_molecular_t.nonmolecular_passive_part);
     active_part_label, Hex_cell_set.to_concrete_object(extmol.Hex_extended_molecular_t.active_part);
   ]);;

end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;
