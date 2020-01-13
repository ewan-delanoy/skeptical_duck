(* 

#use"Hex_analysis/hex_mandatory_set.ml";;

*)

let explain = function 
   Hex_mandatory_set_t.No_constraint -> ""
  |Constraint(mlclr,remaining_ones) -> 
     let first_choice = (
         if Hex_cell_set.length remaining_ones = 0
         then ""
         else (Hex_cell_set.to_string remaining_ones)
     ) 
     and pre_second_choice = "Molecular component : "^(Hex_molecular_linker.to_readable_string mlclr)  in
     let (joiner,second_choice) = (
        if (first_choice<>"")&&(pre_second_choice<>"") 
        then (", or in the ",String.uncapitalize_ascii pre_second_choice)
        else ("",pre_second_choice)
     ) in
     first_choice^joiner^second_choice;;


let of_extended_molecular extmol = 
   Hex_mandatory_set_t.Constraint(
      extmol.Hex_extended_molecular_t.molecular_part,
      extmol.Hex_extended_molecular_t.nonmolecular_passive_part
      );;

let test_for_no_constraint = function 
   Hex_mandatory_set_t.No_constraint -> true 
  |Constraint(_,_) -> false ;;