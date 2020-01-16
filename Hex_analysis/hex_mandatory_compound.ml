(* 

#use"Hex_analysis/hex_mandatory_set.ml";;

*)

let escape_compound_in_disjunction cells older_extmols = 
   if cells=[] 
   then Hex_mandatory_compound_t.No_constraint
   else 
   let common_molecular = Hex_extended_molecular.common_molecular_part older_extmols in 
   let common_passive = Hex_molecular_linker.support common_molecular in 
   let temp1 = List.combine cells older_extmols in 
   let local_escape_sets = Image.image (fun (cell,extmol)-> 
      Hex_cell_set.insert cell (Hex_extended_molecular.passive_part extmol)
   ) temp1 in 
   let global_escape_set = Hex_cell_set.fold_intersect local_escape_sets in 
   Hex_mandatory_compound_t.Constraint(
      common_molecular,
      Hex_cell_set.setminus global_escape_set common_passive
      );;

let explain = function 
   Hex_mandatory_compound_t.No_constraint -> ""
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



let of_extended_molecular_with_condition extmol condition = 
   if Hex_cell_set.length(Hex_extended_molecular.passive_part extmol)=0 
   then Hex_mandatory_compound_t.No_constraint
   else 
   Hex_mandatory_compound_t.Constraint(
      extmol.Hex_extended_molecular_t.molecular_part,
      Hex_cell_set.apply_condition condition (extmol.Hex_extended_molecular_t.nonmolecular_passive_part)
      );;




let test_for_no_constraint = function 
   Hex_mandatory_compound_t.No_constraint -> true 
  |Constraint(_,_) -> false ;;

let test_for_unrealizable_constraint = function 
   Hex_mandatory_compound_t.No_constraint -> false
  |Constraint(_,condition) -> (condition = (Hex_cell_set_t.S[])) ;;
