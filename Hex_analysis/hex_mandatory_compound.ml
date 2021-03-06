(* 

#use"Hex_analysis/hex_mandatory_set.ml";;

*)

exception Escape_set of Hex_cell_set_t.t ;;
exception Missing_constraint ;;

let escape_compound_in_disjunction cells older_extmols = 
   let n =List.length(cells) in 
   if n=0
   then Hex_mandatory_compound_t.No_constraint
   else 
   if n=1
   then let cell=List.hd cells 
        and extmol = List.hd older_extmols in 
        Hex_mandatory_compound_t.Constraint(
          Hex_molecular_linker_t.M[],
          Hex_cell_set.insert cell (Hex_extended_molecular.passive_part extmol)
       )
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
     and pre_second_choice = (
        if mlclr = Hex_molecular_linker_t.M[] 
        then ""
        else "Molecular component : "^(Hex_molecular_linker.to_readable_string mlclr))  in
     let (joiner,second_choice) = (
        if (first_choice<>"")&&(pre_second_choice<>"") 
        then (", or in the ",String.uncapitalize_ascii pre_second_choice)
        else ("",pre_second_choice)
     ) in
     first_choice^joiner^second_choice;;


let assert_exhaustibility = function 
   Hex_mandatory_compound_t.No_constraint -> raise(Missing_constraint) 
  |Constraint(_,escape_set) -> if Hex_cell_set.length escape_set = 0
                               then ()
                               else raise(Escape_set(escape_set));;

let global_escape_set = function 
   Hex_mandatory_compound_t.No_constraint -> None 
  |Constraint(mlclr,small_escape_set) -> 
    Some( Hex_cell_set.merge small_escape_set
        (Hex_molecular_linker.support mlclr)
    );;

let test_for_unrealizable_constraint = function 
   Hex_mandatory_compound_t.No_constraint -> false
  |Constraint(_,condition) -> (condition = (Hex_cell_set_t.S[])) ;;
