(* 

#use"Hex_analysis/hex_springboard.ml";;

*)

module Private = struct 

let wet_earth (Hex_expsv_springboard_t.Sp(fa,cell2,new_island)) =
    Hex_expsv_first_alternative_in_springboard.wet_earth fa ;;

end ;;

let active_part (Hex_expsv_springboard_t.Sp(fa,cell2,new_island)) =
   Hex_expsv_first_alternative_in_springboard.active_part_when_joined_to_another_cell
     fa cell2  ;;

let change_island_component 
   (Hex_expsv_springboard_t.Sp(fa,cell2,new_island)) 
      new_component= 
    Hex_expsv_springboard_t.Sp(fa,cell2,new_component) ;;  


let new_island (Hex_expsv_springboard_t.Sp(fa,cell2,new_island)) =
    new_island ;;

let opt_constructor (fa,cell2,new_island) = 
   let w1 = Hex_expsv_first_alternative_in_springboard.wet_earth fa
   and w2 =  Hex_cell_set.safe_set [cell2] in 
   if Hex_cell_set.does_not_intersect w1 w2 
   then Some(Hex_expsv_springboard_t.Sp(fa,cell2,new_island)) 
   else None ;;

let to_molecular_linker (Hex_expsv_springboard_t.Sp(fa,cell2,new_island)) =
   (* strictly speaking, this is only a partial molecular linker *)
   Hex_expsv_first_alternative_in_springboard.molecular_linker_when_joined_to_another_cell
     fa cell2  ;;


let to_readable_string (Hex_expsv_springboard_t.Sp(fa,cell2,new_island))=
  "\194\171 "^(Hex_expsv_first_alternative_in_springboard.to_readable_string fa)^" \199\128 "^
              (Hex_cell.to_string cell2)^" -> "^(Hex_island.to_readable_string new_island)^
  " \194\187" ;;

  