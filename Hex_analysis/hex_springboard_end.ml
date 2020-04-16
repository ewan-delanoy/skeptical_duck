(* 

#use"Hex_analysis/hex_springboard_end.ml";;

*)

let alternative_move spre= spre.Hex_springboard_end_t.alternative_move ;;

let construct_usual 
  cell2 final_or_not nc = {
      Hex_springboard_end_t.alternative_move = cell2 ;
      is_final = final_or_not ;
      extra_content = nc;
  };;

let extra_active_part spre=
    Hex_named_connector.extra_active_cells 
      spre.Hex_springboard_end_t.extra_content ;;
    

let extra_content spre=
      spre.Hex_springboard_end_t.extra_content ;;

let extra_molecular_linker spre=
    Hex_named_connector.to_molecular_linker spre.Hex_springboard_end_t.extra_content ;;

let is_final spre=
   spre.Hex_springboard_end_t.is_final;;

let new_island (spre:Hex_springboard_end_t.t) = Hex_island_t.I(None,Set_of_poly_pairs.empty_set) ;;

let wet_earth spre = 
   Hex_cell_set.insert spre.Hex_springboard_end_t.alternative_move
    (Hex_named_connector.wet_earth spre.Hex_springboard_end_t.extra_content) ;;

let to_readable_string spre=
  (Hex_cell.to_string spre.Hex_springboard_end_t.alternative_move)^" -> "^
  (Hex_named_connector.to_readable_string spre.Hex_springboard_end_t.extra_content);;

     