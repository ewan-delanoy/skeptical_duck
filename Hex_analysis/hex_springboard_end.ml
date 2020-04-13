(* 

#use"Hex_analysis/hex_springboard_end.ml";;

*)

let alternative_move (Hex_springboard_end_t.End(cell2,pfc))= cell2 ;;

let construct_usual cell2 pfc = (Hex_springboard_end_t.End(cell2,pfc));;

let extra_active_part (Hex_springboard_end_t.End(cell2,pfc))=
    Hex_possibly_final_connector.extra_active_part pfc ;;

let extra_content (Hex_springboard_end_t.End(cell2,pfc))= pfc ;;

let extra_molecular_linker (Hex_springboard_end_t.End(cell2,pfc))=
    Hex_possibly_final_connector.to_molecular_linker pfc ;;

let is_final (Hex_springboard_end_t.End(cell2,pfc))=
   Hex_possibly_final_connector.is_final pfc;;

let wet_earth (Hex_springboard_end_t.End(cell2,pfc)) = 
   Hex_cell_set.insert cell2 (Hex_possibly_final_connector.wet_earth pfc) ;;

let to_readable_string (Hex_springboard_end_t.End(cell2,pfc))=
  (Hex_cell.to_string cell2)^" -> "^
  (Hex_possibly_final_connector.to_readable_string pfc);;

     