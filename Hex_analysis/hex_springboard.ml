(* 

#use"Hex_analysis/hex_springboard.ml";;

*)

module Private = struct 

let we_for_homogeneous_list l =
   Hex_cell_set.fold_merge (Image.image Hex_kite_springless_element.wet_earth l);;

let we_for_list = function 
  [] -> Hex_cell_set.empty_set 
  |joiner ::others ->
    let nc = Hex_kite_springless_element.claim_named_connector joiner in 
    Hex_cell_set.merge 
      (Hex_named_connector.wet_earth_with_entry_unchecked nc) 
       (we_for_homogeneous_list others) ;; 

let we_for_pair  (cell,path)=    
     Hex_cell_set.insert cell (we_for_list path) ;; 

let we_for_springboard (Hex_springboard_t.Sp(cell,path,sol1,actv_in_sol1,cell2,ke)) =
    we_for_pair (cell,path) ;;

end ;;

(*
let active_part (Hex_springboard_t.Sp(cell,path,sol1,actv_in_sol1,cell2,pfc))=
    let opening_pair = Hex_cell_set.safe_set [cell;cell2] in 

    Hex_cell_set.setminus actv_in_sol1 opening_pair ;;
*)

let check_island springboard island = 
   Hex_cell_set.does_not_intersect 
      (Private.we_for_springboard springboard)
         (Hex_island.inner_earth island);;

let check_sea springboard nc = 
   Hex_cell_set.does_not_intersect 
      (Private.we_for_springboard springboard)
         (Hex_named_connector.wet_earth nc);;         

let is_final (Hex_springboard_t.Sp(cell,path,sol1,actv_in_sol1,cell2,pfc))= 
   Hex_possibly_final_connector.is_final pfc;;

let opt_constructor (cell,path,sol1,actv_in_sol1,cell2,ke) = 
   let w1 = Private.we_for_pair (cell,path)
   and w2 = Hex_cell_set.insert cell2 (Hex_possibly_final_connector.wet_earth ke) in 
   if Hex_cell_set.does_not_intersect w1 w2 
   then Some(Hex_springboard_t.Sp(cell,path,sol1,actv_in_sol1,cell2,ke)) 
   else None ;;

let to_molecular_linker (Hex_springboard_t.Sp(cell,path,sol1,actv_in_sol1,cell2,pfc))= 
  (* strictly speaking, this is only a partial molecular linker *)
  Hex_molecular_linker.fold_merge (
     (Hex_molecular_linker.pair cell cell2)::
     (Hex_possibly_final_connector.to_molecular_linker pfc)::
     (Option.filter_and_unpack Hex_kite_springless_element.to_molecular_linker path)
  );;

(*
let to_readable_string (Hex_springboard_t.Sp(cell,path,sol1,actv_in_sol1,cell2,pfc))=
  "S"^(Hex_cell.to_string cell)^" -> "^(Hex_cell.to_string cell2)^" -> "^"S" ;;
*)
  