(* 

#use"Hex_analysis/hex_first_alternative_in_springboard.ml";;

*)

module Private = struct 

let we_for_homogeneous_list l =
   Hex_cell_set.fold_merge (Image.image Hex_kite_springless_element.wet_earth l);;

let we_for_list = function 
  [] -> Hex_cell_set.empty_set 
  |joiner ::others ->
    let nc = Hex_kite_springless_element.claim_sea joiner in 
    Hex_cell_set.merge 
      (Hex_named_connector.wet_earth_with_entry_unchecked nc) 
       (we_for_homogeneous_list others) ;; 

end ;;

let active_part_when_joined_to_another_cell  
     (Hex_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv)) cell2=
    let opening_pair = Hex_cell_set.safe_set 
       [cell;cell2] in 
    Hex_cell_set.setminus actv opening_pair ;;

let molecular_linker_when_joined_to_another_cell 
   (Hex_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv)) cell2=
  (* strictly speaking, this is only a partial molecular linker *)
  Hex_molecular_linker.fold_merge (
     (Hex_molecular_linker.pair cell cell2)::
     (Option.filter_and_unpack Hex_kite_springless_element.to_molecular_linker path)
  );;

let to_readable_string  (Hex_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv))=
  let path_description = String.concat "," (Image.image Hex_kite_springless_element.to_readable_string path) in 
  (Hex_cell.to_string cell)^" -> "^path_description ;;

let wet_earth  
  (Hex_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv))=    
     Hex_cell_set.insert cell (Private.we_for_list path) ;; 
