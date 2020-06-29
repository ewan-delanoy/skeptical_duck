(* 

#use"Hex_analysis/hex_first_alternative_in_springboard.ml";;

*)

module Private = struct 

let we_for_homogeneous_list l =
   Hex_cell_set.fold_merge (Image.imagination Hex_expsv_kite_springless_element.wet_earth l);;

let we_for_list = function 
  [] -> Hex_cell_set.empty_set 
  |joiner ::others ->
    let nc = Hex_expsv_kite_springless_element.claim_sea joiner in 
    Hex_cell_set.merge 
      (Hex_expsv_named_connector.wet_earth_with_entry_unchecked nc) 
       (we_for_homogeneous_list others) ;; 

let wet_earth  
  (Hex_expsv_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv))=    
     Hex_cell_set.insert cell (we_for_list path) ;; 

end ;;

let active_part_when_joined_to_another_cell  
     (Hex_expsv_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv)) cell2=
    let opening_pair = Hex_cell_set.safe_set 
       [cell;cell2] in 
    Hex_cell_set.setminus actv opening_pair ;;

let check_island_after_fa_insertion fa island = 
   if Hex_island.anchor island <> Hex_anchor_t.No_anchor then true else 
   Hex_cell_set.does_not_intersect 
      (Private.wet_earth fa)
         (Hex_island.inner_earth island);;

let check_sea_after_fa_insertion fa nc = 
   Hex_cell_set.does_not_intersect 
      (Private.wet_earth fa)
         (Hex_expsv_named_connector.wet_earth nc);;      

let molecular_linker_when_joined_to_another_cell 
   (Hex_expsv_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv)) cell2=
  (* strictly speaking, this is only a partial molecular linker *)
  Hex_molecular_linker.fold_merge (
     (Hex_molecular_linker.pair cell cell2)::
     (Option.filter_and_unpack Hex_expsv_kite_springless_element.to_molecular_linker path)
  );;

let requisitioned_territory 
   (Hex_expsv_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv)) =
  (* strictly speaking, this is only a partial molecular linker *)
  Hex_cell_set.insert cell (Hex_molecular_linker.support(Hex_molecular_linker.fold_merge (
     (Option.filter_and_unpack Hex_expsv_kite_springless_element.to_molecular_linker path)
  )));;


let to_readable_string  (Hex_expsv_first_alternative_in_springboard_t.Fa(opt_start,cell,path,sol,actv))=
  let path_description = String.concat "," (Image.imagination Hex_expsv_kite_springless_element.to_readable_string path) in 
  (Hex_cell.to_string cell)^" -> "^path_description ;;

let wet_earth  = Private.wet_earth ;;