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

let we_for_springboard (Hex_springboard_t.Sp(cell,path,sol1,sol2,cell2,nc2)) =
    we_for_pair (cell,path) ;;

end ;;


let check_island springboard island = 
   Hex_cell_set.does_not_intersect 
      (Private.we_for_springboard springboard)
         (Hex_island.inner_earth island);;

let check_sea springboard nc = 
   Hex_cell_set.does_not_intersect 
      (Private.we_for_springboard springboard)
         (Hex_named_connector.wet_earth nc);;         

let opt_constructor (cell,path,sol1,sol2,cell2,nc2) = 
   let w1 = Private.we_for_pair (cell,path)
   and w2 = Hex_cell_set.insert cell2 (Hex_named_connector.wet_earth nc2) in 
   if Hex_cell_set.does_not_intersect w1 w2 
   then Some(Hex_springboard_t.Sp(cell,path,sol1,sol2,cell2,nc2)) 
   else None ;;

let to_molecular_linker (Hex_springboard_t.Sp(cell,path,sol1,sol2,cell2,nc2))= 
  (* strictly speaking, this is only a partial molecular linker *)
  Hex_molecular_linker.fold_merge (
     (Hex_molecular_linker.pair cell cell2)::
     (Hex_named_connector.to_molecular_linker nc2)::
     (Option.filter_and_unpack Hex_kite_springless_element.to_molecular_linker path)
  );;


  