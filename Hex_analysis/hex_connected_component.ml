(* 

#use"Hex_analysis/hex_connected_component.ml";;

*)

module Private = struct 

let rec helper dim context (already_treated,to_be_treated) =
   if Hex_cell_set.length to_be_treated = 0 
   then already_treated 
   else 
   let candidates = 
     Hex_cell_set.intersect context (Hex_cell_set.neighbors dim to_be_treated)  in 
   let new_ones = Hex_cell_set.setminus candidates already_treated in 
   let new_whole = Hex_cell_set.merge new_ones already_treated in 
   helper dim context (new_whole,new_ones);;  

end ;;

let cc_in dim context subset=  Private.helper dim context (subset,Hex_cell_set_t.S []) ;;

