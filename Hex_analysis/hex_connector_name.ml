(* 

#use"Hex_analysis/hex_connector_name.ml";;

*)

(*
let base_for_starters =
*)   

let is_inner = function
    Hex_connector_name_t.Inner(_)-> true
   |Border(_,_) -> false ;;

let to_readable_string = function 
    Hex_connector_name_t.Inner(inner)-> Hex_inner_connector_name.to_readable_string inner
   |Border(_,border) -> Hex_border_connector_name.to_readable_string border;;