(* 

#use"Hex_analysis/hex_connector_name.ml";;

*)

module Private = struct 

let siders side =
    Image.image (
        fun border -> 
           Hex_connector_name_t.Border(border)
    ) (Hex_border_connector_name.for_side side);;

end ;;

let enders_for_side side = 
   Private.siders  (Hex_cardinal_direction.oppose side);;

let is_inner = function
   Hex_connector_name_t.Inner(_)-> true
  |Border(_) -> false ;;

let middlers = Image.image (
  fun inner -> Hex_connector_name_t.Inner(inner)
) Hex_inner_connector_name.all;;

let opt_side = function
    Hex_connector_name_t.Inner(inner)-> None
   |Border(border) -> Some(Hex_border_connector_name.side border);;


let starters_for_side = Private.siders ;;
   
let to_nondefault_molecular_linker cname apex junction= match cname with 
    Hex_connector_name_t.Inner(inner)-> Hex_inner_connector_name.to_nondefault_molecular_linker inner
   |Border(border) -> Hex_border_connector_name.to_nondefault_molecular_linker border apex junction;;

let to_readable_string = function 
    Hex_connector_name_t.Inner(inner)-> Hex_inner_connector_name.to_readable_string inner
   |Border(border) -> Hex_border_connector_name.to_readable_string border;;

