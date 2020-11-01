(* 

#use"Hex_analysis/hex_connector_name.ml";;

*)

module Private = struct 

let siders bw side =
    Image.image (
        fun border -> 
           Hex_connector_name_t.Border(bw,border)
    ) (Hex_border_connector_name.for_side side);;

end ;;

let enders_for_side side = 
   Private.siders Hex_borderwise_t.To_border (Hex_cardinal_direction.oppose side);;

let middlers = Image.image (
  fun inner -> Hex_connector_name_t.Inner(inner)
) Hex_inner_connector_name.all;;


let starters_for_side = Private.siders Hex_borderwise_t.From_border;;
   

let is_inner = function
    Hex_connector_name_t.Inner(_)-> true
   |Border(_,_) -> false ;;

let to_nondefault_molecular_linker cname apex junction= match cname with 
    Hex_connector_name_t.Inner(inner)-> Hex_inner_connector_name.to_nondefault_molecular_linker inner
   |Border(_,border) -> Hex_border_connector_name.to_nondefault_molecular_linker border apex junction;;

let to_readable_string = function 
    Hex_connector_name_t.Inner(inner)-> Hex_inner_connector_name.to_readable_string inner
   |Border(_,border) -> Hex_border_connector_name.to_readable_string border;;