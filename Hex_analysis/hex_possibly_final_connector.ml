(* 

#use"Hex_analysis/hex_possibly_final_kite_element.ml";;

*)

module Private = struct 

let content = function 
    Hex_possibly_final_connector_t.Final (nc) ->nc 
   |Hex_possibly_final_connector_t.Nonfinal (nc) ->nc  ;;

end ;;

let extra_active_part pfc = Hex_named_connector.extra_active_cells (Private.content pfc) ;;


let is_final = function 
    Hex_possibly_final_connector_t.Final (_) ->true 
   |Hex_possibly_final_connector_t.Nonfinal (_) ->false  ;;

let to_molecular_linker pfc = Hex_named_connector.to_molecular_linker (Private.content pfc) ;;

let wet_earth  pfc = Hex_named_connector.wet_earth (Private.content pfc) ;;
