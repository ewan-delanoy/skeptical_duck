(* 

#use"Hex_analysis/hex_possibly_final_kite_element.ml";;

*)

let to_molecular_linker = function 
    Hex_possibly_final_kite_element_t.Final (nc) ->Hex_named_connector.to_molecular_linker nc 
   |Hex_possibly_final_kite_element_t.Nonfinal (nc) ->Hex_named_connector.to_molecular_linker nc  ;;

let wet_earth = function 
    Hex_possibly_final_kite_element_t.Final (nc) ->Hex_named_connector.wet_earth nc 
   |Hex_possibly_final_kite_element_t.Nonfinal (nc) ->Hex_named_connector.wet_earth nc  ;;


