(* 

#use"Hex_analysis/hex_inner_connector_name.ml";;

*)

let to_readable_string = function 
    Hex_inner_connector_name_t.Bridge(us)-> Hex_unit_side.to_readable_string us ;;