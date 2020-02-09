(* 

#use"Hex_analysis/hex_inner_connector_name.ml";;

*)

let all =
    Image.image (
      fun us -> Hex_inner_connector_name_t.Bridge(us)
    ) Hex_unit_side.all ;;

let to_nondefault_molecular_linker = function 
    Hex_inner_connector_name_t.Bridge(us)-> None ;;

let to_readable_string = function 
    Hex_inner_connector_name_t.Bridge(us)-> Hex_unit_side.to_readable_string us ;;