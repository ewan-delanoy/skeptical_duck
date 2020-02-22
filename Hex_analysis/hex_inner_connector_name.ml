(* 

#use"Hex_analysis/hex_inner_connector_name.ml";;

*)

let all =
    (Image.image (
      fun us -> Hex_inner_connector_name_t.Bridge(us)
    ) Hex_unit_side.all)
    @[Haddock1] ;;

let to_nondefault_molecular_linker = function 
    Hex_inner_connector_name_t.Bridge(_)
    |Haddock1 -> None ;;

let to_readable_string = function 
     Hex_inner_connector_name_t.Bridge(us)-> Hex_unit_side.to_readable_string us 
    |Haddock1 -> "hk1";;