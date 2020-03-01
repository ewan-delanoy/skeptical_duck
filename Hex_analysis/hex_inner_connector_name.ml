(* 

#use"Hex_analysis/hex_inner_connector_name.ml";;

*)

let all =
    (Image.image (
      fun us -> Hex_inner_connector_name_t.Bridge(us)
    ) Hex_unit_side.all)
    @
    (
      Image.image (
        fun (d1,d2) -> Hex_inner_connector_name_t.Haddock1 (d1,d2)
      ) Hex_cardinal_direction.all_orthogonal_pairs
    );;

let to_nondefault_molecular_linker = function 
    Hex_inner_connector_name_t.Bridge(_)
    |Haddock1(_,_) -> None ;;

let to_readable_string = function 
     Hex_inner_connector_name_t.Bridge(us)-> Hex_unit_side.to_readable_string us 
    |Haddock1(_,_) -> "hk1";;