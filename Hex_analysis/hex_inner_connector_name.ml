(* 

#use"Hex_analysis/hex_inner_connector_name.ml";;

*)

let all =
    let base = Cartesian.product 
      Hex_typical_inner_connector_name.all  Hex_cardinal_direction.all  in 
    (
      Image.image (
        fun (tic,direction) -> Hex_inner_connector_name_t.Typical (tic,direction)
      ) base
    );;


let to_nondefault_molecular_linker = fun
     (Hex_inner_connector_name_t.Typical(_,_))-> None ;;

let to_readable_string = fun
     (Hex_inner_connector_name_t.Typical(tic,_)) -> Hex_typical_inner_connector_name.to_readable_string tic ;;
    