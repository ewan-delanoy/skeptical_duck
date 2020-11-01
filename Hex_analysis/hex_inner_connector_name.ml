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


let to_nondefault_molecular_linker = function 
     Hex_inner_connector_name_t.Broken_bridge(_,_,_,_) 
    |Typical(_)-> None ;;

let to_readable_string = function 
     Hex_inner_connector_name_t.Broken_bridge(_,cell1,cell2,_) -> 
        (Hex_cell.to_string cell1)^"\126"^(Hex_cell.to_string cell2)
    |Typical(tic,_) -> Hex_typical_inner_connector_name.to_readable_string tic ;;
    