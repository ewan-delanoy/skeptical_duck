(* 

#use"Hex_analysis/hex_inner_connector_name.ml";;

*)

let all =
    let base = Cartesian.tproduct 
      Hex_typical_inner_connector_name.all  Hex_cardinal_direction.all [true;false] in 
    (Image.image (
      fun us -> Hex_inner_connector_name_t.Bridge(us)
    ) Hex_unit_side.all)
    @
    (
      Image.image (
        fun (tic,direction,orientation) -> Hex_inner_connector_name_t.Typical (tic,direction,orientation)
      ) base
    );;

(*
    let reverse = function 
     Hex_inner_connector_name_t.Bridge(us) ->
    |Broken_bridge(island1,cell1,cell2,island2) -> 
      Hex_inner_connector_name_t.Broken_bridge(island2,cell1,cell2,island1)
    |Typical(tic,direction,orientation)-> 
        Hex_inner_connector_name_t.Typical(tic,direction,not(orientation)) ;;
*)

let to_nondefault_molecular_linker = function 
    Hex_inner_connector_name_t.Bridge(_)
    |Broken_bridge(_,_,_,_) 
    |Typical(_)-> None ;;

let to_readable_string = function 
     Hex_inner_connector_name_t.Bridge(us)-> Hex_unit_side.to_readable_string us 
    |Broken_bridge(_,cell1,cell2,_) -> 
        (Hex_cell.to_string cell1)^"\126"^(Hex_cell.to_string cell2)
    |Typical(tic,_,_) -> Hex_typical_inner_connector_name.to_readable_string tic   ;;
    