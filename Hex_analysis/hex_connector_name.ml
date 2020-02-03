(* 

#use"Hex_analysis/hex_connector_name.ml";;

*)

let is_inner = function
    Hex_connector_name_t.Bridge(_)-> true
   |Eyed_claw(_,_,_) 
   |Noneyed_claw(_,_,_) 
   |Pyramid(_,_) 
   |Small_pyramid(_,_) -> false ;;

