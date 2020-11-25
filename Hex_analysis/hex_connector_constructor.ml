(* 

#use"Hex_analysis/hex_connector_constructor.ml";;

*)

exception Bad_haddock1_specification of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;

module Private = struct 

let oppose = Hex_connector.oppose 
and reflect = Hex_connector.reflect 

and arbitrary_dim = Hex_dimension.eleven;;

let down = Hex_cardinal_direction_t.Down and left = Hex_cardinal_direction_t.Left  
and right = Hex_cardinal_direction_t.Right and up = Hex_cardinal_direction_t.Up ;;
let high = up and low =down;;


module Inner = struct 


let expand_name = function 
   Hex_inner_connector_name_t.Typical(tic,side) -> Hex_typical_inner_connector_name.full_constructor tic side ;; 

end ;; 

module Border = struct  
 
let eyed_claw d1 d2  =
    let (apex,ipairs) = Hex_connector_data.default_eyed_claw d1 d2 in 
   {
    Hex_connector_t.entry = Hex_island_t.I(Hex_anchor_t.No_anchor,Set_of_poly_pairs_t.S [apex]);
    junction = ipairs;
    exit = Hex_island_t.I(Hex_anchor_t.Single_anchor(d2),Set_of_poly_pairs.empty_set);
    apex = Some(apex);
} ;;   



let typical_border = Hex_typical_border_connector_name.specify_side ;;
    



let expand_name = function 
   Hex_border_connector_name_t.Eyed_claw(d1,d2) -> (eyed_claw d1 d2)
   |Typical(tbc,d) -> (typical_border tbc d);;

end ;; 

end ;;

let expand_name = function 
   Hex_connector_name_t.Inner(inner)-> Private.Inner.expand_name inner 
   |Border(border) -> Private.Border.expand_name border;;