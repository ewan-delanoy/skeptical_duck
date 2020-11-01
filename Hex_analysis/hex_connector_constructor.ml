(* 

#use"Hex_analysis/hex_connector_constructor.ml";;

*)

exception Bad_haddock1_specification of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;

module Private = struct 

let oppose = Hex_connector.oppose 
and reflect = Hex_connector.reflect 
and reverse = Hex_connector.reverse  
and arbitrary_dim = Hex_dimension.eleven;;

let down = Hex_cardinal_direction_t.Down and left = Hex_cardinal_direction_t.Left  
and right = Hex_cardinal_direction_t.Right and up = Hex_cardinal_direction_t.Up ;;
let high = up and low =down;;


module Inner = struct 


let broken_bridge (entry,p1,p2,exit) = {
    Hex_connector_t.entry = entry;
    junction = Image.image Hex_cell.to_int_pair [ p1 ; p2 ];
    exit = exit;
    apex = None ;
} ;;   

let expand_name = function 
   Hex_inner_connector_name_t.Broken_bridge(entry,p1,p2,exit) -> broken_bridge (entry,p1,p2,exit)
   |Typical(tic,side) -> Hex_typical_inner_connector_name.full_constructor tic side ;; 

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
    

let basic_doubling bw y =
  match bw with 
   Hex_borderwise_t.From_border -> reverse y
  |Hex_borderwise_t.To_border -> y ;;


let expand_name bw = function 
   Hex_border_connector_name_t.Eyed_claw(d1,d2) -> basic_doubling bw (eyed_claw d1 d2)
   |Typical(tbc,d) -> basic_doubling bw (typical_border tbc d);;

end ;; 

end ;;

let expand_name = function 
   Hex_connector_name_t.Inner(inner)-> Private.Inner.expand_name inner 
   |Border(bw,border) -> Private.Border.expand_name bw border;;