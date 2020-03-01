(* 

#use"Hex_analysis/hex_connector_constructor.ml";;

*)

module Private = struct 

let oppose = Hex_connector.oppose 
and reflect = Hex_connector.reflect 
and reverse = Hex_connector.reverse  
and arbitrary_dim = Hex_dimension.eleven;;


 
let northeast_bridge = Hex_connector.Example.northeast_bridge ;;  
let northwest_bridge = Hex_connector.Example.northwest_bridge ;;   
let upwards_small_pyramid = Hex_connector.Example.upwards_small_pyramid ;;  

let leftwards_small_pyramid = reflect upwards_small_pyramid;;
let downwards_small_pyramid dim = oppose dim upwards_small_pyramid ;;
let rightwards_small_pyramid dim = oppose dim leftwards_small_pyramid;;

let southwest_bridge = reverse northeast_bridge ;;
let south_bridge = reflect  northeast_bridge ;;
let north_bridge = reverse south_bridge ;;

let southeast_bridge = reverse northwest_bridge ;;




let bridge = function 
    Hex_unit_side_t.North      ->  north_bridge
   |Hex_unit_side_t.North_east ->  northeast_bridge
   |Hex_unit_side_t.North_west ->  northwest_bridge
   |Hex_unit_side_t.South      ->  south_bridge
   |Hex_unit_side_t.South_east ->  southeast_bridge
   |Hex_unit_side_t.South_west ->  southwest_bridge ;;



let eyed_claw (d1,d2) =
   (* this function is deliberately non-curried because we need it to be a 
    univariate function, see below *) 
    let (apex,ipairs) = Hex_connector_data.default_eyed_claw d1 d2 in 
   {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [apex]);
    junction = ipairs;
    exit = Hex_island_t.I(Some(d2),Set_of_poly_pairs.empty_set);
    apex = Some(apex);
} ;;   

let noneyed_claw (dh,d) = 
    let (apex,ipairs)= Hex_connector_data.default_noneyed_claw (dh,d) in 
  {Hex_connector_t.entry = Hex_island_t.I (None, Set_of_poly_pairs_t.S [apex]);
   junction = ipairs;
   exit = Hex_island_t.I (Some d, Set_of_poly_pairs_t.S []);
   apex = Some(apex);
   };; 

let pyramid d = 
  let (apex,ipairs)= Hex_connector_data.default_pyramid d in 
  {Hex_connector_t.entry = Hex_island_t.I (None, Set_of_poly_pairs_t.S [apex]);
   junction = ipairs;
   exit = Hex_island_t.I (Some d, Set_of_poly_pairs_t.S []);
   apex = Some(apex);
  };; 

  

let small_pyramid = function 
     Hex_cardinal_direction_t.Down  -> downwards_small_pyramid arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_small_pyramid 
    |Hex_cardinal_direction_t.Right -> rightwards_small_pyramid arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_small_pyramid;; 

let standard_doubling f bw x =
  let y = f x in 
  match bw with 
   Hex_borderwise_t.From_border -> reverse y
  |Hex_borderwise_t.To_border -> y ;;

let expand_inner_name = function 
   Hex_inner_connector_name_t.Bridge(us)-> bridge us ;;
   (* |Haddock1(d1,d2) -> haddock1 d1 d2 ;; *)

let expand_border_name bw = function 
   Hex_border_connector_name_t.Eyed_claw(d1,d2) -> standard_doubling eyed_claw bw (d1,d2)
   |Noneyed_claw(dh,d) ->standard_doubling noneyed_claw bw (dh,d)
   |Pyramid(d) -> standard_doubling pyramid bw d
   |Small_pyramid(d) -> standard_doubling small_pyramid bw d ;;   

end ;;

let expand_name = function 
   Hex_connector_name_t.Inner(inner)-> Private.expand_inner_name inner 
   |Border(bw,border) -> Private.expand_border_name bw border;;