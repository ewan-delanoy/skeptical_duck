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


end ;;

let bridge = function 
    Hex_unit_side_t.North      ->  Private.north_bridge
   |Hex_unit_side_t.North_east ->  Private.northeast_bridge
   |Hex_unit_side_t.North_west ->  Private.northwest_bridge
   |Hex_unit_side_t.South      ->  Private.south_bridge
   |Hex_unit_side_t.South_east ->  Private.southeast_bridge
   |Hex_unit_side_t.South_west ->  Private.southwest_bridge ;;