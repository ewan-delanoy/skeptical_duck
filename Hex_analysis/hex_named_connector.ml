(* 

#use"Hex_analysis/hex_named_connector.ml";;

*)

exception Bad_specification_for_eyed_claw of 
     Hex_cardinal_direction_t.t *  Hex_cardinal_direction_t.t;;

 module Private = struct 


 
let oppose = Hex_connector.oppose 
and reflect = Hex_connector.reflect 
and reverse = Hex_connector.reverse  
and arbitrary_dim = Hex_dimension.eleven;;


let bs_upwards_claw = Hex_connector.Example.bs_upwards_claw ;;    
let left_eyed_upwards_claw = Hex_connector.Example.left_eyed_upwards_claw ;; 
let northeast_bridge = Hex_connector.Example.northeast_bridge ;;  
let northwest_bridge = Hex_connector.Example.northwest_bridge ;;   
let right_eyed_upwards_claw = Hex_connector.Example.right_eyed_upwards_claw ;;   
let upwards_pyramid = Hex_connector.Example.upwards_pyramid ;;  
let upwards_small_pyramid = Hex_connector.Example.upwards_small_pyramid ;;  
let sb_upwards_claw = Hex_connector.Example.sb_upwards_claw ;;   


let low_eyed_leftwards_claw = reflect right_eyed_upwards_claw;;
let left_eyed_downwards_claw dim = oppose dim right_eyed_upwards_claw ;;
let high_eyed_rightwards_claw dim = oppose dim low_eyed_leftwards_claw;;

let high_eyed_leftwards_claw = reflect left_eyed_upwards_claw;;
let right_eyed_downwards_claw dim = oppose dim left_eyed_upwards_claw ;;
let low_eyed_rightwards_claw dim = oppose dim high_eyed_leftwards_claw;;

let sb_leftwards_claw = reflect bs_upwards_claw;;
let sb_downwards_claw dim = oppose dim bs_upwards_claw;;
let bs_rightwards_claw dim = oppose dim sb_leftwards_claw ;;

let bs_leftwards_claw = reflect sb_upwards_claw;;
let bs_downwards_claw dim = oppose dim sb_upwards_claw;;
let sb_rightwards_claw dim = oppose dim bs_leftwards_claw ;;

let leftwards_pyramid = reflect upwards_pyramid;;
let downwards_pyramid dim = oppose dim upwards_pyramid ;;
let rightwards_pyramid dim = oppose dim leftwards_pyramid;;

let leftwards_small_pyramid = reflect upwards_small_pyramid;;
let downwards_small_pyramid dim = oppose dim upwards_small_pyramid ;;
let rightwards_small_pyramid dim = oppose dim leftwards_small_pyramid;;

let southwest_bridge = reverse northeast_bridge ;;
let south_bridge = reflect  northeast_bridge ;;
let north_bridge = reverse south_bridge ;;

let southeast_bridge = reverse northwest_bridge ;;






let down = Hex_cardinal_direction_t.Down and left = Hex_cardinal_direction_t.Left  
and right = Hex_cardinal_direction_t.Right and up = Hex_cardinal_direction_t.Up ;;
let high = up and low =down;;

let high_eyed_claw = function 
     Hex_cardinal_direction_t.Left  -> high_eyed_leftwards_claw 
    |Hex_cardinal_direction_t.Right -> high_eyed_rightwards_claw arbitrary_dim
    |d->raise(Bad_specification_for_eyed_claw(high,d));;  

let left_eyed_claw = function 
     Hex_cardinal_direction_t.Down  -> left_eyed_downwards_claw arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> left_eyed_upwards_claw
    |d->raise(Bad_specification_for_eyed_claw(left,d));;  

let low_eyed_claw = function 
     Hex_cardinal_direction_t.Left  -> low_eyed_leftwards_claw 
    |Hex_cardinal_direction_t.Right -> low_eyed_rightwards_claw arbitrary_dim
    |d->raise(Bad_specification_for_eyed_claw(low,d));;  

let right_eyed_claw = function 
     Hex_cardinal_direction_t.Down  -> right_eyed_downwards_claw arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> right_eyed_upwards_claw
    |d->raise(Bad_specification_for_eyed_claw(right,d));;  

let bs_claw = function 
     Hex_cardinal_direction_t.Down  -> bs_downwards_claw arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> bs_leftwards_claw
    |Hex_cardinal_direction_t.Right -> bs_rightwards_claw arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> bs_upwards_claw;;    



let sb_claw = function 
     Hex_cardinal_direction_t.Down  -> sb_downwards_claw arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> sb_leftwards_claw
    |Hex_cardinal_direction_t.Right -> sb_rightwards_claw arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> sb_upwards_claw;;    



let bridge = function 
    Hex_unit_side_t.North      ->  north_bridge
   |Hex_unit_side_t.North_east ->  northeast_bridge
   |Hex_unit_side_t.North_west ->  northwest_bridge
   |Hex_unit_side_t.South      ->  south_bridge
   |Hex_unit_side_t.South_east ->  southeast_bridge
   |Hex_unit_side_t.South_west ->  southwest_bridge ;;

let eyed_claw (d1,d2) = match d1 with 
     Hex_cardinal_direction_t.Down  -> low_eyed_claw d2
    |Hex_cardinal_direction_t.Left  -> left_eyed_claw d2
    |Hex_cardinal_direction_t.Right -> right_eyed_claw d2
    |Hex_cardinal_direction_t.Up    -> high_eyed_claw d2;;

let noneyed_claw (dh,d) = match dh with 
      Hex_double_hump_qualifier_t.Big_followed_by_small -> bs_claw d 
     |Hex_double_hump_qualifier_t.Small_followed_by_big -> sb_claw d ;;

let pyramid = function 
     Hex_cardinal_direction_t.Down  -> downwards_pyramid arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_pyramid 
    |Hex_cardinal_direction_t.Right -> rightwards_pyramid arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_pyramid;;      

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

let expand_border_name bw = function 
   Hex_border_connector_name_t.Eyed_claw(d1,d2) -> standard_doubling eyed_claw bw (d1,d2)
   |Noneyed_claw(dh,d) ->standard_doubling noneyed_claw bw (dh,d)
   |Pyramid(d) -> standard_doubling pyramid bw d
   |Small_pyramid(d) -> standard_doubling small_pyramid bw d ;;   

let expand_name = function 
   Hex_connector_name_t.Inner(inner)-> expand_inner_name inner 
   |Border(bw,border) -> expand_border_name bw border;;
   

let of_name nm = 
   let temp = expand_name nm in 
   {
     Hex_named_connector_t.name     = nm ;
     entry    = temp.Hex_connector_t.entry ;
     junction = temp.Hex_connector_t.junction ;
     exit     = temp.Hex_connector_t.exit
   };;   


let to_readable_string nc = Hex_connector_name.to_readable_string nc.Hex_named_connector_t.name;;



end ;;

let of_name = Private.of_name;;

let print_out (fmt:Format.formatter) nc=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string nc);;     
