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

let northeast_bridge = Hex_connector.Example.northeast_bridge ;;  
let northwest_bridge = Hex_connector.Example.northwest_bridge ;;  
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

let ul_haddock1 = Hex_connector.Example.upwards_left_situated_haddock1;;

let left_situated_haddock1  = function 
     Hex_cardinal_direction_t.Down  -> reverse ul_haddock1
    |Hex_cardinal_direction_t.Up -> ul_haddock1
    |bad_direction   -> raise(Bad_haddock1_specification(bad_direction,left));; 

let uptown_haddock1 d = 
    reflect (left_situated_haddock1 (Hex_cardinal_direction.reflect d));;

let downtown_haddock1 dim d = 
    oppose dim (uptown_haddock1 (Hex_cardinal_direction.oppose d));;

let right_situated_haddock1 dim d= 
  reflect (downtown_haddock1 dim (Hex_cardinal_direction.reflect d));;

let haddock1 dim location = match location with 
     Hex_cardinal_direction_t.Down  -> downtown_haddock1 dim
    |Hex_cardinal_direction_t.Left  -> left_situated_haddock1
    |Hex_cardinal_direction_t.Right -> right_situated_haddock1 dim
    |Hex_cardinal_direction_t.Up    -> uptown_haddock1 ;; 

let broken_bridge (entry,p1,p2,exit) = {
    Hex_connector_t.entry = entry;
    junction = Image.image Hex_cell.to_int_pair [ p1 ; p2 ];
    exit = exit;
    apex = None ;
    extra_active_cells = [] ;
} ;;   

let expand_name = function 
   Hex_inner_connector_name_t.Bridge(us)-> bridge us 
   |Haddock1(qualifier,location) -> haddock1 arbitrary_dim location qualifier 
   |Broken_bridge(entry,p1,p2,exit) -> broken_bridge (entry,p1,p2,exit);; 


end ;; 

module Border = struct  
 
let upwards_small_pyramid = Hex_connector.Example.upwards_small_pyramid ;;  
let leftwards_small_pyramid = reflect upwards_small_pyramid;;
let downwards_small_pyramid dim = oppose dim upwards_small_pyramid ;;
let rightwards_small_pyramid dim = oppose dim leftwards_small_pyramid;;


let eyed_claw (d1,d2) =
   (* this function is deliberately non-curried because we need it to be a 
    univariate function, see below *) 
    let (apex,ipairs) = Hex_connector_data.default_eyed_claw d1 d2 in 
   {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [apex]);
    junction = ipairs;
    exit = Hex_island_t.I(Some(d2),Set_of_poly_pairs.empty_set);
    apex = Some(apex);
    extra_active_cells = [];
} ;;   

let noneyed_claw (dh,d) = 
    let (apex,ipairs)= Hex_connector_data.default_noneyed_claw (dh,d) in 
  {Hex_connector_t.entry = Hex_island_t.I (None, Set_of_poly_pairs_t.S [apex]);
   junction = ipairs;
   exit = Hex_island_t.I (Some d, Set_of_poly_pairs_t.S []);
   apex = Some(apex);
   extra_active_cells = [];
   };; 

let pyramid d = 
  let (apex,ipairs)= Hex_connector_data.default_pyramid d in 
  {Hex_connector_t.entry = Hex_island_t.I (None, Set_of_poly_pairs_t.S [apex]);
   junction = ipairs;
   exit = Hex_island_t.I (Some d, Set_of_poly_pairs_t.S []);
   apex = Some(apex);
   extra_active_cells = [];
  };; 

  

let small_pyramid = function 
     Hex_cardinal_direction_t.Down  -> downwards_small_pyramid arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_small_pyramid 
    |Hex_cardinal_direction_t.Right -> rightwards_small_pyramid arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_small_pyramid;; 

let upwards_border_bridge = Hex_connector.Example.upwards_border_bridge ;;  
let leftwards_border_bridge = reflect upwards_border_bridge;;
let downwards_border_bridge dim = oppose dim upwards_border_bridge ;;
let rightwards_border_bridge dim = oppose dim leftwards_border_bridge;;

let border_bridge = function 
     Hex_cardinal_direction_t.Down  -> downwards_border_bridge arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_border_bridge 
    |Hex_cardinal_direction_t.Right -> rightwards_border_bridge arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_border_bridge;; 

let upwards_walleye1 = Hex_connector.Example.upwards_walleye1 ;;  
let leftwards_walleye1 = reflect upwards_walleye1;;
let downwards_walleye1 dim = oppose dim upwards_walleye1 ;;
let rightwards_walleye1 dim = oppose dim leftwards_walleye1;;

let walleye1 = function 
     Hex_cardinal_direction_t.Down  -> downwards_walleye1 arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_walleye1 
    |Hex_cardinal_direction_t.Right -> rightwards_walleye1 arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_walleye1;; 

let upwards_byssus = Hex_connector.Example.upwards_byssus ;;  
let leftwards_byssus = reflect upwards_byssus;;
let downwards_byssus dim = oppose dim upwards_byssus ;;
let rightwards_byssus dim = oppose dim leftwards_byssus;;

let byssus = function 
     Hex_cardinal_direction_t.Down  -> downwards_byssus arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_byssus 
    |Hex_cardinal_direction_t.Right -> rightwards_byssus arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_byssus;; 

let upwards_sybil = Hex_connector.Example.upwards_sybil ;;  
let leftwards_sybil = reflect upwards_sybil;;
let downwards_sybil dim = oppose dim upwards_sybil ;;
let rightwards_sybil dim = oppose dim leftwards_sybil;;

let sybil = function 
     Hex_cardinal_direction_t.Down  -> downwards_sybil arbitrary_dim
    |Hex_cardinal_direction_t.Left  -> leftwards_sybil 
    |Hex_cardinal_direction_t.Right -> rightwards_sybil arbitrary_dim
    |Hex_cardinal_direction_t.Up    -> upwards_sybil;; 

let basic_doubling bw y =
  match bw with 
   Hex_borderwise_t.From_border -> reverse y
  |Hex_borderwise_t.To_border -> y ;;

let standard_doubling f bw x =
  let y = f x in 
  match bw with 
   Hex_borderwise_t.From_border -> reverse y
  |Hex_borderwise_t.To_border -> y ;;

let bs = Hex_double_hump_qualifier_t.Big_followed_by_small
and sb = Hex_double_hump_qualifier_t.Small_followed_by_big ;;

let d = Hex_cardinal_direction_t.Down 
and l = Hex_cardinal_direction_t.Left 
and r = Hex_cardinal_direction_t.Right
and u = Hex_cardinal_direction_t.Up ;;  

let expand_name bw = function 
   Hex_border_connector_name_t.Eyed_claw(d1,d2) -> standard_doubling eyed_claw bw (d1,d2)
   |Bs_D -> basic_doubling bw (sybil d)  
   |Bs_L -> basic_doubling bw (sybil l)  
   |Bs_R -> basic_doubling bw (byssus r)  
   |Bs_U -> basic_doubling bw (byssus u) 
   |Sb_D -> basic_doubling bw (byssus d) 
   |Sb_L -> basic_doubling bw (byssus l)
   |Sb_R -> basic_doubling bw (sybil r)
   |Sb_U -> basic_doubling bw (sybil u)   
   |Pyramid(d) -> standard_doubling pyramid bw d
   |Small_pyramid(d) -> standard_doubling small_pyramid bw d 
   |Border_bridge(d) -> standard_doubling border_bridge bw d
   |Walleye1(d) -> standard_doubling walleye1 bw d;;   

end ;; 

end ;;

let expand_name = function 
   Hex_connector_name_t.Inner(inner)-> Private.Inner.expand_name inner 
   |Border(bw,border) -> Private.Border.expand_name bw border;;