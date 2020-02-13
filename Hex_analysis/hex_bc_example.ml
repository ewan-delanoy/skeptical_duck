(* 

#use"Hex_analysis/hex_bc_example.ml";;

bc is short for Border connector

*)

module Private = struct 

let arbitrary_dim = Hex_dimension.eleven;;
let oppose (apex,ipairs)= (Hex_ipair.oppose arbitrary_dim apex,
   Image.image (Hex_ipair.oppose arbitrary_dim) ipairs);;
let reflect (apex,ipairs)= (Hex_ipair.reflect apex,Image.image Hex_ipair.reflect ipairs);;


let bs_upwards_claw = ((3, 1),
    [(1, 1); (1, 2); (1, 3); (1, 4); (2, 1); (2, 3); (2, 2); (3, 2)]) ;;   

let sb_upwards_claw = ((3, 2),
   [(1, 1); (1, 2); (1, 3); (1, 4); (2, 1); (2, 3); (2, 2); (3, 1)]) ;;

let upwards_pyramid = ((4, 3),
    [(1, 1); (1, 2);    (1, 3); (1, 4);   (1, 5); (1, 6);   (1, 7); (1, 8); 
     (2, 1); (2, 3);    (2, 5); (2, 7);   (2, 2); (3, 1);   (2, 6); (3, 6); 
     (3, 2); (3, 5);    (3, 3); (4, 2);   (3, 4); (4, 4);
    ]);;

let sb_leftwards_claw = reflect bs_upwards_claw;;
let sb_downwards_claw = oppose bs_upwards_claw;;
let bs_rightwards_claw = oppose sb_leftwards_claw ;;

let bs_leftwards_claw = reflect sb_upwards_claw;;
let bs_downwards_claw = oppose sb_upwards_claw;;
let sb_rightwards_claw = oppose bs_leftwards_claw ;;

let leftwards_pyramid = reflect upwards_pyramid;;
let downwards_pyramid = oppose upwards_pyramid ;;
let rightwards_pyramid = oppose leftwards_pyramid;;

let bs_claw = function 
     Hex_cardinal_direction_t.Down  -> bs_downwards_claw 
    |Hex_cardinal_direction_t.Left  -> bs_leftwards_claw
    |Hex_cardinal_direction_t.Right -> bs_rightwards_claw 
    |Hex_cardinal_direction_t.Up    -> bs_upwards_claw;;    



let sb_claw = function 
     Hex_cardinal_direction_t.Down  -> sb_downwards_claw 
    |Hex_cardinal_direction_t.Left  -> sb_leftwards_claw
    |Hex_cardinal_direction_t.Right -> sb_rightwards_claw 
    |Hex_cardinal_direction_t.Up    -> sb_upwards_claw;;    

let default_noneyed_claw (dh,d) = match dh with 
      Hex_double_hump_qualifier_t.Big_followed_by_small -> bs_claw d 
     |Hex_double_hump_qualifier_t.Small_followed_by_big -> sb_claw d ;;

let default_pyramid = function 
     Hex_cardinal_direction_t.Down  -> downwards_pyramid 
    |Hex_cardinal_direction_t.Left  -> leftwards_pyramid 
    |Hex_cardinal_direction_t.Right -> rightwards_pyramid 
    |Hex_cardinal_direction_t.Up    -> upwards_pyramid;;      

let advanced_noneyed_claw dh d (i,j)=
   let ((i0,j0),default_ipairs) =  default_noneyed_claw (dh,d) in 
   Image.image (fun (x,y)-> (i-i0+x,j-j0+y)) default_ipairs;;


let advanced_pyramid d (i,j)=
   let ((i0,j0),default_ipairs) =  default_pyramid d in 
   Image.image (fun (x,y)-> (i-i0+x,j-j0+y)) default_ipairs;;

end ;;

let advanced_noneyed_claw = Private.advanced_noneyed_claw ;;
let advanced_pyramid = Private.advanced_pyramid ;;

let default_noneyed_claw = Private.default_noneyed_claw ;;
let default_pyramid = Private.default_pyramid ;;

