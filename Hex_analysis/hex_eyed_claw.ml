(* 

#use"Hex_analysis/hex_eyed_claw.ml";;

*)

exception Bad_specification of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;


module Private = struct 

let down = Hex_cardinal_direction_t.Down and left = Hex_cardinal_direction_t.Left  
and right = Hex_cardinal_direction_t.Right and up = Hex_cardinal_direction_t.Up ;;
let high = up and low =down;;


let apex_for_downwards_claw d1 junction = 
  let (_,minimizers) =Min.minimize_it_with_care fst junction in 
  let (x1,y1) = List.hd minimizers in 
  match d1 with 
   Hex_cardinal_direction_t.Left -> (x1,y1-1)
  |Hex_cardinal_direction_t.Right -> (x1,y1+1)
  |_->raise(Bad_specification(d1,down));;

let apex_for_leftwards_claw d1 junction = 
  let (_,maximizers) =Max.maximize_it_with_care snd junction in 
  let (x1,y1) = List.hd maximizers in 
  match d1 with 
   Hex_cardinal_direction_t.Down -> (x1+1,y1)
  |Hex_cardinal_direction_t.Up -> (x1-1,y1)
  |_->raise(Bad_specification(d1,left));;

let apex_for_rightwards_claw d1 junction = 
  let (_,minimizers) =Min.minimize_it_with_care snd junction in 
  let (x1,y1) = List.hd minimizers in 
  match d1 with 
   Hex_cardinal_direction_t.Down -> (x1+1,y1)
  |Hex_cardinal_direction_t.Up -> (x1-1,y1)
  |_->raise(Bad_specification(d1,right));;

let apex_for_upwards_claw d1 junction = 
  let (_,maximizers) =Max.maximize_it_with_care fst junction in 
  let (x1,y1) = List.hd maximizers in 
  match d1 with 
   Hex_cardinal_direction_t.Left -> (x1,y1-1)
  |Hex_cardinal_direction_t.Right -> (x1,y1+1)
  |_->raise(Bad_specification(d1,up));;

let compute_apex_coordinates d1 d2 junction = match d2 with 
   Hex_cardinal_direction_t.Down -> apex_for_downwards_claw d1 junction
  |Hex_cardinal_direction_t.Left -> apex_for_leftwards_claw d1 junction
  |Hex_cardinal_direction_t.Right -> apex_for_rightwards_claw d1 junction
  |Hex_cardinal_direction_t.Up -> apex_for_upwards_claw d1 junction ;;

let arbitrary_dim = Hex_dimension.eleven;;


let oppose (apex,cnnctr)= (Hex_ipair.oppose arbitrary_dim apex,Hex_connector.oppose arbitrary_dim cnnctr);;
let reflect (apex,cnnctr)= (Hex_ipair.reflect apex,Hex_connector.reflect cnnctr);;

let left_eyed_upwards_claw = ((4,2),Hex_connector.Example.left_eyed_upwards_claw);; 
let right_eyed_upwards_claw = ((4,3),Hex_connector.Example.right_eyed_upwards_claw);; 

let low_eyed_leftwards_claw = reflect right_eyed_upwards_claw;;
let left_eyed_downwards_claw = oppose right_eyed_upwards_claw ;;
let high_eyed_rightwards_claw = oppose low_eyed_leftwards_claw;;

let high_eyed_leftwards_claw = reflect left_eyed_upwards_claw;;
let right_eyed_downwards_claw = oppose left_eyed_upwards_claw ;;
let low_eyed_rightwards_claw = oppose high_eyed_leftwards_claw;;


let high_eyed_claw = function 
     Hex_cardinal_direction_t.Left  -> high_eyed_leftwards_claw 
    |Hex_cardinal_direction_t.Right -> high_eyed_rightwards_claw 
    |d->raise(Bad_specification(high,d));;  

let left_eyed_claw = function 
     Hex_cardinal_direction_t.Down  -> left_eyed_downwards_claw 
    |Hex_cardinal_direction_t.Up    -> left_eyed_upwards_claw
    |d->raise(Bad_specification(left,d));;  

let low_eyed_claw = function 
     Hex_cardinal_direction_t.Left  -> low_eyed_leftwards_claw 
    |Hex_cardinal_direction_t.Right -> low_eyed_rightwards_claw 
    |d->raise(Bad_specification(low,d));;  

let right_eyed_claw = function 
     Hex_cardinal_direction_t.Down  -> right_eyed_downwards_claw 
    |Hex_cardinal_direction_t.Up    -> right_eyed_upwards_claw
    |d->raise(Bad_specification(right,d));;  


let constructor d1 d2 = match d1 with 
     Hex_cardinal_direction_t.Down  -> low_eyed_claw d2
    |Hex_cardinal_direction_t.Left  -> left_eyed_claw d2
    |Hex_cardinal_direction_t.Right -> right_eyed_claw d2
    |Hex_cardinal_direction_t.Up    -> high_eyed_claw d2;;

end ;;



let constructor = Private.constructor;;
let compute_apex_coordinates = Private.compute_apex_coordinates;;

