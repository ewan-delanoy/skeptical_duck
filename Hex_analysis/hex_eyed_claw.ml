(* 

#use"Hex_analysis/hex_eyed_claw.ml";;

*)

exception Bad_specification of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Incorrect_abscissa of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Incorrect_ordinate of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Mismatch of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;

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


let oppose (apex,ipairs)= (Hex_ipair.oppose arbitrary_dim apex,
   Image.image (Hex_ipair.oppose arbitrary_dim) ipairs);;
let reflect (apex,ipairs)= (Hex_ipair.reflect apex,Image.image Hex_ipair.reflect ipairs);;

let left_eyed_upwards_claw = ((4,2),
    [(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); 
     (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); 
     (3, 1); (3, 2); (3, 3); (3, 4); (3, 5); 
                     (4, 3)]
);; 
let right_eyed_upwards_claw = ((4,3),
   [ 
        (1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); 
        (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); 
        (3, 1); (3, 2); (3, 3); (3, 4); (3, 5); 
                (4, 2)
    ]);; 

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

let default_constructor d1 d2 = match d1 with 
     Hex_cardinal_direction_t.Down  -> low_eyed_claw d2
    |Hex_cardinal_direction_t.Left  -> left_eyed_claw d2
    |Hex_cardinal_direction_t.Right -> right_eyed_claw d2
    |Hex_cardinal_direction_t.Up    -> high_eyed_claw d2;;

let advanced_constructor d1 d2 (i,j)=
   let ((i0,j0),default_ipairs) =  default_constructor d1 d2 in 
   Image.image (fun (x,y)-> (i-i0+x,j-j0+y)) default_ipairs;;

module Check = struct 

let left_eyed_downwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>dim-3         then raise(Incorrect_abscissa(left,down)) else 
    if (y<5)||(y>dim-2) then raise(Incorrect_ordinate(left,down)) else
    ();;

let right_eyed_downwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>dim-3         then raise(Incorrect_abscissa(right,down)) else 
    if (y<6)||(y>dim-1) then raise(Incorrect_ordinate(right,down)) else
    ();;    


let left_eyed_upwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>4         then raise(Incorrect_abscissa(left,up)) else 
    if (y<2)||(y>dim-5) then raise(Incorrect_ordinate(left,up)) else
    ();;

let right_eyed_upwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>4         then raise(Incorrect_abscissa(right,up)) else 
    if (y<3)||(y>dim-4) then raise(Incorrect_ordinate(right,up)) else
    ();;    

let high_eyed_leftwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>4            then raise(Incorrect_ordinate(high,left)) else
    if (x<2)||(x>dim-5) then raise(Incorrect_abscissa(high,left)) else 
    ();;

let low_eyed_leftwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>4            then raise(Incorrect_ordinate(low,left)) else
    if (x<3)||(x>dim-4) then raise(Incorrect_abscissa(low,left)) else 
    ();;    


let high_eyed_rightwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>dim-3        then raise(Incorrect_ordinate(high,left)) else
    if (x<5)||(x>dim-2) then raise(Incorrect_abscissa(high,left)) else 
    ();;

let low_eyed_rightwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>dim-3        then raise(Incorrect_ordinate(low,left)) else
    if (x<6)||(x>dim-1) then raise(Incorrect_abscissa(low,left)) else 
    ();;    


let left_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Down -> left_eyed_downwards dim cell 
    |Hex_cardinal_direction_t.Up   -> left_eyed_upwards dim cell    
    |                           _  -> raise(Mismatch(left,direction));;

let right_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Down -> right_eyed_downwards dim cell 
    |Hex_cardinal_direction_t.Up   -> right_eyed_upwards dim cell    
    |                           _  -> raise(Mismatch(right,direction));;

let high_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Left  -> high_eyed_leftwards dim cell 
    |Hex_cardinal_direction_t.Right -> high_eyed_rightwards dim cell    
    |                           _   -> raise(Mismatch(high,direction));;

let low_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Left  -> low_eyed_leftwards dim cell 
    |Hex_cardinal_direction_t.Right -> low_eyed_rightwards dim cell    
    |                           _   -> raise(Mismatch(low,direction));;

let eyed dim =function
   Hex_cardinal_direction_t.Down  -> low_eyed dim 
  |Hex_cardinal_direction_t.Left  -> left_eyed dim
  |Hex_cardinal_direction_t.Right -> right_eyed dim
  |Hex_cardinal_direction_t.Up    -> high_eyed dim ;;  

end ;;


end ;;

let advanced_constructor = Private.advanced_constructor;;
let check_parameters = Private.Check.eyed;;
let compute_apex_coordinates = Private.compute_apex_coordinates;;
let default_constructor = Private.default_constructor;;

