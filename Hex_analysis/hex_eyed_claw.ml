(* 

#use"Hex_analysis/hex_eyed_claw.ml";;

*)

exception Bad_specification;;


module Private = struct 


let apex_for_downwards_claw d1 junction = 
  let (_,minimizers) =Min.minimize_it_with_care fst junction in 
  let (x1,y1) = List.hd minimizers in 
  match d1 with 
   Hex_cardinal_direction_t.Left -> (x1,y1-1)
  |Hex_cardinal_direction_t.Right -> (x1,y1+1)
  |_->raise(Bad_specification);;

let apex_for_leftwards_claw d1 junction = 
  let (_,maximizers) =Max.maximize_it_with_care snd junction in 
  let (x1,y1) = List.hd maximizers in 
  match d1 with 
   Hex_cardinal_direction_t.Down -> (x1+1,y1)
  |Hex_cardinal_direction_t.Up -> (x1-1,y1)
  |_->raise(Bad_specification);;

let apex_for_rightwards_claw d1 junction = 
  let (_,minimizers) =Min.minimize_it_with_care snd junction in 
  let (x1,y1) = List.hd minimizers in 
  match d1 with 
   Hex_cardinal_direction_t.Down -> (x1+1,y1)
  |Hex_cardinal_direction_t.Up -> (x1-1,y1)
  |_->raise(Bad_specification);;

let apex_for_upwards_claw d1 junction = 
  let (_,maximizers) =Max.maximize_it_with_care fst junction in 
  let (x1,y1) = List.hd maximizers in 
  match d1 with 
   Hex_cardinal_direction_t.Left -> (x1,y1-1)
  |Hex_cardinal_direction_t.Right -> (x1,y1+1)
  |_->raise(Bad_specification);;

let compute_apex_coordinates d1 d2 junction = match d2 with 
   Hex_cardinal_direction_t.Down -> apex_for_downwards_claw d1 junction
  |Hex_cardinal_direction_t.Left -> apex_for_leftwards_claw d1 junction
  |Hex_cardinal_direction_t.Right -> apex_for_rightwards_claw d1 junction
  |Hex_cardinal_direction_t.Up -> apex_for_upwards_claw d1 junction ;;


(*

let force_apex_in_eyed_claw apex d1 d2 old_junction =
   let (old_i,old_j) = compute_apex_coordinates_in_eyed_claw d1 d2 old_junction 
   and (new_i,new_j) = apex in 
   let di=new_i-old_i and dj=new_j-old_j in 
   Image.image (fun (i,j)->(i+di,j+dj)) old_junction;;
   

let compute_support_in_eyed_claw d1 d2 apex_cell =
    let representative = Private.eyed_claw (d1,d2) in
    let old_junction =  representative.Hex_connector_t.junction in 
    Hex_border_connector_name.compute_support_in_eyed_claw 
          d1 d2 (Hex_cell.to_int_pair apex_cell) old_junction;;

*)  

end ;;




let compute_apex_coordinates = Private.compute_apex_coordinates;;

