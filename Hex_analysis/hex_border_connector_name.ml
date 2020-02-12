(* 

#use"Hex_analysis/hex_border_connector_name.ml";;

*)

exception Bad_eyed_claw_specification ;; 

module Private = struct 


let apex_for_downwards_claw d1 junction = 
  let (_,minimizers) =Min.minimize_it_with_care fst junction in 
  let (x1,y1) = List.hd minimizers in 
  match d1 with 
   Hex_cardinal_direction_t.Left -> (x1,y1-1)
  |Hex_cardinal_direction_t.Right -> (x1,y1+1)
  |_->raise(Bad_eyed_claw_specification);;

let apex_for_leftwards_claw d1 junction = 
  let (_,maximizers) =Max.maximize_it_with_care snd junction in 
  let (x1,y1) = List.hd maximizers in 
  match d1 with 
   Hex_cardinal_direction_t.Down -> (x1+1,y1)
  |Hex_cardinal_direction_t.Up -> (x1-1,y1)
  |_->raise(Bad_eyed_claw_specification);;

let apex_for_rightwards_claw d1 junction = 
  let (_,minimizers) =Min.minimize_it_with_care snd junction in 
  let (x1,y1) = List.hd minimizers in 
  match d1 with 
   Hex_cardinal_direction_t.Down -> (x1+1,y1)
  |Hex_cardinal_direction_t.Up -> (x1-1,y1)
  |_->raise(Bad_eyed_claw_specification);;

let apex_for_upwards_claw d1 junction = 
  let (_,maximizers) =Max.maximize_it_with_care fst junction in 
  let (x1,y1) = List.hd maximizers in 
  match d1 with 
   Hex_cardinal_direction_t.Left -> (x1,y1-1)
  |Hex_cardinal_direction_t.Right -> (x1,y1+1)
  |_->raise(Bad_eyed_claw_specification);;

let compute_apex_coordinates_in_eyed_claw d1 d2 junction = match d2 with 
   Hex_cardinal_direction_t.Down -> apex_for_downwards_claw d1 junction
  |Hex_cardinal_direction_t.Left -> apex_for_leftwards_claw d1 junction
  |Hex_cardinal_direction_t.Right -> apex_for_rightwards_claw d1 junction
  |Hex_cardinal_direction_t.Up -> apex_for_upwards_claw d1 junction ;;

let force_apex_in_eyed_claw apex d1 d2 old_junction =
   let (old_i,old_j) = compute_apex_coordinates_in_eyed_claw d1 d2 old_junction 
   and (new_i,new_j) = apex in 
   let di=new_i-old_i and dj=new_j-old_j in 
   Image.image (fun (i,j)->(i+di,j+dj)) old_junction;;
   

end ;;

let compute_support_in_eyed_claw d1 d2 apex old_junction=
    let new_junction = Private.force_apex_in_eyed_claw apex d1 d2 old_junction  in 
    Hex_cell_set.safe_set(Image.image Hex_cell.of_int_pair new_junction);;

let for_side side =
    let ortho = Hex_cardinal_direction.orthogonal_directions side in 
    (Image.image (fun specifier->
        Hex_border_connector_name_t.Eyed_claw(specifier,side)
    ) ortho)
    @
    (
     Image.image (
       fun qualifier -> Hex_border_connector_name_t.Noneyed_claw(qualifier,side)
     )   Hex_double_hump_qualifier.all
    )
    @
    [
      Hex_border_connector_name_t.Pyramid(side);
      Hex_border_connector_name_t.Small_pyramid(side)  
    ] ;;

let to_nondefault_molecular_linker nm junction = match nm with 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         let p= Private.compute_apex_coordinates_in_eyed_claw d1 d2 junction in 
         Some(Hex_molecular_linker.constructor [
              Hex_atomic_linker_t.Eyed_claw(d1,d2,Hex_cell.of_int_pair p)])
   |Noneyed_claw(_,_) -> None
   |Pyramid(_) -> None 
   |Small_pyramid(_) -> None ;;
    

let to_readable_string = function 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         (Hex_cardinal_direction.for_eye_description d1)^"e"^(Hex_cardinal_direction.for_ground_description d2)
   |Noneyed_claw(dh,d) -> let s = (Hex_double_hump_qualifier.to_readable_string dh) in 
                          (String.sub s 0 1)^(Hex_cardinal_direction.for_ground_description d)^(String.sub s 1 1)
   |Pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"py" 
   |Small_pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"sy" ;;
