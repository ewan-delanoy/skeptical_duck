(* 

#use"Hex_analysis/hex_planar_linker_data.ml";;

*)

exception Bad_distance of Hex_planar_linker_data_t.t ;;
exception Coordinate_out_of_bounds of int * int ;;
exception Bad_eyed_claw_specification of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t;;

 module Private = struct 



let translate_point (dx,dy) (x,y)= (x+dx,y+dy);;

let translate d_pair data =
   let tr1 = translate_point d_pair in 
   {
    data with  
    Hex_planar_linker_data_t.apex = tr1 (data.Hex_planar_linker_data_t.apex) ;
    Hex_planar_linker_data_t.support = 
         (Image.image tr1 (data.Hex_planar_linker_data_t.support)) ;
};; 

let force_new_apex (new_x,new_y) data =
    let (x,y) = data.Hex_planar_linker_data_t.apex in 
    translate (new_x-x,new_y-y) data;;

let reflect_point (x,y) = (y,x);;


let reflect_without_renormalizing data =
   let tr1 = reflect_point in 
   {
    data with  
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction.reflect (data.Hex_planar_linker_data_t.ground) ; 
    Hex_planar_linker_data_t.apex = tr1 (data.Hex_planar_linker_data_t.apex) ;
    Hex_planar_linker_data_t.support = 
        (Image.image tr1 (data.Hex_planar_linker_data_t.support)) ;
};; 

let oppose_point (x,y) = (-x,-y);;

let oppose_cardinal_direction = function 
     Hex_cardinal_direction_t.Down  -> Hex_cardinal_direction_t.Up
    |Hex_cardinal_direction_t.Left  -> Hex_cardinal_direction_t.Right
    |Hex_cardinal_direction_t.Right -> Hex_cardinal_direction_t.Left
    |Hex_cardinal_direction_t.Up    -> Hex_cardinal_direction_t.Down;;

let oppose_without_renormalizing data =
   let tr1 = oppose_point in 
   {
    data with  
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction.opposite (data.Hex_planar_linker_data_t.ground) ; 
    Hex_planar_linker_data_t.apex = tr1 (data.Hex_planar_linker_data_t.apex) ;
    Hex_planar_linker_data_t.support = 
        (Image.image tr1 (data.Hex_planar_linker_data_t.support)) ;
};; 

let put_support_in_order_if_allowed data = 
   if data.Hex_planar_linker_data_t.is_reducible_to_pairs 
   then data 
   else {
           data with 
           Hex_planar_linker_data_t.support = 
            (Ordered.safe_set Total_ordering.standard2 data.Hex_planar_linker_data_t.support)
        };;

let renormalize data= 
   (* ensure that all coordinates are positive *)
   let points=(data.Hex_planar_linker_data_t.apex)::(data.Hex_planar_linker_data_t.support) in 
   let (_,xmin) = Min.minimize_it fst points 
   and (_,ymin) = Min.minimize_it snd points in 
   put_support_in_order_if_allowed (translate (1-xmin,1-ymin) data);;

let reflect data = renormalize(reflect_without_renormalizing data);;
let oppose data = renormalize(oppose_without_renormalizing data);;

let list_for_eyed_rightwards_claw = 
        [(1, 4); (2, 3); (2, 4); (3, 2); (3, 3); (3, 4); (4, 2); (4, 3); (4, 4);
         (5, 2); (5, 3); (5, 4); (6, 1); (6, 2); (6, 3); (6, 4); (7, 2); (7, 3);
         (7, 4)];;

let high_eyed_rightwards_claw = {
    
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction_t.Right ; 
    Hex_planar_linker_data_t.distance_from_ground = 3 ; 
    Hex_planar_linker_data_t.is_reducible_to_pairs = false ; 
    Hex_planar_linker_data_t.apex = (5,1) ;
    Hex_planar_linker_data_t.support = list_for_eyed_rightwards_claw ;
};; 


let low_eyed_rightwards_claw = {
    
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction_t.Right ; 
    Hex_planar_linker_data_t.distance_from_ground = 3 ; 
    Hex_planar_linker_data_t.is_reducible_to_pairs = false ; 
    Hex_planar_linker_data_t.apex = (6,1) ;
    Hex_planar_linker_data_t.support =  
       [(1, 4); (2, 3); (2, 4); (3, 2); (3, 3); (3, 4); (4, 2); (4, 3); 
        (4, 4); (5, 1); (5, 2); (5, 3); (5, 4); (6, 2); (6, 3); (6, 4); 
        (7, 2); (7, 3); (7, 4)]
};; 

let low_eyed_leftwards_claw = oppose high_eyed_rightwards_claw;;
let left_eyed_downwards_claw = reflect high_eyed_rightwards_claw;;
let right_eyed_upwards_claw = oppose left_eyed_downwards_claw;;

let high_eyed_leftwards_claw = oppose low_eyed_rightwards_claw;;
let left_eyed_upwards_claw = reflect high_eyed_leftwards_claw;;
let right_eyed_downwards_claw = oppose left_eyed_upwards_claw;;


let bs_rightwards_claw = {
    
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction_t.Right ; 
    Hex_planar_linker_data_t.distance_from_ground = 3 ; 
    Hex_planar_linker_data_t.is_reducible_to_pairs = true ; 
    Hex_planar_linker_data_t.apex = (4,1) ;
    Hex_planar_linker_data_t.support = 
        [(4, 3) ;  (3, 3); (2, 3) ; (1, 3); (4, 2); (2, 2); (3, 2) ; (3, 1)] ;
};; 

let sb_rightwards_claw = {
    
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction_t.Right ; 
    Hex_planar_linker_data_t.distance_from_ground = 3 ; 
    Hex_planar_linker_data_t.is_reducible_to_pairs = true ; 
    Hex_planar_linker_data_t.apex = (3,1) ;
    Hex_planar_linker_data_t.support = 
        [(4, 3) ;  (3, 3); (2, 3) ; (1, 3); (4, 2); (2, 2); (3, 2) ; (4, 1)] ;
};; 

let sb_leftwards_claw = oppose bs_rightwards_claw;;
let bs_upwards_claw = reflect sb_leftwards_claw;;
let sb_downwards_claw = oppose bs_upwards_claw;;

let bs_leftwards_claw = oppose sb_rightwards_claw;;
let sb_upwards_claw = reflect bs_leftwards_claw;;
let bs_downwards_claw = oppose sb_upwards_claw;;

let rightwards_pyramid = {
    
    Hex_planar_linker_data_t.ground = Hex_cardinal_direction_t.Right ; 
    Hex_planar_linker_data_t.distance_from_ground = 4 ; 
    Hex_planar_linker_data_t.is_reducible_to_pairs = true ; 
    Hex_planar_linker_data_t.apex = (6,1) ;
    Hex_planar_linker_data_t.support = 
        [(7, 4); (8, 4); (5, 4); (6, 4); (6, 3); (8, 3); (8, 2); (7, 3); (7, 1);
   (6, 2); (1, 4); (2, 4); (3, 4); (4, 4); (2, 3); (4, 3); (3, 2); (3, 3);
   (5, 1); (5, 2); (4, 2); (7, 2)];
};; 

let leftwards_pyramid = oppose rightwards_pyramid;;
let upwards_pyramid = reflect leftwards_pyramid;;
let downwards_pyramid = oppose upwards_pyramid;;


let check_finished_data formal_dim data =
    let (Hex_dimension_t.D dim) = formal_dim in   
    if not(Hex_cardinal_direction.test_for_distance 
         (formal_dim,data.Hex_planar_linker_data_t.ground) 
          data.Hex_planar_linker_data_t.apex 
            data.Hex_planar_linker_data_t.distance_from_ground
           )
    then raise(Bad_distance(data))
    else 
    match Option.seek (
       fun (x,y) -> (x<1) || (x>dim) || (y<1) || (y>dim)
    ) data.Hex_planar_linker_data_t.support with 
    Some(x0,y0)->raise(Coordinate_out_of_bounds(x0,y0))
    |None -> ();;

let test_finished_data dim data =
   try (fun _->true)(check_finished_data dim data) with _->false;;

let down = Hex_cardinal_direction_t.Down;;
let left = Hex_cardinal_direction_t.Left;;
let right = Hex_cardinal_direction_t.Right;;
let up = Hex_cardinal_direction_t.Up;;
let high = up and low = up;;
let bs = Hex_double_hump_qualifier_t.Big_followed_by_small;;
let sb = Hex_double_hump_qualifier_t.Small_followed_by_big;;

let some_high_eyed_claw  = function
     Hex_cardinal_direction_t.Left  -> high_eyed_leftwards_claw
    |Hex_cardinal_direction_t.Right -> high_eyed_rightwards_claw
    | d -> raise(Bad_eyed_claw_specification(high,d));;

let some_left_eyed_claw  = function
     Hex_cardinal_direction_t.Down  -> left_eyed_downwards_claw
    |Hex_cardinal_direction_t.Up    -> left_eyed_upwards_claw
    | d -> raise(Bad_eyed_claw_specification(left,d));;

let some_low_eyed_claw  = function
     Hex_cardinal_direction_t.Left  -> low_eyed_leftwards_claw
    |Hex_cardinal_direction_t.Right -> low_eyed_rightwards_claw
    | d -> raise(Bad_eyed_claw_specification(low,d));;

let some_right_eyed_claw  = function
     Hex_cardinal_direction_t.Down  -> right_eyed_downwards_claw
    |Hex_cardinal_direction_t.Up    -> right_eyed_upwards_claw
    | d -> raise(Bad_eyed_claw_specification(right,d));;


let some_eyed_claw d1 d2= match d1 with 
     Hex_cardinal_direction_t.Down  -> some_low_eyed_claw d2 
    |Hex_cardinal_direction_t.Left  -> some_left_eyed_claw d2
    |Hex_cardinal_direction_t.Right -> some_right_eyed_claw d2
    |Hex_cardinal_direction_t.Up    -> some_high_eyed_claw d2;;

let eyed_claw d1 d2 p = 
   force_new_apex p (some_eyed_claw d1 d2);;

let some_bs_claw d = 
    match d with 
     Hex_cardinal_direction_t.Down  -> bs_downwards_claw 
    |Hex_cardinal_direction_t.Left  -> bs_leftwards_claw  
    |Hex_cardinal_direction_t.Right -> bs_rightwards_claw  
    |Hex_cardinal_direction_t.Up    -> bs_upwards_claw  ;;

let bs_claw d p = 
   force_new_apex p (some_bs_claw d);;

let some_sb_claw d = 
    match d with 
     Hex_cardinal_direction_t.Down  -> sb_downwards_claw 
    |Hex_cardinal_direction_t.Left  -> sb_leftwards_claw  
    |Hex_cardinal_direction_t.Right -> sb_rightwards_claw  
    |Hex_cardinal_direction_t.Up    -> sb_upwards_claw  ;;

let sb_claw d p = 
   force_new_apex p (some_sb_claw d);;

let noneyed_claw double_hump d p = match double_hump with 
   Hex_double_hump_qualifier_t.Big_followed_by_small -> bs_claw d p 
  |Hex_double_hump_qualifier_t.Small_followed_by_big -> sb_claw d p;;

let some_pyramid d = 
    match d with 
     Hex_cardinal_direction_t.Down  -> downwards_pyramid
    |Hex_cardinal_direction_t.Left  -> leftwards_pyramid
    |Hex_cardinal_direction_t.Right -> rightwards_pyramid 
    |Hex_cardinal_direction_t.Up    -> upwards_pyramid  ;;
  
let pyramid d p = 
   force_new_apex p (some_pyramid d);;

let cell_support data =
   Hex_cell_set.safe_set 
    (Image.image Hex_cell.of_int_pair 
      data.Hex_planar_linker_data_t.support
     );;


let unfold_eyed_claws_around_ipair dim ipair=
   List.filter (
       fun (d1,d2) ->
        test_finished_data dim (eyed_claw d1 d2 ipair)
   ) [
       left,up; left,down;high,left;high,right;
      right,up;right,down; low,left; low,right;
     ];;

let unfold_noneyed_claws_around_ipair dim ipair=
   List.filter (
       fun (dh,d) ->
        test_finished_data dim (noneyed_claw dh d ipair)
   ) [
       bs,left;bs,up;bs,right;bs,down;
       sb,left;sb,up;sb,right;sb,down; 
     ];;

let unfold_pyramids_around_ipair dim ipair=
   List.filter (
       fun d ->
        test_finished_data dim (pyramid d ipair)
   ) [
       left;up;right;down
     ];;


 end ;; 


let bridges_in_noneyed_claw double_hump d p =
    let the_data = Private.noneyed_claw double_hump d p in 
    Listennou.extract_successive_pairs_from_even_list 
      (the_data.Hex_planar_linker_data_t.support);;

let bridges_in_pyramid d p =
    let the_data = Private.pyramid d p in 
    Listennou.extract_successive_pairs_from_even_list
      (the_data.Hex_planar_linker_data_t.support);;

let check_eyed_claw dim d1 d2 cell =
   let the_data = Private.eyed_claw d1 d2 (Hex_cell.to_int_pair cell) in 
   Private.check_finished_data dim the_data;;

let check_noneyed_claw dim double_hump d cell =
   let the_data = Private.noneyed_claw double_hump d (Hex_cell.to_int_pair cell) in 
   Private.check_finished_data dim the_data;;

let check_pyramid dim d cell =
   let the_data = Private.pyramid  d (Hex_cell.to_int_pair cell) in 
   Private.check_finished_data dim the_data;;



let support_for_eyed_claw d1 d2 cell =
   let the_data = Private.eyed_claw d1 d2 (Hex_cell.to_int_pair cell) in 
   Private.cell_support the_data;;

let support_for_noneyed_claw double_hump d cell =
   let the_data = Private.noneyed_claw double_hump d (Hex_cell.to_int_pair cell) in 
   Private.cell_support the_data;;

let support_for_pyramid d cell =
   let the_data = Private.pyramid  d (Hex_cell.to_int_pair cell) in 
   Private.cell_support the_data;;

let unfold_eyed_claws_around_cell dim  cell =
   Private.unfold_eyed_claws_around_ipair dim (Hex_cell.to_int_pair cell);;

let unfold_noneyed_claws_around_cell dim  cell =
   Private.unfold_noneyed_claws_around_ipair dim (Hex_cell.to_int_pair cell);;

let unfold_pyramids_around_cell dim  cell =
   Private.unfold_pyramids_around_ipair dim (Hex_cell.to_int_pair cell);;

