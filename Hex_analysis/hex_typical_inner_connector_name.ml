(* 

#use"Hex_analysis/hex_typical_inner_connector_name.ml";;

      H1:                   H2:                  H3:
--------------------------------------------------------------
|   | A | b |   |   |   |   | A | e |   |   |   | A | g |   |
--------------------------------------------------------------
  | A | b | a |   |   |   | A | e | d |   |   | f | g | f |   |
  --------------------------------------------------------------
    | a | c | c |   |   | A |   | d | A |   | A | i | h | h |   |
    --------------------------------------------------------------
      | A | A |   |   |   | A | A |   |   |   | i | A | A |   |   |
      --------------------------------------------------------------

      H4:                   H5:                  H6:
--------------------------------------------------------------
|   | A | A | k |   |   | A | n |   |   |   |   |   |   |   |
--------------------------------------------------------------
  | j | j | k | l |   | A | n | A |   |   |   |   |   |   |   |
  --------------------------------------------------------------
    | A |   | l | m |   | o | o |   |   |   |   |   |   |   |   |
    --------------------------------------------------------------
      | A | A | m | A |   | A |   |   |   |   |   |   |   |   |   |
      --------------------------------------------------------------


*)

let prepare_for_journey = 
  [
     Hex_typical_inner_connector_name_t.Haddock1 , 
       ([(1,2);(2,1)],[ (1,3);(2,2);(2,3);(3,1);(3,2);(3,3)],[(4,1);(4,2)],"hk1"); 
     Hex_typical_inner_connector_name_t.Haddock2 , 
       ([(3,4)],[ (2,3);(1,4);(3,3);(2,4)],[(1,3);(2,2);(3,1);(4,1);(4,2)],"hk2") ; 
  ];;

let all = Image.image fst prepare_for_journey ;;
let to_readable_string tic = 
    let (_,_,_,name) = List.assoc tic prepare_for_journey in 
    name ;;

let upwards_version tic = 
    let (entry1,ju,exit1,_) = List.assoc tic prepare_for_journey in 
   {Hex_connector_t.entry = Hex_island_t.I(Hex_anchor_t.No_anchor,Set_of_poly_pairs.safe_set entry1);
    junction = ju;
    exit = Hex_island_t.I(Hex_anchor_t.No_anchor,Set_of_poly_pairs.safe_set exit1);
    apex = Some(List.hd entry1) ;
    };;

let specify_side tic side = 
     let u = upwards_version tic in 
     let l = Hex_connector.reflect u in 
      match side with 
     Hex_cardinal_direction_t.Down  -> Hex_connector.oppose Hex_dimension.eleven u
    |Hex_cardinal_direction_t.Left  -> l
    |Hex_cardinal_direction_t.Right -> Hex_connector.oppose Hex_dimension.eleven l
    |Hex_cardinal_direction_t.Up    -> u;; 
    
let specify_orientation undirected_tic is_direct = 
   if is_direct 
   then undirected_tic 
   else Hex_connector.reverse  undirected_tic ;;

let full_constructor tic (side,is_direct) =
      specify_orientation (specify_side tic side) is_direct ;;

(*
let full_constructor tic (side,is_direct) new_apex=
     let default_example = specify_orientation (specify_side tic side) is_direct in 
     let (x1,y1) = List.hd(default_example.Hex_connector_t.junction) 
     and (x2,y2) = new_apex in 
     let dx=x2-x1 and dy=y2-y1 in 
     Hex_connector.translate (dx,dy) default_example;;    
*) 