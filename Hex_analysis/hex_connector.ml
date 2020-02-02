(* 

#use"Hex_analysis/hex_connector.ml";;

*)

module Private = struct 

let translate cnnctr (dx,dy)=  
   let trl = (fun z->
      Set_of_poly_pairs.safe_set( Set_of_poly_pairs.image (fun (x,y)->(x+dx,y+dy)) z) 
   )  in 
   let (Hex_island_t.I(opt1,elts1)) = cnnctr.Hex_connector_t.entry 
   and (Hex_island_t.I(opt2,elts2)) = cnnctr.Hex_connector_t.exit in 
   {
    Hex_connector_t.entry =Hex_island_t.I(opt1,trl elts1);
    junction = Image.image (fun (x,y)->(x+dx,y+dy)) (cnnctr.Hex_connector_t.junction) ;
    exit = Hex_island_t.I(opt2,trl elts2);
};;

end ;;


let check_entry island cnnctr =
    Hex_island.is_included_in cnnctr.Hex_connector_t.entry island;;

let check_exit island cnnctr =
    Hex_island.is_included_in cnnctr.Hex_connector_t.exit island;;

let oppose dim cnnctr = 
  let on_island = Hex_island.oppose dim
  and on_pairs =  Image.image (Hex_ipair.oppose dim) in 
{
    Hex_connector_t.entry =on_island (cnnctr.Hex_connector_t.entry);
    junction =  on_pairs (cnnctr.Hex_connector_t.junction) ;
    exit = on_island (cnnctr.Hex_connector_t.entry);
};;

let reflect cnnctr = {
    Hex_connector_t.entry = Hex_island.reflect (cnnctr.Hex_connector_t.entry);
    junction = Image.image Hex_ipair.reflect (cnnctr.Hex_connector_t.junction) ;
    exit = Hex_island.reflect (cnnctr.Hex_connector_t.entry);
};;

let reverse cnnctr = {
    cnnctr with  
    Hex_connector_t.entry = (cnnctr.Hex_connector_t.exit);
                    exit  = (cnnctr.Hex_connector_t.entry);
};;


let translates formal_dim cnnctr = 
   let (Hex_dimension_t.D dim) = formal_dim in 
   let (Hex_island_t.I(opt1,elts1)) = cnnctr.Hex_connector_t.entry 
   and (Hex_island_t.I(opt2,elts2)) = cnnctr.Hex_connector_t.exit 
   and elts3 = cnnctr.Hex_connector_t.junction in 
   let opt = (if opt1<>None then opt1 else opt2) in 
   let base = Hex_cardinal_direction.authorized_translations formal_dim opt in 
   let elts = Set_of_poly_pairs.fold_merge [elts1;elts2;Set_of_poly_pairs.sort elts3] in 
   let abscissas = Set_of_poly_pairs.image fst elts 
   and ordinates = Set_of_poly_pairs.image snd elts in 
   let xmin = Min.list abscissas and xmax = Max.list abscissas 
   and ymin = Min.list ordinates and ymax = Max.list ordinates in    
   let cleaned_base =  List.filter ( 
       fun (dx,dy) ->  (1-xmin <= dx) && (dx <= dim-xmax) 
                    && (1-ymin <= dy) && (dy <= dim-ymax) 
    )  base in 
   Image.image (Private.translate cnnctr) cleaned_base ;; 

module Example = struct 

let bs_rightwards_claw = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(4,1)]);
    junction = [(4, 3); (3, 3); (2, 3); (1, 3); (4, 2); (2, 2); (3, 2); (3, 1)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Right),Set_of_poly_pairs.empty_set);
} ;;   

let high_eyed_rightwards_claw = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(5,1)]);
    junction = [                        (1, 4); 
                                (2, 3); (2, 4); 
                        (3, 2); (3, 3); (3, 4); 
                        (4, 2); (4, 3); (4, 4);
                        (5, 2); (5, 3); (5, 4); 
                (6, 1); (6, 2); (6, 3); (6, 4); 
                        (7, 2); (7, 3); (7, 4)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Right),Set_of_poly_pairs.empty_set);
} ;;   

let low_eyed_rightwards_claw = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(6,1)]);
    junction = [                        (1, 4); 
                                (2, 3); (2, 4); 
                        (3, 2); (3, 3); (3, 4); 
                        (4, 2); (4, 3); (4, 4);
                (5, 1); (5, 2); (5, 3); (5, 4); 
                        (6, 2); (6, 3); (6, 4); 
                        (7, 2); (7, 3); (7, 4)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Right),Set_of_poly_pairs.empty_set);
} ;;   


let northeast_bridge = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(3,3)]);
    junction = [(2, 4); (3, 4)];
    exit = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(2,5)]);
} ;;   

let northwest_bridge = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(3,3)]);
    junction = [(2, 3); (3, 2)];
    exit = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(2,2)]);
} ;;   


let rightwards_pyramid = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(6,1)]);
    junction =  [(7, 4); (8, 4); (5, 4); (6, 4); (6, 3); (8, 3); (8, 2); (7, 3); (7, 1);
   (6, 2); (1, 4); (2, 4); (3, 4); (4, 4); (2, 3); (4, 3); (3, 2); (3, 3);
   (5, 1); (5, 2); (4, 2); (7, 2)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Right),Set_of_poly_pairs.empty_set);
} ;;   

let rightwards_small_pyramid = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(4,1)]);
    junction =  [(5, 3); (4, 3); (2, 3); (1, 3); (5, 2); (2, 2); (4, 2); (5, 1); 
     (3, 2); (3, 1)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Right),Set_of_poly_pairs.empty_set);
} ;;   

let sb_rightwards_claw = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(3,1)]);
    junction = [(4, 3); (3, 3); (2, 3); (1, 3); (4, 2); (2, 2); (3, 2); (4, 1)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Right),Set_of_poly_pairs.empty_set);
} ;;   

(*
let oppose = Hex_connector.oppose Hex_dimension.eleven and reflect = Hex_connector.reflect ;;

let low_eyed_leftwards_claw = oppose high_eyed_rightwards_claw;;
let left_eyed_downwards_claw = reflect high_eyed_rightwards_claw;;
let right_eyed_upwards_claw = oppose left_eyed_downwards_claw;;

let high_eyed_leftwards_claw = oppose low_eyed_rightwards_claw;;
let left_eyed_upwards_claw = reflect high_eyed_leftwards_claw;;
let right_eyed_downwards_claw = oppose left_eyed_upwards_claw;;

let sb_leftwards_claw = oppose bs_rightwards_claw;;
let bs_upwards_claw = reflect sb_leftwards_claw;;
let sb_downwards_claw = oppose bs_upwards_claw;;

let bs_leftwards_claw = oppose sb_rightwards_claw;;
let sb_upwards_claw = reflect bs_leftwards_claw;;
let bs_downwards_claw = oppose sb_upwards_claw;;

let leftwards_pyramid = oppose rightwards_pyramid;;
let upwards_pyramid = reflect leftwards_pyramid;;
let downwards_pyramid = oppose upwards_pyramid;;

let leftwards_small_pyramid = oppose rightwards_small_pyramid;;
let upwards_small_pyramid = reflect leftwards_small_pyramid;;
let downwards_small_pyramid = oppose upwards_small_pyramid;;

let southwest_bridge = oppose northeast_bridge ;;
let south_bridge = reflect  northeast_bridge ;;
let north_bridge = oppose south_bridge ;;

let southeast_bridge = oppose northwest_bridge ;;

 *)

end ;;
