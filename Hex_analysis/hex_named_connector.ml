(* 

#use"Hex_analysis/hex_named_connector.ml";;

*)

exception Bad_specification_for_eyed_claw of 
     Hex_cardinal_direction_t.t *  Hex_cardinal_direction_t.t;;
exception Precomputed_starter_exn of Hex_dimension_t.t * Hex_cardinal_direction_t.t ;;
exception Precomputed_middler_exn of Hex_dimension_t.t  ;;
exception Precomputed_ender_exn of Hex_dimension_t.t * Hex_cardinal_direction_t.t ;;


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
   
let add_name nm cnnctr = 
   {
     Hex_named_connector_t.name     = nm ;
     entry    = cnnctr.Hex_connector_t.entry ;
     junction = cnnctr.Hex_connector_t.junction ;
     exit     = cnnctr.Hex_connector_t.exit
   };;   

let of_name nm = add_name nm (expand_name nm);; 

let forget_name nc = 
   {
     Hex_connector_t.entry = nc.Hex_named_connector_t.entry ;
     junction = nc.Hex_named_connector_t.junction ;
     exit     = nc.Hex_named_connector_t.exit
   };;   

let all_translates dim nc =
   let nm =nc.Hex_named_connector_t.name 
   and cnnctr = forget_name nc in 
   Image.image (add_name nm) (Hex_connector.all_translates dim cnnctr);;

let expand_all dim cnnctrs = 
    List.flatten( Image.image (fun cnnctr ->
    let nc=of_name cnnctr in all_translates dim nc ) cnnctrs);; 


let to_readable_string nc = Hex_connector_name.to_readable_string nc.Hex_named_connector_t.name;;


let starters_for_side (dim,side)=
    expand_all dim (Hex_connector_name.starters_for_side side);;
    
let middlers dim= 
    expand_all dim  (Hex_connector_name.middlers);;   

let enders_for_side (dim,side)=
    expand_all dim (Hex_connector_name.enders_for_side side);;    
 
module Precomputed = struct 

let usual_range = Image.image (
   fun d -> (Hex_dimension.eleven,d)
) Hex_cardinal_direction.all;; 

let data_for_starters = 
   Image.image (fun (dim,d)->((dim,d),starters_for_side (dim,d)) ) usual_range;;

let data_for_middlers = 
   Image.image (fun dim->(dim,middlers dim) ) [Hex_dimension.eleven];;


let data_for_enders = 
   Image.image (fun (dim,d)->((dim,d),enders_for_side (dim,d)) ) usual_range;;

let starters_for_side dim side=
     match Option.seek (fun p->fst(p)=(dim,side)) data_for_starters with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_starter_exn(dim,side));;
    
let middlers dim=
     match Option.seek (fun p->fst(p)=dim) data_for_middlers with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_middler_exn(dim));;

let enders_for_side dim side=
     match Option.seek (fun p->fst(p)=(dim,side)) data_for_enders with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_ender_exn(dim,side));;
    


end ;;

let inner_sea nc =
   Hex_cell_set.safe_set 
     (Image.image Hex_cell.of_int_pair nc.Hex_named_connector_t.junction) ;;



end ;;



let check_compatiblity end_of_battle nc = 
   let inner_data = Hex_cell_set.forget_order (Private.inner_sea nc) in 
   List.for_all (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell)=Hex_eob_result_t.Unoccupied ) inner_data ;;

let check_disjointness nc1 nc2 =
    Hex_cell_set.does_not_intersect (Private.inner_sea nc1) (Private.inner_sea nc2);; 
   
let check_entry island nc = Hex_connector.check_entry island (Private.forget_name nc);;  

let check_exit nc island = Hex_connector.check_exit island (Private.forget_name nc);;  

let compute_support_in_eyed_claw d1 d2 apex =
    let representative = Private.eyed_claw (d1,d2) in
    let old_junction =  representative.Hex_connector_t.junction in 
    Hex_border_connector_name.compute_support_in_eyed_claw d1 d2 apex old_junction;;

let enders_for_side = Private.Precomputed.enders_for_side ;;

let inner_sea = Private.inner_sea ;;

let middlers = Private.Precomputed.middlers ;;


let print_out (fmt:Format.formatter) nc=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string nc);;     

let starters_for_side = Private.Precomputed.starters_for_side ;;

let to_molecular_linker nc = 
     match Hex_connector_name.to_nondefault_molecular_linker 
             nc.Hex_named_connector_t.name 
              nc.Hex_named_connector_t.junction with 
     None -> Hex_connector.to_default_molecular_linker (Private.forget_name nc)           
    |Some(mlclr) -> mlclr  ;; 
   

let to_readable_string = Private.to_readable_string ;;