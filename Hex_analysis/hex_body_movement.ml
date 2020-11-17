(* 

#use"Hex_analysis/hex_body_movement.ml";;

*)

exception Between_ucs_exn of Hex_unified_connector_t.t * Hex_unified_connector_t.t ;; 

module Private = struct

let side_of_inner_connector = function 
  Hex_inner_connector_name_t.Broken_bridge(_,_,_,_) -> Hex_cardinal_direction_t.Up
 |Typical(_,side)-> side ;;  

let side_of_border_connector = function 
   Hex_border_connector_name_t.Eyed_claw(specifier,side) -> side
  |Typical(tic,side) -> side ;;  

let side_of_connector nc = match nc.Hex_named_connector_t.name with  
    Hex_connector_name_t.Inner(inner) -> side_of_inner_connector inner 
   |Border(border) -> side_of_border_connector  border ;;

let forced_apex nc = match nc.Hex_named_connector_t.apex with 
   Some(natural_apex) -> natural_apex 
   | None -> List.hd (nc.Hex_named_connector_t.junction);;

let side_and_apex nc = (side_of_connector nc,forced_apex nc) ;;

let seek_translation_for_pair  (dim,refl,opp,cell1,cell2,cell3,cell4)=
   let (x1,y1) = Hex_cell.to_int_pair cell1 
   and (x2,y2) = Hex_cell.to_int_pair cell2 
   and (x3,y3) = Hex_cell.to_int_pair cell3 
   and (x4,y4) = Hex_cell.to_int_pair cell4 in 
   let dx=x3-x1 and dy=y3-y1 in 
   if (x2+dx=x4) && (y2+dy=y4) 
   then  Some ({Hex_body_movement_t.dimension = dim;
                reflect = refl;
                oppose = opp;
                translation = (dx,dy)
         })
   else  None;;

let apply_on_ipair mv ipair=
   let ipair2 = (if mv.Hex_body_movement_t.reflect 
                then Hex_ipair.reflect ipair
                else ipair) in 
    if mv.Hex_body_movement_t.oppose 
    then Hex_ipair.oppose mv.Hex_body_movement_t.dimension ipair2 
    else ipair2 ;;     

let apply_on_named_connector mv nc=
    let nc2 = (if mv.Hex_body_movement_t.reflect 
                 then Hex_named_connector.reflect nc
                 else nc) in 
     if mv.Hex_body_movement_t.oppose 
     then Hex_named_connector.oppose mv.Hex_body_movement_t.dimension nc2 
     else nc2 ;;     

let apply_on_side mv side=
   let side2 = (if mv.Hex_body_movement_t.reflect 
                then Hex_cardinal_direction.reflect side 
                else side) in 
    if mv.Hex_body_movement_t.oppose 
    then Hex_cardinal_direction.oppose side2 
    else side2 ;;              

let apply_on_pattern mv (Hex_pattern_t.Pat l) =
     Hex_pattern_t.Pat (Image.image (fun (pair,lbl)->(apply_on_ipair mv pair,lbl)) l);;       

let nontranslated (dim,refl,opp) = 
     {Hex_body_movement_t.dimension = dim;
      reflect = refl;
      oppose = opp;
      translation = (0,0)
     } ;; 

let all_nontranslated = 
     let d = Hex_dimension.eleven in 
     Image.image nontranslated 
   [d,false,false;d,false,true;d,true,false;d,true,true];;

let between_sides side1 side2 = 
  Listennou.force_find
  (fun mv-> apply_on_side mv side1 = side2) 
  all_nontranslated
;;

let between_bridges dim different_owners (cell1,cell2) (cell3,cell4) = 
     let cell5 = Hex_cell_isometry.oppose dim cell3
     and cell6 = Hex_cell_isometry.oppose dim cell4 in 
     Option.filter_and_unpack (
          seek_translation_for_pair 
     ) [
          (dim,different_owners,false,cell1,cell2,cell3,cell4);
          (dim,different_owners,false,cell1,cell2,cell4,cell3);  
          (dim,different_owners,true,cell1,cell2,cell5,cell6);  
          (dim,different_owners,true,cell1,cell2,cell6,cell5);  
       ];;



let between_owned_bridges dim ((plyr1:Hex_player_t.t),cell1,cell2) (plyr2,cell3,cell4) = 
     if plyr1=plyr2
     then  between_bridges dim false (cell1,cell2) (cell3,cell4)
     else  between_bridges dim true  (cell1,cell2) (Hex_cell_isometry.reflect cell3,Hex_cell_isometry.reflect cell4);;

let between_named_connectors dim nc1 nc2 = 
     let (side1,apex1) = side_and_apex nc1 
     and (side2,apex2) = side_and_apex nc2 in 
     let mv1 = between_sides side1 side2 in 
     let (x2,y2)=apex2
     and (x3,y3)=apply_on_ipair mv1 apex1 in 
     let mv2 = {mv1 with Hex_body_movement_t.translation=(x2-x3,y2-y3)} in 
     if (apply_on_named_connector mv2 nc1) = nc2 
     then [mv2]      
     else [];;

let between_ucs dim uc1 uc2= match uc1 with  
   Hex_unified_connector_t.Bridge((_,plyr1),(cell1,cell2))->
       (
           match uc2 with 
           Hex_unified_connector_t.Bridge((_,plyr2),(cell3,cell4)) -> 
                   between_owned_bridges dim (plyr1,cell1,cell2) (plyr2,cell3,cell4) 
           |Named(nc2) -> raise(Between_ucs_exn(uc1,uc2))        
       )
   |Named(nc1) ->    
       (
          match uc2 with 
          Hex_unified_connector_t.Bridge(plyr2,(cell3,cell4)) -> 
               raise(Between_ucs_exn(uc1,uc2))
                  
          |Named(nc2) -> between_named_connectors dim nc1 nc2  
       );;

     
end ;;

let apply_on_pattern = Private.apply_on_pattern ;;
let between_unified_connectors = Private.between_ucs ;;