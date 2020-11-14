(* 

#use"Hex_analysis/hex_composite_pattern.ml";;

*)


let bounds_for_uc dim uc = match uc with 
  (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
     Hex_ipair.bounds_for_authorized_translations dim (Image.image Hex_cell.to_int_pair [cell1;cell2])   
 |Named(nc) ->Hex_named_connector.bounds_for_authorized_translations dim nc ;;
 

let oppflect_uc dim uc = match uc with 
  (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
     Hex_unified_connector_t.Bridge(Hex_player.other_player plyr,(Hex_cell_isometry.oppflect dim cell1,Hex_cell_isometry.oppflect dim cell2))    
 |Named(nc) ->Named (Hex_named_connector.oppose dim (Hex_named_connector.reflect nc));;   


let oppose_uc dim uc = match uc with 
  (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
      Hex_unified_connector_t.Bridge(plyr,(Hex_cell_isometry.oppose dim cell1,Hex_cell_isometry.oppose dim cell2))    
 |Named(nc) ->Named (Hex_named_connector.oppose dim nc);;  

let reflect_uc uc = match uc with 
 (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
    Hex_unified_connector_t.Bridge(Hex_player.other_player plyr,(Hex_cell_isometry.reflect cell1,Hex_cell_isometry.reflect cell2))    
|Named(nc) ->Named (Hex_named_connector.reflect nc);;

let translate_uc dv = function 
 (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
   Hex_unified_connector_t.Bridge(plyr,(Hex_cell_isometry.translate dv cell1,Hex_cell_isometry.translate dv cell2))    
 |Named(nc) ->Named (Hex_named_connector.translate dv nc);;
 

let reflect (Hex_composite_pattern_t.C(patt,uc)) = 
  Hex_composite_pattern_t.C(Hex_pattern.reflect patt,reflect_uc uc) ;;

let oppose dim (Hex_composite_pattern_t.C(patt,uc)) = 
    Hex_composite_pattern_t.C(Hex_pattern.oppose dim patt,oppose_uc dim uc) ;;
    
let oppflect dim (Hex_composite_pattern_t.C(patt,uc)) = 
      Hex_composite_pattern_t.C(Hex_pattern.oppose dim (Hex_pattern.reflect patt),oppflect_uc dim uc) ;;    


let rotate_before_standardizing dim comp_patt =
    let (Hex_composite_pattern_t.C(patt,uc)) = comp_patt in 
    match uc with 
      Hex_unified_connector_t.Bridge(plyr,(cell1,cell2))->
         (
           if plyr = Hex_player_t.Second_player 
           then Hex_composite_pattern_t.C(Hex_pattern.reflect patt,
                Hex_unified_connector_t.Bridge(Hex_player_t.First_player,
                  (Hex_cell_isometry.reflect cell1,Hex_cell_isometry.reflect cell2)) )
          else comp_patt
         )
     |Named(nc)-> 
      (match Hex_named_connector.opt_side nc with 
       None -> comp_patt 
       |Some(side)-> (match side with 
        Hex_cardinal_direction_t.Down -> oppose dim comp_patt 
         |Left -> oppose dim comp_patt 
         |Right -> oppflect dim comp_patt 
         | Up -> comp_patt  
      ))
;;

  
let bounds_for_authorized_translations dim (Hex_composite_pattern_t.C(patt,uc))= 
    let (Hex_pattern_t.Pat l)= patt in 
    let bounds1 = Hex_ipair.bounds_for_authorized_translations dim (Image.image fst l) in 
    let bounds2 = bounds_for_uc dim uc in 
    Rectangle_bounds.combine bounds1 bounds2 ;;


let translate dv (Hex_composite_pattern_t.C(patt,uc)) = 
  Hex_composite_pattern_t.C(Hex_pattern.translate dv patt,translate_uc dv uc) ;;    

let bring_to_left_upper_corner dim comp_patt =
     let (Rectangle_bounds_t.B(xmin,_,ymin,_)) = bounds_for_authorized_translations dim comp_patt in 
     translate (xmin,ymin) comp_patt ;;

let standardize dim comp_patt =  bring_to_left_upper_corner dim (rotate_before_standardizing dim comp_patt);;    



