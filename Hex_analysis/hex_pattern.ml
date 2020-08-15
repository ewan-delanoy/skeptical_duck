(* 

#use"Hex_analysis/hex_pattern.ml";;

*)


module Private = struct 

let allowed_range_for_translation formal_dim (Hex_pattern_t.Pat(l)) =
  let cells = Image.image (fun (pair,_)->Hex_cell.of_int_pair pair) l in 
  let ((xmin,xmax),(ymin,ymax)) = 
    Hex_cell.allowed_range_for_translation_of_list formal_dim cells in 
  Cartesian.product (Ennig.ennig xmin xmax) (Ennig.ennig ymin ymax);;

let translate (Hex_pattern_t.Pat(l)) (dx,dy) = 
  Hex_pattern_t.Pat(
    Image.image (
       fun ((x,y),color) ->
         ((x+dx,y+dy),color)
    ) l
  ) ;;    

let all_translates formal_dim patt = 
   Image.image (translate patt) (allowed_range_for_translation formal_dim patt);;

let individual_test_for_match eob ((x,y),is_active) =
   let res = Hex_end_of_battle.assess eob (Hex_cell.of_int_pair (x,y)) 
   and expected_res = (
       if is_active 
       then Hex_eob_result_t.Ally_territory
       else Hex_eob_result_t.Unoccupied
   ) in 
   res = expected_res ;;  
 
let test_for_match eob (Hex_pattern_t.Pat(l))= 
   List.for_all (individual_test_for_match eob) l ;;

let occurrences_of_in patt eob =
  let patts = all_translates (eob.Hex_end_of_battle_t.dimension) patt in 
  List.filter (test_for_match eob) patts ;; 


end ;;

let  occurrences_of_in = Private.occurrences_of_in ;;
