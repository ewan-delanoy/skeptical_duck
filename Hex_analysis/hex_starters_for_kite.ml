(* 

#use"Hex_analysis/hex_starters_for_kite.ml";;

*)



module Private = struct 

let local_cmp = Total_ordering.product 
    Hex_cell_set.length_first_cmp Total_ordering.standard;;

let compute_initial_seeds end_of_battle islands side =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let clean = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) in 
   let pre_middle_base = clean (Hex_named_connector.middlers dim)
   and pre_end_base = clean (Hex_named_connector.enders_for_side dim side) in                             
   let first_island = Hex_island.get_side side islands  in   
   let unexpected_starters = List.filter (
       Hex_named_connector.check_entry first_island
   ) pre_middle_base in 
   let start_base = clean (
      (Hex_named_connector.starters_for_side dim side)
      @
      unexpected_starters
      @
      (Hex_named_connector.islanders dim first_island islands)
   ) in 
   let full_base = Ordered.sort local_cmp (Image.image (
     fun nc -> (Hex_named_connector.missing_earth end_of_battle nc,nc)
   )  (start_base@pre_middle_base@pre_end_base) ) in            
   let free_ones = Hex_end_of_battle.remaining_free_cells end_of_battle in    
   Hex_partial_kite_field.constructor first_island islands (full_base) free_ones ;;

let compute_initial_seeds_in_nonprudent_case end_of_battle islands side =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let clean = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) in 
   let pre_middle_base = clean (Hex_named_connector.middlers dim)
   and pre_end_base = clean (Hex_named_connector.enders_for_side dim side) in                             
   let first_island = Hex_island.get_side side islands  in   
   let unexpected_starters = List.filter (
       Hex_named_connector.check_entry first_island
   ) pre_middle_base in 
   let start_base = clean (
      (Hex_named_connector.starters_for_side dim side)
      @
      unexpected_starters
      @
      (Hex_named_connector.islanders dim first_island islands)
   ) in 
   let full_base = Ordered.sort local_cmp (Image.image (
     fun nc -> (Hex_named_connector.missing_earth end_of_battle nc,nc)
   )  (pre_middle_base@pre_end_base) ) in            
   let free_ones = Hex_end_of_battle.remaining_free_cells end_of_battle in    
   (Hex_partial_kite_field.constructor first_island islands (full_base) free_ones,start_base) ;;



let helper_for_starter_computation end_of_battle islands side =
   let (seed,base) = compute_initial_seeds_in_nonprudent_case end_of_battle islands side in   
   let conditional_constructor = (fun 
     first_nc -> 
        if List.exists (Hex_named_connector.check_exit first_nc) islands
        then Some(snd(Hex_partial_kite_field.extend_with_sea seed first_nc))
        else None 
   ) in 
   Option.filter_and_unpack conditional_constructor  base ;; 

let nonsacrificial_starters_for_side end_of_battle side =
   let islands = Hex_island.decompose end_of_battle in  
   helper_for_starter_computation end_of_battle islands side ;; 



let sacrificial_starter end_of_battle (side,cell1,cell2,cell3) = 
  let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
  let artificial_eob = {
      end_of_battle with 
      Hex_end_of_battle_t.enemy_territory = 
        (Hex_cell_set.merge (Hex_cell_set.safe_set [cell2;cell3]) end_of_battle.Hex_end_of_battle_t.enemy_territory)
  } in 
  let natural_islands = Hex_island.decompose artificial_eob in 
  let artificial_islands = Hex_island.add_and_forget_the_adding dim (side,cell2) natural_islands in 
  let opposite_side = Hex_cardinal_direction.oppose side in 
  (helper_for_starter_computation artificial_eob artificial_islands side)@
  (helper_for_starter_computation artificial_eob artificial_islands opposite_side);; 

end ;; 

let nonsacrificial_starters end_of_battle = 
   let (side1,side2) = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   List.flatten (Image.image (Private.nonsacrificial_starters_for_side end_of_battle) [side1;side2]);;

let prudent_nonsacrificial_starters end_of_battle = 
    let islands = Hex_island.decompose end_of_battle in  
    let (side1,side2) = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
    Image.image (Private.compute_initial_seeds end_of_battle islands) [side1;side2] ;;


let sacrificial_starters end_of_battle = 
    let triangles = Hex_end_of_battle.compatible_border_triangles end_of_battle in 
    List.flatten (Image.image (fun triangle ->
      Image.image (fun pk->(triangle,pk)) 
          (Private.sacrificial_starter end_of_battle triangle)
    ) triangles);;

