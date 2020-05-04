(* 

#use"Hex_analysis/hex_starters_for_kite.ml";;

*)



module Private = struct 

let local_cmp = Total_ordering.product 
    Hex_cell_set.length_first_cmp Total_ordering.standard;;

let constructor end_of_battle islands side =
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



let sacrificial_starter end_of_battle (sacrifice_side,cell1,cell2,cell3) side= 
  let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
  let artificial_eob = {
      end_of_battle with 
      Hex_end_of_battle_t.enemy_territory = 
        (Hex_cell_set.merge (Hex_cell_set.safe_set [cell2;cell3]) 
          end_of_battle.Hex_end_of_battle_t.enemy_territory)
  } in 
  let natural_islands = Hex_island.decompose artificial_eob in 
  let artificial_islands = Hex_island.add_and_forget_the_adding dim (sacrifice_side,cell2) natural_islands in 
  constructor artificial_eob artificial_islands side ;; 

 
end ;; 


let nonsacrificial_starters end_of_battle = 
    let islands = Hex_island.decompose end_of_battle in  
    let (side1,side2) = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
    Image.image (Private.constructor end_of_battle islands) [side1;side2] ;;


let sacrificial_starters end_of_battle = 
    let triangles = Hex_end_of_battle.compatible_border_triangles end_of_battle in 
    let (side1,side2) = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
    let base = Cartesian.product triangles [side1;side2] in 
    Image.image (fun (triangle,side) ->
      (triangle, Private.sacrificial_starter end_of_battle triangle side)
    ) base;;

