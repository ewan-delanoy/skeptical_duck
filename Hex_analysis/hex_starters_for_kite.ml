(* 

#use"Hex_analysis/hex_starters_for_kite.ml";;

*)



module Private = struct 

let local_cmp = Total_ordering.product 
    Hex_cell_set.length_first_cmp Total_ordering.standard;;

let helper_for_starter_computation end_of_battle islands side =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let clean = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) in 
   let pre_middle_base = clean (Hex_named_connector.middlers dim)
   and pre_end_base = clean (Hex_named_connector.enders_for_side dim side) in 
   let middle_base = Ordered.sort local_cmp (Image.image (
     fun nc -> (Hex_named_connector.missing_earth end_of_battle nc,nc)
   )  pre_middle_base) 
   and end_base = Ordered.sort local_cmp (Image.image (
     fun nc -> (Hex_named_connector.missing_earth end_of_battle nc,nc)
   )  pre_end_base) in                                
   let first_island = Hex_island.get_side side islands  in   
   let unexpected_starters = List.filter (
       Hex_named_connector.check_entry first_island
   ) pre_middle_base in 
   let base1 = clean (
      (Hex_named_connector.starters_for_side dim side)
      @
      unexpected_starters
      @
      (Hex_named_connector.islanders dim first_island islands)
   ) in         
   let free_ones = Hex_end_of_battle.remaining_free_cells end_of_battle in         
   let constructor = (
      fun first_nc ->
        let new_middle_base=List.filter ( 
         fun (z,other_nc) -> 
         Hex_named_connector.check_disjointness first_nc other_nc ) middle_base
        and new_end_base=List.filter ( 
         fun (z,other_nc) -> 
         Hex_named_connector.check_disjointness first_nc other_nc ) end_base  
        and new_free_ones=Hex_cell_set.setminus  free_ones
          (Hex_named_connector.inner_sea first_nc) in
        {
            Hex_partial_kite_t.place_of_birth = first_island;
            first_step = Hex_kite_element_t.Sea(first_nc);
            stops_so_far =  [];
            unvisited_islands = List.filter (fun x->x<>first_island ) islands;
            unvisited_seas = new_middle_base ;
            unvisited_enders = new_end_base ;
            added_by_casing = Hex_cell_set.empty_set;
            remaining_free_cells = new_free_ones;
        }
   ) in 
   let conditional_constructor = (fun 
     first_nc -> 
        if List.exists (Hex_named_connector.check_exit first_nc) islands
        then Some(constructor first_nc)
        else None 
   ) in 
   Option.filter_and_unpack conditional_constructor  base1 ;; 

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

let sacrificial_starters end_of_battle = 
    let triangles = Hex_end_of_battle.compatible_border_triangles end_of_battle in 
    List.flatten (Image.image (fun triangle ->
      Image.image (fun pk->(triangle,pk)) 
          (Private.sacrificial_starter end_of_battle triangle)
    ) triangles);;

