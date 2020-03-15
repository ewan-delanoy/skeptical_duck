(* 

#use"Hex_analysis/hex_starters_for_kite.ml";;

*)



module Private = struct 

let local_cmp = Total_ordering.product 
    Hex_cell_set.length_first_cmp Total_ordering.standard;;

let starters_for_side end_of_battle side =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let clean = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) in 
   let base1 = clean (Hex_named_connector.starters_for_side dim side)
   and pre_base2 = clean ((Hex_named_connector.middlers dim)@
                       (Hex_named_connector.enders_for_side dim side)
                      ) in 
   let unordered_base2 = Image.image (
     fun nc -> (Hex_named_connector.missing_earth end_of_battle nc,nc)
   )  pre_base2 in                  
   let base2 = Ordered.sort local_cmp unordered_base2 in                    
   let islands = Hex_island.decompose end_of_battle in 
   let first_island = Hex_island.get_side side islands  in                    
   let constructor = (
      fun first_nc ->
        let new_base=List.filter ( 
         fun (z,other_nc) -> 
         Hex_named_connector.check_disjointness first_nc other_nc ) base2 in
        let elt2 = Hex_kite_element_t.Sea(first_nc) in 
        {
            Hex_partial_kite_t.place_of_birth = first_island;
            first_step = elt2;
            stops_so_far =  [];
            original_side = side ;
            unvisited_islands = List.filter (fun x->x<>first_island ) islands;
            unvisited_seas = new_base ;
            added_by_casing = Hex_cell_set.empty_set;
        }
   ) in 
   let conditional_constructor = (fun 
     first_nc -> 
        if List.exists (Hex_named_connector.check_exit first_nc) islands
        then Some(constructor first_nc)
        else None 
   ) in 
   Option.filter_and_unpack conditional_constructor  base1 ;; 

end ;; 

let starters end_of_battle = 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   List.flatten (Image.image (Private.starters_for_side end_of_battle) sides);;


