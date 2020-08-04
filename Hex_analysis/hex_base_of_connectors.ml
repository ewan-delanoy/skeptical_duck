(* 

#use"Hex_analysis/hex_base_of_connectors.ml";;

*)

let from_end_of_battle end_of_battle =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension 
   and winner = end_of_battle.Hex_end_of_battle_t.winner in
   let (side1,side2) =  Hex_cardinal_direction.sides_for_player winner in 
   let pre_base = List.concat 
      [   
      (Hex_named_connector.starters_for_side dim side1);
      (Hex_named_connector.starters_for_side dim side2);
      (Hex_named_connector.middlers dim);
      (Hex_named_connector.enders_for_side dim side1);
      (Hex_named_connector.enders_for_side dim side2);
      ] in 
   let base = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) pre_base in 
   Hex_base_of_connectors_t.B( Image.image (
      fun nc->
        let (entry_part,exit_part) = Hex_named_connector.missing_pods end_of_battle nc in 
        (nc,entry_part,exit_part)
   ) base );;
 
