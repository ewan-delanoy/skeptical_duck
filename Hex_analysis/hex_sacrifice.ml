(* 

#use"Hex_analysis/hex_sacrifice.ml";;

*)

module Private = struct 

let all_sacrifices dim =
   List.flatten (
     Image.image (fun side ->
       Image.image (fun (cell1,cell2,cell3)->
         Hex_sacrifice_t.Scr(side,cell1,cell2,cell3)
       ) (Hex_cardinal_direction.triangles_for_side dim side)
     )
     Hex_cardinal_direction.all
   ) ;;

end ;; 

let compatible_sacrifices eob = 
   let base = Private.all_sacrifices (eob.Hex_end_of_battle_t.dimension) 
   and (side1,side2) = Hex_cardinal_direction.sides_for_player (eob.Hex_end_of_battle_t.winner) in 
   let sides = [side1;side2] in 
   let evl = Hex_end_of_battle.assess eob in 
   List.filter (
      function (Hex_sacrifice_t.Scr(side,cell1,cell2,cell3)) -> 
        (evl cell1 = Hex_eob_result_t.Ally_territory) &&
        (evl cell2 = Hex_eob_result_t.Unoccupied) &&
        (evl cell3 = Hex_eob_result_t.Unoccupied) && 
        (List.mem side sides)
   ) base ;;


let data_for_sacrificial_starter end_of_battle 
  (Hex_sacrifice_t.Scr(sacrifice_side,cell1,cell2,cell3)) = 
  let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
  let artificial_eob = {
      end_of_battle with 
      Hex_end_of_battle_t.enemy_territory = 
        (Hex_cell_set.merge (Hex_cell_set.safe_set [cell2;cell3]) 
          end_of_battle.Hex_end_of_battle_t.enemy_territory)
  } in 
  let natural_islands = Hex_island.decompose artificial_eob in 
  let artificial_islands = Hex_island.add_and_forget_the_adding dim (sacrifice_side,cell2) natural_islands in 
  (artificial_eob,artificial_islands);; 

let reconstruct_sacrificial_solutions scr mlclr=
    match scr with 
    (Hex_sacrifice_t.Scr(side,cell1,cell2,cell3)) -> 
      let pair = Hex_atomic_linker.pair (cell2,cell3) in 
      Hex_molecular_linker.insert pair mlclr ;;

