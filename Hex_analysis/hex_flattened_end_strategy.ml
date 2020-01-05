(* 

#use"Hex_analysis/hex_flattened_end_strategy_t.ml";;

*)

module Private = struct

let use_ally_move_to_simplify_one cell old_fles=
   let active_part = Hex_flattened_end_strategy_field.active_part old_fles
   and passive_part = Hex_flattened_end_strategy_field.passive_part old_fles in 
   let new_actives=Hex_cell_set.outsert cell active_part 
   and new_passives=Hex_cell_set.outsert cell passive_part in 
   Hex_flattened_end_strategy_field.change_parts 
      old_fles (new_actives,new_passives);;
  
     
let use_ally_move_to_simplify_several cell old_flesses =
    Image.image(use_ally_move_to_simplify_one cell) old_flesses;;
          
let use_enemy_move_to_simplify_one cell old_fles=
   let active_part = Hex_flattened_end_strategy_field.active_part old_fles
   and passive_part = Hex_flattened_end_strategy_field.passive_part old_fles in 
   if (Hex_cell_set.mem cell active_part)||(Hex_cell_set.mem cell passive_part)
   then None
   else Some(old_fles);;
     
let use_enemy_move_to_simplify_several cell old_flesses =
    Option.filter_and_unpack (use_enemy_move_to_simplify_one cell) old_flesses;;

let use_move_to_simplify_one (player,cell) old_fles =
   if player = Hex_flattened_end_strategy_field.beneficiary  old_fles
   then Some(use_ally_move_to_simplify_one cell old_fles)
   else use_enemy_move_to_simplify_one cell old_fles;;



let immediate_opportunities flesses =
   Option.filter_and_unpack (
       fun fles->
         let l=Hex_flattened_end_strategy_field.active_part fles in 
         if Hex_cell_set.length(l)=1 
         then let passive_set = Hex_flattened_end_strategy_field.passive_part fles in 
              let mandatory_set=Hex_cell_set.insert (Hex_cell_set.min l) passive_set in 
               Some(mandatory_set,Hex_flattened_end_strategy_field.index fles)
         else None
   ) flesses;;

let support fles =
   Hex_cell_set.fold_merge
   [Hex_flattened_end_strategy_field.active_part fles;
    Hex_flattened_end_strategy_field.passive_part fles];;

end;;


let immediate_opportunities = Private.immediate_opportunities;;
let support = Private.support;;
let use_ally_move_to_simplify_several = Private.use_ally_move_to_simplify_several;;
let use_enemy_move_to_simplify_several = Private.use_enemy_move_to_simplify_several;;
let use_move_to_simplify_one = Private.use_move_to_simplify_one;;


