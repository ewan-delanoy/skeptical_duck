(* 

#use"Hex_analysis/hex_flattened_end_strategy.ml";;

*)

module Private = struct


let use_ally_move_to_simplify_one cell old_fles=
    {
       old_fles with 
       Hex_flattened_end_strategy_t.data = 
         Hex_extended_molecular.use_ally_move_to_simplify_one cell 
          (old_fles.Hex_flattened_end_strategy_t.data)
    };;
  
     
let use_ally_move_to_simplify_several cell old_flesses =
    Image.vorstellung(use_ally_move_to_simplify_one cell) old_flesses;;
          
let use_enemy_move_to_simplify_one cell old_fles=
   let old_data =old_fles.Hex_flattened_end_strategy_t.data in 
   match Hex_extended_molecular.use_enemy_move_to_simplify_one cell old_data with 
   None -> None 
   |Some(new_data) ->
   Some({
       old_fles with 
       Hex_flattened_end_strategy_t.data = new_data
    });;
     
let use_enemy_move_to_simplify_several cell old_flesses =
    Option.filter_and_unpack (use_enemy_move_to_simplify_one cell) old_flesses;;

let use_move_to_simplify_one (player,cell) old_fles =
   if player = Hex_flattened_end_strategy_field.beneficiary  old_fles
   then Some(use_ally_move_to_simplify_one cell old_fles)
   else use_enemy_move_to_simplify_one cell old_fles;;

let immediate_opportunities flesses =
   let temp1 = Option.filter_and_unpack (
       fun fles->
         let l=Hex_flattened_end_strategy_field.active_part fles in 
         if Hex_cell_set.length(l)=1 
         then let passive_set = Hex_flattened_end_strategy_field.passive_part fles in 
              let m = Hex_cell_set.min l in 
              let mandatory_set=Hex_cell_set.insert m passive_set in 
               Some(fles,mandatory_set,m)
         else None
   ) flesses in 
   let cells = Image.vorstellung (fun (_,_,cell)->cell) temp1 in 
   let older_extmols = Image.vorstellung (fun (fles,_,_)->fles.Hex_flattened_end_strategy_t.data) temp1 in
   let interesting_indices = Image.vorstellung (fun (fles,_,_)->Hex_flattened_end_strategy_field.index fles) temp1 in 
   let mand = Hex_mandatory_compound.escape_compound_in_disjunction cells older_extmols in 
   (cells,interesting_indices,mand);;

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


