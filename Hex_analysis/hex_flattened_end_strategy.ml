(* 

#use"Hex_analysis/hex_flattened_end_strategy_t.ml";;

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

let neuop flesses =
   let temp1 = Option.filter_and_unpack (
       fun fles->
         let l=Hex_flattened_end_strategy_field.active_part fles in 
         if Hex_cell_set.length(l)=1 
         then let passive_set = Hex_flattened_end_strategy_field.passive_part fles in 
              let mandatory_set=Hex_cell_set.insert (Hex_cell_set.min l) passive_set in 
               Some(fles,mandatory_set)
         else None
   ) flesses in 
   let interesting_indices = Image.image (fun (fles,_)->Hex_flattened_end_strategy_field.index fles) temp1 in 
   let temp2 = Image.image (fun (fles,_)->fles.Hex_flattened_end_strategy_t.data) temp1 in
   let main = Hex_extended_molecular.disjunction temp2 in  
   let condition =(if temp1=[] then None else 
     Some(Hex_cell_set.fold_intersect (Image.image snd temp1))
   ) in 
   (interesting_indices,Hex_mandatory_set.of_extended_molecular main,condition);;

let support fles =
   Hex_cell_set.fold_merge
   [Hex_flattened_end_strategy_field.active_part fles;
    Hex_flattened_end_strategy_field.passive_part fles];;

end;;


let immediate_opportunities = Private.immediate_opportunities;;
let neuop = Private.neuop;;
let support = Private.support;;
let use_ally_move_to_simplify_several = Private.use_ally_move_to_simplify_several;;
let use_enemy_move_to_simplify_several = Private.use_enemy_move_to_simplify_several;;
let use_move_to_simplify_one = Private.use_move_to_simplify_one;;


