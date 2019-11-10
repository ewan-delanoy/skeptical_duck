(* 

#use"Hex_analysis/hex_flattened_end_strategy_t.ml";;

*)

module Private = struct

let use_ally_move_to_simplify_one cell old_fles=
   let active_part = old_fles.Hex_flattened_end_strategy_t.active_part
   and passive_part = old_fles.Hex_flattened_end_strategy_t.passive_part in 
   let new_actives=Hex_cell_set.outsert cell active_part 
   and new_passives=Hex_cell_set.outsert cell passive_part in 
   { old_fles with 
            Hex_flattened_end_strategy_t.active_part = new_actives;
            Hex_flattened_end_strategy_t.passive_part = new_passives;
    };;
     
let use_ally_move_to_simplify_several cell old_flesses =
    Image.image(use_ally_move_to_simplify_one cell) old_flesses;;
          
let use_enemy_move_to_simplify_one cell old_fles=
   let active_part = old_fles.Hex_flattened_end_strategy_t.active_part
   and passive_part = old_fles.Hex_flattened_end_strategy_t.passive_part in 
   if (Hex_cell_set.mem cell active_part)||(Hex_cell_set.mem cell passive_part)
   then None
   else Some(old_fles);;
     
let use_enemy_move_to_simplify_several cell old_flesses =
    Option.filter_and_unpack (use_enemy_move_to_simplify_one cell) old_flesses;;

let use_move_to_simplify_one (player,cell) old_fles =
   if player = old_fles.Hex_flattened_end_strategy_t.beneficiary 
   then Some(use_ally_move_to_simplify_one cell old_fles)
   else use_enemy_move_to_simplify_one cell old_fles;;



let immediate_opportunities flesses =
   Option.filter_and_unpack (
       fun fles->
         let l=fles.Hex_flattened_end_strategy_t.active_part in 
         if Hex_cell_set.length(l)=1 
         then let passive_set = fles.Hex_flattened_end_strategy_t.passive_part in 
              let mandatory_set=Hex_cell_set.insert (Hex_cell_set.min l) passive_set in 
               Some(mandatory_set,fles.Hex_flattened_end_strategy_t.index)
         else None
   ) flesses;;

let support fles =
   Hex_cell_set.fold_merge
   [fles.Hex_flattened_end_strategy_t.active_part;
    fles.Hex_flattened_end_strategy_t.passive_part];;

let partial_unveil fles=
  (
     fles.Hex_flattened_end_strategy_t.beneficiary,
     Hex_cell_set.forget_order(fles.Hex_flattened_end_strategy_t.active_part),
     Hex_cell_set.forget_order(fles.Hex_flattened_end_strategy_t.passive_part) 
  );;

let cmp = 
  let cmp_for_cell_lists = Total_ordering.lex_compare Hex_cell.cmp in 
 ((fun fles1 fles2 ->
   (Total_ordering.triple_product 
     Total_ordering.standard
     cmp_for_cell_lists
     cmp_for_cell_lists)
   (partial_unveil fles1) (partial_unveil fles2)  
) :> Hex_flattened_end_strategy_t.t Total_ordering.t);;

let salt = "Hex_"^"flattened_end_strategy_t.";;

let beneficiary_label    = salt ^ "beneficiary";;
let character_label      = salt ^ "character";;
let active_part_label    = salt ^ "active_part";;
let passive_part_label   = salt ^ "passive_part";;
let index_label          = salt ^ "index";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_flattened_end_strategy_t.beneficiary = Hex_player.of_concrete_object (g beneficiary_label);
      character = Hex_strategy_character.of_concrete_object (g character_label);
      active_part = Hex_cell_set.of_concrete_object (g active_part_label);
      passive_part = Hex_cell_set.of_concrete_object (g passive_part_label);
      index = Concrete_object_field.unwrap_int (g index_label);
   };;

let to_concrete_object fles =
 
   Concrete_object_t.Record([
     beneficiary_label,Hex_player.to_concrete_object(fles.Hex_flattened_end_strategy_t.beneficiary);
     character_label, Hex_strategy_character.to_concrete_object(fles.Hex_flattened_end_strategy_t.character);
     active_part_label, Hex_cell_set.to_concrete_object(fles.Hex_flattened_end_strategy_t.active_part);
     passive_part_label, Hex_cell_set.to_concrete_object(fles.Hex_flattened_end_strategy_t.passive_part);
     index_label, Concrete_object_t.Int(fles.Hex_flattened_end_strategy_t.index); 
   ]);;

end;;

let of_concrete_object = Private.of_concrete_object ;;
let immediate_opportunities = Private.immediate_opportunities;;
let support = Private.support;;
let use_ally_move_to_simplify_several = Private.use_ally_move_to_simplify_several;;
let use_enemy_move_to_simplify_several = Private.use_enemy_move_to_simplify_several;;
let use_move_to_simplify_one = Private.use_move_to_simplify_one;;
let to_concrete_object = Private.to_concrete_object ;;

