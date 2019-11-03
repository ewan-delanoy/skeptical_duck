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

let partial_unveil fles=
  (
     fles.Hex_flattened_end_strategy_t.beneficiary,
     Hex_cell_set.unveil(fles.Hex_flattened_end_strategy_t.active_part),
     Hex_cell_set.unveil(fles.Hex_flattened_end_strategy_t.passive_part) 
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
let use_ally_move_to_simplify_several = Private.use_ally_move_to_simplify_several;;
let use_enemy_move_to_simplify_several = Private.use_enemy_move_to_simplify_several;;
let use_move_to_simplify_one = Private.use_move_to_simplify_one;;
let to_concrete_object = Private.to_concrete_object ;;

(*

let announce_beneficiary ="\nBeneficiary : \n";;
let announce_active_part ="\nActive part : \n";;
let announce_passive_part="\nPassive part : \n";;
let announce_index="\nIndex : \n";;

let to_string fles=
  let descr1=Hex_player.to_string(fles.Hex_flattened_end_strategy_t.beneficiary) 
  and descr2=Hex_common.cell_list_to_string(Hex_cell_set.unveil(fles.Hex_flattened_end_strategy_t.active_part)) 
  and descr3=Hex_common.cell_list_to_string(Hex_cell_set.unveil(fles.Hex_flattened_end_strategy_t.passive_part)) 
  and descr4=string_of_int(fles.Hex_flattened_end_strategy_t.index) in 
  announce_beneficiary^descr1^
  announce_active_part^descr2^
  announce_passive_part^descr3^
  announce_index^descr4;;


let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_beneficiary) s in 
   let i1=Substring.leftmost_index_of_in announce_active_part s1 
   and i2=Substring.leftmost_index_of_in announce_passive_part s1 
   and i3=Substring.leftmost_index_of_in announce_index s1 in
   let j1=i1+(String.length announce_active_part)-1
   and j2=i2+(String.length announce_passive_part)-1 
   and j3=i3+(String.length announce_index)-1 in 
   let descr1=Cull_string.interval s1 1 (i1-1) 
   and descr2=Cull_string.interval s1 (j1+1) (i2-1) 
   and descr3=Cull_string.interval s1 (j2+1) (i3-1) 
   and descr4=Cull_string.interval s1  (j3+1) (String.length s1) in 
   {
     Hex_flattened_end_strategy_t.beneficiary=Hex_player.of_string descr1;
     Hex_flattened_end_strategy_t.active_part=Hex_cell_set.safe_set(Hex_common.cell_list_of_string descr2);
     Hex_flattened_end_strategy_t.passive_part=Hex_cell_set.safe_set(Hex_common.cell_list_of_string descr3);
     Hex_flattened_end_strategy_t.index=int_of_string descr4;
   };;

*)

