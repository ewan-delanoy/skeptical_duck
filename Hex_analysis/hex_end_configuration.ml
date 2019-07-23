(* 

#use"Hex_analysis/hex_end_configuration.ml";;

*)


let use_ally_move_to_simplify_one cell old_config=
   let active_part = old_config.Hex_end_configuration_t.active_part
   and passive_part = old_config.Hex_end_configuration_t.passive_part in 
   let new_actives=Hex_cell_set.outsert cell active_part 
   and new_passives=Hex_cell_set.outsert cell passive_part in 
   { old_config with 
            Hex_end_configuration_t.active_part = new_actives;
            Hex_end_configuration_t.passive_part = new_passives;
    };;
     
let use_ally_move_to_simplify_several cell old_configs =
    Image.image(use_ally_move_to_simplify_one cell) old_configs;;
          
let use_enemy_move_to_simplify_one cell old_config=
   let active_part = old_config.Hex_end_configuration_t.active_part
   and passive_part = old_config.Hex_end_configuration_t.passive_part in 
   if (Hex_cell_set.mem cell active_part)||(Hex_cell_set.mem cell passive_part)
   then None
   else Some(old_config);;
     
let use_enemy_move_to_simplify_several cell old_configs =
    Option.filter_and_unpack (use_enemy_move_to_simplify_one cell) old_configs;;

let use_move_to_simplify_one (player,cell) old_config =
   if player = old_config.Hex_end_configuration_t.beneficiary 
   then Some(use_ally_move_to_simplify_one cell old_config)
   else use_enemy_move_to_simplify_one cell old_config;;


let immediate_dangers configs =
   Option.filter_and_unpack (
       fun config->
         let l=config.Hex_end_configuration_t.active_part in 
         if Hex_cell_set.length(l)=1 
         then let passive_set = config.Hex_end_configuration_t.passive_part in 
              let mandatory_set=Hex_cell_set.insert (Hex_cell_set.min l) passive_set in 
               Some(mandatory_set,config.Hex_end_configuration_t.index)
         else None
   ) configs;;



let announce_beneficiary ="\nBeneficiary : \n";;
let announce_active_part ="\nActive part : \n";;
let announce_passive_part="\nPassive part : \n";;
let announce_index="\nIndex : \n";;

let to_string config=
  let descr1=Hex_player.to_string(config.Hex_end_configuration_t.beneficiary) 
  and descr2=Hex_common.cell_list_to_string(Hex_cell_set.unveil(config.Hex_end_configuration_t.active_part)) 
  and descr3=Hex_common.cell_list_to_string(Hex_cell_set.unveil(config.Hex_end_configuration_t.passive_part)) 
  and descr4=string_of_int(config.Hex_end_configuration_t.index) in 
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
     Hex_end_configuration_t.beneficiary=Hex_player.of_string descr1;
     Hex_end_configuration_t.active_part=Hex_cell_set.safe_set(Hex_common.cell_list_of_string descr2);
     Hex_end_configuration_t.passive_part=Hex_cell_set.safe_set(Hex_common.cell_list_of_string descr3);
     Hex_end_configuration_t.index=int_of_string descr4;
   };;

let partial_unveil config=
  (
     config.Hex_end_configuration_t.beneficiary,
     Hex_cell_set.unveil(config.Hex_end_configuration_t.active_part),
     Hex_cell_set.unveil(config.Hex_end_configuration_t.passive_part) 
  );;

let cmp = 
  let cmp_for_cell_lists = Total_ordering.lex_compare Hex_cell.cmp in 
 ((fun config1 config2 ->
   (Total_ordering.triple_product 
     Total_ordering.standard
     cmp_for_cell_lists
     cmp_for_cell_lists)
   (partial_unveil config1) (partial_unveil config2)  
) :> Hex_end_configuration_t.t Total_ordering.t);;

let insert_carefully ec l=
  let reindexed_ec = {
     ec with 
     Hex_end_configuration_t.index=(List.length(l))+1
  } in 
  Ordered.insert_plaen cmp reindexed_ec l;;





