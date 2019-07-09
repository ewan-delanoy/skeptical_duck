(* 

#use"Hex_memory/Hex_final_analysis/hex_decisive_configuration.ml";;

*)


let add_move_to_one (cell,player) old_config=
   let beneficiary = old_config.Hex_decisive_configuration_t.beneficiary 
   and active_part = old_config.Hex_decisive_configuration_t.active_part
   and passive_part = old_config.Hex_decisive_configuration_t.passive_part in 
   let (active_found,other_actives)=List.partition (fun cell2->cell2=cell) active_part 
   and (passive_found,other_passives)=List.partition (fun cell2->cell2=cell) passive_part in 
   let compatible=(
   if (active_found=[])&&(passive_found=[])
   then true
   else player=beneficiary    
   ) in 
   if compatible 
   then let new_config={
            Hex_decisive_configuration_t.beneficiary = beneficiary;
            Hex_decisive_configuration_t.active_part = other_actives;
            Hex_decisive_configuration_t.passive_part = other_passives;
        } in 
        Some(new_config)
   else None;;
     
let add_move_to_several move old_configs =
    Option.filter_and_unpack(
      fun (config_idx,current_config)->match 
        add_move_to_one move current_config with 
        None->None
        |Some(next_config)->Some(config_idx,next_config)
    ) old_configs;;
          
let immediate_dangers indexed_configs =
   Option.filter_and_unpack (
       fun (config_idx,config)->
         let l=config.Hex_decisive_configuration_t.active_part in 
         if List.length(l)=1 
         then Some(List.hd l,config_idx)
         else None
   ) indexed_configs;;


let announce_beneficiary ="\nBeneficiary : \n";;
let announce_active_part ="\nActive part : \n";;
let announce_passive_part="\nPassive part : \n";;

(*

let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_first_player) s in 
   let i1=Substring.leftmost_index_of_in announce_second_player s1 in
   let descr1=Cull_string.beginning (i1-1) s1 in
   let descr2=Cull_string.cobeginning (i1+(String.length announce_second_player)-1) s1 in 
   {
     Hex_cigame_memorizer_t.strategies_for_first_player=Hex_cigame_collection.of_string descr1;
     Hex_cigame_memorizer_t.strategies_for_second_player=Hex_cigame_collection.of_string descr2;
   };;

let to_string config=
  let descr1=Hex_cigame_collection.to_string(mmrzr.Hex_cigame_memorizer_t.strategies_for_first_player) 
  and descr2=Hex_cigame_collection.to_string(mmrzr.Hex_cigame_memorizer_t.strategies_for_second_player) 
  and descr3=Hex_cigame_collection.to_string(mmrzr.Hex_cigame_memorizer_t.strategies_for_second_player) in 
  announce_first_player^descr1^announce_second_player^descr2;;

*)
