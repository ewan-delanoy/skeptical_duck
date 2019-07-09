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


