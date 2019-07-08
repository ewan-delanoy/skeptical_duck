(* 

#use"Hex_memory/Hex_final_analysis/hex_decisive_configuration.ml";;

*)

(*
let mix_with_one unchecked_game old_config=
   let (Hex_decisive_configuration_t.DC(player,l_config))=old_config in 
   let accu_for_missing_items=ref[]
   and accu_for_breakers=ref[] in 
   let _=List.iter (
     fun (cell,active_or_passive)->
       match Hex_unchecked_game.get unchecked_game cell with 
       None->if active_or_passive=Hex_active_or_passive_t.Active
             then accu_for_missing_items:=cell::(!accu_for_missing_items)
       |Some(plyr)->
           if plyr<>player 
           then accu_for_breakers:=cell::(!accu_for_breakers) 
   ) l_config in 
   if (!accu_for_breakers)<>[]
   then None 
   else 
   let new_l_config=List.filter 
     (fun (cell,_)->List.mem cell (!accu_for_missing_items) ) l_config in 
   Some(Hex_decisive_configuration_t.DC(player,new_l_config));;
     
let mix_with_several unchecked_game old_configs =
    Option.filter_and_unpack(
      fun (config_idx,current_config)->match 
        mix_with_one unchecked_game current_config with 
        None->None
        |Some(next_config)->Some(config_idx,next_config)
    ) old_configs;;
          
let immediate_dangers unchecked_game configs =
   let temp1=mix_with_several unchecked_game configs in 
   Option.filter_and_unpack (
       fun (config_idx,Hex_decisive_configuration_t.DC(_,l))->
         if List.length(l)=1 
         then Some(fst(List.hd l),config_idx)
         else None
   ) temp1;;
*)

