(* 

#use"Hex_memory/Hex_final_analysis/hex_decisive_configuration.ml";;

*)


let add_ally_move_to_one cell old_config=
   let active_part = old_config.Hax_decisive_configuration_t.active_part
   and passive_part = old_config.Hax_decisive_configuration_t.passive_part in 
   let new_actives=List.filter (fun cell2->cell2<>cell) active_part 
   and new_passives=List.filter (fun cell2->cell2<>cell) passive_part in 
   { old_config with 
            Hax_decisive_configuration_t.active_part = new_actives;
            Hax_decisive_configuration_t.passive_part = new_passives;
    };;
     
let add_ally_move_to_several cell old_configs =
    Image.image(
      fun (config_idx,current_config)->
       (config_idx,add_ally_move_to_one cell current_config)
    ) old_configs;;
          
let add_enemy_move_to_one cell old_config=
   let active_part = old_config.Hax_decisive_configuration_t.active_part
   and passive_part = old_config.Hax_decisive_configuration_t.passive_part in 
   if (List.mem cell active_part)&&(List.mem cell passive_part)
   then None
   else Some(old_config);;
     
let add_enemy_move_to_several cell old_configs =
    Option.filter_and_unpack(
      fun (config_idx,current_config)->match 
        add_enemy_move_to_one cell current_config with 
        None->None
        |Some(next_config)->Some(config_idx,next_config)
    ) old_configs;;

let immediate_dangers indexed_configs =
   Option.filter_and_unpack (
       fun (config_idx,config)->
         let l=config.Hax_decisive_configuration_t.active_part in 
         if List.length(l)=1 
         then let mandatory_set=
               Ordered.insert_plaen Hex_cell.cmp 
                (List.hd l) config.Hax_decisive_configuration_t.passive_part in 
               Some(mandatory_set,config_idx)
         else None
   ) indexed_configs;;


let announce_beneficiary ="\nBeneficiary : \n";;
let announce_active_part ="\nActive part : \n";;
let announce_passive_part="\nPassive part : \n";;

let to_string config=
  let descr1=Hax_player.to_string(config.Hax_decisive_configuration_t.beneficiary) 
  and descr2=Hax_common.cell_list_to_string(config.Hax_decisive_configuration_t.active_part) 
  and descr3=Hax_common.cell_list_to_string(config.Hax_decisive_configuration_t.passive_part) in 
  announce_beneficiary^descr1^announce_active_part^descr2^announce_passive_part^descr3;;

let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_beneficiary) s in 
   let i1=Substring.leftmost_index_of_in announce_active_part s1 in
   let i2=Substring.leftmost_index_of_in announce_passive_part s1 in
   let j1=i1+(String.length announce_active_part)-1
   and j2=i2+(String.length announce_passive_part)-1 in 
   let descr1=Cull_string.interval s1 1 (i1-1) 
   and descr2=Cull_string.interval s1 (j1+1) (i2-1) 
   and descr3=Cull_string.interval s1  (j2+1) (String.length s1) in 
   {
     Hax_decisive_configuration_t.beneficiary=Hax_player.of_string descr1;
     Hax_decisive_configuration_t.active_part=Hax_common.cell_list_of_string descr2;
     Hax_decisive_configuration_t.passive_part=Hax_common.cell_list_of_string descr3;
   };;

let unveil config=
  (
     config.Hax_decisive_configuration_t.beneficiary,
     config.Hax_decisive_configuration_t.active_part,
     config.Hax_decisive_configuration_t.passive_part 
  );;

let cmp = 
  let cmp_for_cell_lists = Total_ordering.lex_compare Hex_cell.cmp in 
 ((fun config1 config2 ->
   (Total_ordering.triple_product 
     Total_ordering.standard
     cmp_for_cell_lists
     cmp_for_cell_lists)
   (unveil config1) (unveil config2)  
) :> Hax_decisive_configuration_t.t Total_ordering.t);;
