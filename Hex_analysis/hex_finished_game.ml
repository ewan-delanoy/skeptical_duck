(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

exception End_config_is_too_fast of Hex_end_configuration_t.t;;
exception Complaruncbeg_exn of (Hex_player_t.t * Hex_cell_t.t)* Hex_end_configuration_t.t;;

let compute_largest_unconclusive_beginning end_config fgame =
   if List.length(end_config.Hex_end_configuration_t.active_part)<2
   then raise(End_config_is_too_fast(end_config))
   else 
   let temp1= Ennig.index_everything (fgame.Hex_finished_game_t.sequence_of_moves) in 
   let temp2= Image.image (
       fun (k,move)->
         let player=(if k mod 2 = 1 
                     then Hex_player_t.First_player 
                     else Hex_player_t.Second_player )  in 
         (player,move)            
   ) temp1 in 
   let rec tempf=(fun (treated,walker,to_be_treated)->
      match to_be_treated with 
      []->{fgame with Hex_finished_game_t.sequence_of_moves = (List.rev treated)}
      |next_move::other_moves->
       (
         match Hex_end_configuration.use_move_to_simplify_one next_move walker with 
         None->raise(Complaruncbeg_exn(next_move,walker))
         |Some(walker2)->
         if List.length(walker2.Hex_end_configuration_t.active_part)<2
         then {fgame with Hex_finished_game_t.sequence_of_moves = (List.rev treated)}
         else tempf((snd next_move)::treated,walker2,other_moves)

       )
   ) in 
   tempf([],end_config,temp2);;



(*
    
   
   (Hex_end_configuration_t.)

let use_end_config_to_simplify_one end_config fgame=
   let active_part = old_config.Hex_end_configuration_t.active_part
   and passive_part = old_config.Hex_end_configuration_t.passive_part in 
   let new_actives=List.filter (fun cell2->cell2<>cell) active_part 
   and new_passives=List.filter (fun cell2->cell2<>cell) passive_part in 
   { old_config with 
            Hex_end_configuration_t.active_part = new_actives;
            Hex_end_configuration_t.passive_part = new_passives;
    };;
     
let use_ally_move_to_simplify_several cell old_configs =
    Image.image(
      fun (config_idx,current_config)->
       (config_idx,use_ally_move_to_simplify_one cell current_config)
    ) old_configs;;
          
let use_enemy_move_to_simplify_one cell old_config=
   let active_part = old_config.Hex_end_configuration_t.active_part
   and passive_part = old_config.Hex_end_configuration_t.passive_part in 
   if (List.mem cell active_part)&&(List.mem cell passive_part)
   then None
   else Some(old_config);;
     
let use_enemy_move_to_simplify_several cell old_configs =
    Option.filter_and_unpack(
      fun (config_idx,current_config)->match 
        use_enemy_move_to_simplify_one cell current_config with 
        None->None
        |Some(next_config)->Some(config_idx,next_config)
    ) old_configs;;

let immediate_dangers indexed_configs =
   Option.filter_and_unpack (
       fun (config_idx,config)->
         let l=config.Hex_end_configuration_t.active_part in 
         if List.length(l)=1 
         then let mandatory_set=
               Ordered.insert_plaen Hax_cell.cmp 
                (List.hd l) config.Hex_end_configuration_t.passive_part in 
               Some(mandatory_set,config_idx)
         else None
   ) indexed_configs;;


let announce_beneficiary ="\nBeneficiary : \n";;
let announce_active_part ="\nActive part : \n";;
let announce_passive_part="\nPassive part : \n";;

let to_string config=
  let descr1=Hax_player.to_string(config.Hex_end_configuration_t.beneficiary) 
  and descr2=Hax_common.cell_list_to_string(config.Hex_end_configuration_t.active_part) 
  and descr3=Hax_common.cell_list_to_string(config.Hex_end_configuration_t.passive_part) in 
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
     Hex_end_configuration_t.beneficiary=Hax_player.of_string descr1;
     Hex_end_configuration_t.active_part=Hax_common.cell_list_of_string descr2;
     Hex_end_configuration_t.passive_part=Hax_common.cell_list_of_string descr3;
   };;

let unveil config=
  (
     config.Hex_end_configuration_t.beneficiary,
     config.Hex_end_configuration_t.active_part,
     config.Hex_end_configuration_t.passive_part 
  );;

let cmp = 
  let cmp_for_cell_lists = Total_ordering.lex_compare Hax_cell.cmp in 
 ((fun config1 config2 ->
   (Total_ordering.triple_product 
     Total_ordering.standard
     cmp_for_cell_lists
     cmp_for_cell_lists)
   (unveil config1) (unveil config2)  
) :> Hex_end_configuration_t.t Total_ordering.t);;

*)