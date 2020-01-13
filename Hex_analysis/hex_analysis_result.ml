(* 

#use"Hex_analysis/hex_analysis_result.ml";;

*)


module Private = struct

let report_on_danger res=
    let mand = res.Hex_analysis_result_t.mandatory_set in 
    match mand with 
     Hex_mandatory_set_t.No_constraint->""
    |_->"Danger, because of "^
                (Strung.of_intlist res.Hex_analysis_result_t.dangerous_enemy_strategies)^
                " : play in "^(Hex_mandatory_set.explain(mand))^"\n";; 

let explanation_for_familiar_move other_possible_moves =
   if other_possible_moves = []
   then "familiar move"
   else let temp1 = Image.image Hex_cell.to_string other_possible_moves in 
        "or "^(String.concat "," temp1)^" : familiar moves" ;;

let explanation_for_move res =
    match res.Hex_analysis_result_t.completion_for_strong_move with 
    Some(forcing_possible,expected_seq)->
        if (not forcing_possible) 
        then (false,"prudent move")
        else 
        if expected_seq = []
        then (false,"forcing move")
        else let temp1 = Image.image Hex_cell.to_string expected_seq in 
             let s_remaining = String.concat "," temp1 in 
             (false,"expecting "^s_remaining)
    |None -> let (Hex_cell_set_t.S l)=res.Hex_analysis_result_t.familiar_moves in 
              match l with 
               []->(true,"")
              | _ :: others->(false,explanation_for_familiar_move others);;    
   
let report_on_chosen_move res=
     let (expl_not_useful,expl) = explanation_for_move res  in 
     if expl_not_useful then "" else 
     let plyr = Hex_player.color (res.Hex_analysis_result_t.next_to_play) 
     and s_move = Hex_cell.to_string (res.Hex_analysis_result_t.chosen_move) in 
     "Suggestion for "^plyr^" : "^s_move^" ("^expl^")\n" ;;  

let report_on_enemies res = 
   let plyr = Hex_player.color (res.Hex_analysis_result_t.next_to_play) in 
   match res.Hex_analysis_result_t.number_of_remaining_enemies with 
    0->"No enemy remaining against "^plyr^"."
   |1->"One enemy remaining against "^plyr^"."
   |k->(string_of_int k)^" enemies remaining against "^plyr^".";; 



let full_report res = 
   if not(res.Hex_analysis_result_t.info_needed)
   then ""
   else 
         (report_on_danger res)^
         (report_on_chosen_move res)^
         (report_on_enemies res)^"\n";;
  
end;;

let empty_result = 
{
     Hex_analysis_result_t.next_to_play = Hex_player_t.First_player;
     mandatory_set = Hex_mandatory_set_t.No_constraint;
     dangerous_enemy_strategies = [] ;
     completion_for_strong_move =  None ;
     familiar_moves =  Hex_cell_set_t.S [];
     chosen_move = Hex_cell.of_string "a1"; (* arbitrary, will never be used *)
     number_of_remaining_enemies = 0 ;
     info_needed = true;
  } ;;

let full_report = Private.full_report;;    