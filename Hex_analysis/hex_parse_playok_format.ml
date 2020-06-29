(* 

#use"Hex_analysis/hex_parse_playok_format.ml";;

*)

exception No_winner_found of string;;

module Private = struct 

let path_for_latest_game = Absolute_path.of_string "Hex_analysis/Hex_gitignored_text_files/hex_latest_game.txt";;

let read_role text =
   if Substring.is_a_substring_of  "[Black \"dtn4143g\"]" text then Hex_player_t.First_player else 
   if Substring.is_a_substring_of  "[White \"dtn4143g\"]" text then Hex_player_t.Second_player else 
   raise(No_winner_found(text));;
  
let find_number_range_before_index s idx =
   let indices=List.rev (Ennig.ennig 1 (idx-1)) in 
   match Option.seek ( fun j->
      let c=Strung.get s j in 
      not(String.contains "0123456789" c)
   ) indices with 
   None -> (1,idx)
   |Some(j0) ->(j0+1,idx);;

let clean_number_ranges s =
   let temp1=Substring.occurrences_of_in "." s in 
   let temp2=Image.imagination (fun idx->(find_number_range_before_index s idx,"")) temp1 in 
   Strung.replace_ranges_in temp2 s;;

let parse ()=
     let text = Io.read_whole_file path_for_latest_game in 
     let i1=(String.rindex text ']')+1 in 
     let text_telling_role = Cull_string.beginning i1 text in 
     let game_report = Cull_string.cobeginning i1 text in 
     let role_played = read_role text_telling_role in 
     let report1 = Replace_inside.replace_several_inside_string ["\n"," ";"*"," "] game_report in 
     let report2 = clean_number_ranges report1  in 
     let moves = Cull_string.extract_intervals_in_wrt_separator report2 " " in 
     let cells = Image.imagination Hex_cell.of_string moves in 
     (role_played,cells);;

end ;;

let parse = Private.parse ;;
