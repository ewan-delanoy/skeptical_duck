(* 

#use"Hex_analysis/hex_shell_command.ml";;

*)

let send_report ()=
   let msg = Hex_analysis_result.full_report(snd(!(Hex_analysis.walker))) in 
   print_string msg;flush stdout;;

let parse_list_of_moves s=
   let temp1=Cull_string.extract_intervals_in_wrt_separator s "," in 
   Image.image Hex_cell.of_string temp1;;

let execute = function 
    Hex_shell_command_t.Add_basic_linker_from_sheet(msg) -> Hex_analysis.add_basic_linker msg
   |Absorb_moves(l)->let _=Hex_analysis.absorb_all_moves l in send_report()
   |Cancel_last_game -> Hex_persistent.cancel_last_game()
   |Declare_winner(player)->Hex_analysis.declare_winner player
   |Make_usual_move ->let _=Hex_analysis.move_as_usual () in send_report()
   |Preprocess_sheet -> let _=Hex_ascii_grid.process_sheet () in ()
   |Quit -> (Hex_shell_internals.game_is_on:=false)
   |Replay_written_game -> Hex_analysis.replay_and_declare_winner ()
   |Start_new_game -> let _=Hex_analysis.restart() in send_report()
   |Undo_last_move -> let _=Hex_analysis.undo_last_absorption() in send_report() 
   |Unknown_command -> (print_string "Unknown command";flush stdout);;


let of_string untrimmed_text =
   let text = Cull_string.trim_spaces untrimmed_text  in 
   if Supstring.begins_with text "abl"  
   then let msg = Cull_string.two_sided_cutting ("abl","") text in 
        Hex_shell_command_t.Add_basic_linker_from_sheet msg 
   else 
   if text = "clg" then  Hex_shell_command_t.Cancel_last_game else
   if text = "dwb" then  Hex_shell_command_t.Declare_winner(Hex_player_t.First_player) else 
   if text = "dww" then  Hex_shell_command_t.Declare_winner(Hex_player_t.Second_player) else    
   if text = "u"   then  Hex_shell_command_t.Make_usual_move else
   if text = "ps"  then  Hex_shell_command_t.Preprocess_sheet else 
   if text = "q"   then  Hex_shell_command_t.Quit else 
   if text = "r"   then  Hex_shell_command_t.Replay_written_game else 
   if text = "s"   then  Hex_shell_command_t.Start_new_game else
   if text = "uo"  then  Hex_shell_command_t.Undo_last_move else
                   try   Hex_shell_command_t.Absorb_moves(parse_list_of_moves text) with 
                   _->   Hex_shell_command_t.Unknown_command;; 

   