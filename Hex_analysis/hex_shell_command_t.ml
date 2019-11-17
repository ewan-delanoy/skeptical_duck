(* 

#use"Hex_analysis/hex_shell_command_t.ml";;

*)

type t=
    Add_basic_linker_from_sheet of string
   |Absorb_moves of Hex_cell_t.t list
   |Cancel_last_game 
   |Declare_winner of Hex_player_t.t 
   |Make_usual_move
   |Preprocess_sheet
   |Quit
   |Replay_written_game
   |Start_officially of Hex_player_t.t 
   |Start_incognito
   |Undo_last_move
   |Unknown_command;;
