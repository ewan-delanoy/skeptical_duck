(* 

#use"Hex_analysis/hex_shortcuts.ml";;

*)

let abl = Hex_analysis.add_basic_linker ;;
let am s = Hex_analysis.absorb_move (Hex_cell.of_string s);;
let ams l = Hex_analysis.absorb_moves (Image.image Hex_cell.of_string l);;
let dw k=Hex_analysis.declare_winner (Hex_player.of_int k);;
let init = Hex_persistent.initialize_all_data_if_necessary ;;
let ps=Hex_ascii_grid.process_sheet;;
let refs ()=(!Hex_persistent.games_ref,!Hex_persistent.strong_openings_ref,
!Hex_persistent.winning_end_strategies_for_first_player_ref,!Hex_persistent.winning_end_strategies_for_second_player_ref);;
let rep = Hex_analysis.replay_and_declare_winner ;;   
let res = Hex_analysis.restart ;;   
let u = Hex_analysis.move_as_usual;;
let uo = Hex_analysis.undo_last_absorption;;
let wk=Hex_analysis.walker;;