(* 

#use"Hex_analysis/hex_shortcut.ml";;

*)



let list_for_spec1=["f6"; "f8"; "c9"; "e7"; "d7"; "e6"; "d6"; "e4"; "e5"; "f4"; "f5"; "g4"];;

let abl = Hex_analysis.add_basic_linker ;;
let am s = Hex_analysis.absorb_move (Hex_cell.of_string s);;
let ams s = Hex_analysis.absorb_all_moves (Hex_common.parse_list_of_moves s);;
let clg = Hex_persistent.cancel_last_game;;
let spec1 j = Hex_analysis.absorb_some_moves 
     (Image.image Hex_cell.of_string list_for_spec1) j;;
let dw k=Hex_analysis.declare_winner (Hex_player.of_int k);;
let init = Hex_persistent.initialize_all_data_if_necessary ;;
let lau = Hex_shell.launch ;;
let ps=Hex_ascii_grid.process_sheet;;
let refs ()=(!Hex_persistent.games_ref,!Hex_persistent.untamed_openings_ref,
!Hex_persistent.winning_end_strategies_for_first_player_ref,!Hex_persistent.winning_end_strategies_for_second_player_ref);;
let rep = Hex_analysis.replay_and_declare_winner ;;   
let res = Hex_analysis.restart ;;   
let u = Hex_analysis.move_as_usual;;
let uo = Hex_analysis.undo_last_absorption;;
let wk=Hex_analysis.walker;;