(* 

#use"Hex_analysis/hex_shortcuts.ml";;

*)

let abl = Hex_analysis.add_basic_linker ;;
let am s = Hex_analysis.absorb_move (Hex_cell.of_string s);;
let ams l = Hex_analysis.absorb_moves (Image.image Hex_cell.of_string l);;
let dw k=Hex_analysis.declare_winner (Hex_player.of_int k);;
let wk=Hex_analysis.walker;;
let ps=Hex_ascii_grid.process_sheet;;
let refs ()=(!Hex_persistent.games_ref,!Hex_persistent.strong_openings_ref,
!Hex_persistent.winning_end_strategies_for_first_player_ref,!Hex_persistent.winning_end_strategies_for_second_player_ref);;
let add_linker i msg (a,p)=Hex_persistent.add_end_strategy 
   (Hex_player.of_int i,Hex_strategy_static_constructor_t.Basic_Linker(a,p),msg,[]);;
let rep i = Hex_analysis.replay_and_declare_winner (Hex_player.of_string i);;   
let res i = Hex_analysis.restart (Hex_player.of_string i);;   
