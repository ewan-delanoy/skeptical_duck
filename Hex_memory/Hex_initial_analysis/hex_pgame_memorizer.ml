(* 

#use"Hex_memory/hex_pgame_memorizer.ml";;

First coordinate is column index, second is row index

*)

let announce_first_player="\nFirst player : \n";;
let announce_second_player="\nSecond player : \n";;



let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_first_player) s in 
   let i1=Substring.leftmost_index_of_in announce_second_player s1 in
   let descr1=Cull_string.beginning (i1-1) s1 in
   let descr2=Cull_string.cobeginning (i1+(String.length announce_second_player)-1) s1 in 
   {
     Hex_cigame_memorizer_t.strategies_for_first_player=Hex_cigame_collection.of_string descr1;
     Hex_cigame_memorizer_t.strategies_for_second_player=Hex_cigame_collection.of_string descr2;
   };;

let to_string mmrzr=
  let descr1=Hex_cigame_collection.to_string(mmrzr.Hex_cigame_memorizer_t.strategies_for_first_player) 
  and descr2=Hex_cigame_collection.to_string(mmrzr.Hex_cigame_memorizer_t.strategies_for_second_player) in 
  announce_first_player^descr1^announce_second_player^descr2;;


let remember_as_example mmrzr =
  let assignment="\n\n\nlet z=Hex_pgame_memorizer.of_string\n\""^(to_string mmrzr)^"\";;\n\n\n" in 
  let ap=Absolute_path.of_string "Hex_memory/Hex_initial_analysis/hex_pgame_memorizer_example.ml" in 
  Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) ("(* Assignment starts here *)","(* Assignment ends here *)") ap ;;

let strategies mmrzr=function 
   Hex_player_t.First_player -> mmrzr.Hex_cigame_memorizer_t.strategies_for_first_player
   |Hex_player_t.Second_player -> mmrzr.Hex_cigame_memorizer_t.strategies_for_second_player ;;

let is_foreseen_in pgame mmrzr=
   let the_strategies=strategies mmrzr (Hex_checked_initial_game.last_one_to_play pgame) in 
   Hex_cigame_collection.is_foreseen_in pgame the_strategies;;

let initial_one cell= 
   {
     Hex_cigame_memorizer_t.strategies_for_first_player=Hex_cigame_collection.singleton cell;
     Hex_cigame_memorizer_t.strategies_for_second_player=Hex_cigame_collection.empty_one;
   };;

let insert_in pgame mmrzr =
     let last_player = Hex_checked_initial_game.last_one_to_play pgame in  
     let old_strategies = strategies mmrzr last_player in 
     let new_strategies = Hex_cigame_collection.insert_in pgame old_strategies in 
     match last_player with 
     Hex_player_t.First_player -> 
        {mmrzr with Hex_cigame_memorizer_t.strategies_for_first_player= new_strategies}
    |Hex_player_t.Second_player -> 
        {mmrzr with Hex_cigame_memorizer_t.strategies_for_second_player= new_strategies};;

let cut_by mmrzr pgame=
  let last_player = Hex_checked_initial_game.last_one_to_play pgame in 
  let future_player = (function 
      Hex_player_t.First_player -> Hex_player_t.Second_player
      |_-> Hex_player_t.First_player
  ) last_player in 
  Hex_cigame_collection.cut_by (strategies mmrzr future_player) pgame;;



let print_out (fmt:Format.formatter) memorizer=
   Format.fprintf fmt "@[%s@]" (to_string memorizer);;     