(* 

#use"Hex_memory/Hex_final_analysis/hex_abstract_color_t.ml";;

First coordinate is column index, second is row index

*)

type t= 
    No_color
   |First_player_color
   |Second_player_color
   |Registered_ban of int;;