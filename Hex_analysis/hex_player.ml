(* 

#use"Hex_analysis/hex_player.ml";;

*)

let other_player = function 
     Hex_player_t.First_player-> Hex_player_t.Second_player 
    |Hex_player_t.Second_player->Hex_player_t.First_player;;

exception Incorrect_hex_player_of_string_arg of string;;

let of_string s=
   if s="1" then Hex_player_t.First_player else 
   if s="2" then Hex_player_t.Second_player else 
   raise(Incorrect_hex_player_of_string_arg(s));;

let to_string=function  
     Hex_player_t.First_player->"1" 
    |Hex_player_t.Second_player->"2";;

let of_int i=of_string(string_of_int i);;

