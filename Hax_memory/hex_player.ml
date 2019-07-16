(* 

#use"Hex_memory/hex_player.ml";;

*)

exception Incorrect_hex_player_of_string_arg of string;;

let of_string s=
   if s="1" then Hax_player_t.First_player else 
   if s="2" then Hax_player_t.Second_player else 
   raise(Incorrect_hex_player_of_string_arg(s));;

let to_string=function  
     Hax_player_t.First_player->"1" 
    |Hax_player_t.Second_player->"2";;