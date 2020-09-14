(* 

#use"Hex_analysis/hex_order_for_player_sides.ml";;


*)

let select player idx =
   let the_list = (match player with 
      Hex_player_t.First_player -> [Hex_cardinal_direction_t.Down ; Hex_cardinal_direction_t.Up]
     |Hex_player_t.Second_player -> [Hex_cardinal_direction_t.Left ; Hex_cardinal_direction_t.Right]
   ) in 
   List.nth the_list (idx-1);;