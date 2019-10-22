(* 

#use"Hex_analysis/hex_initializer.ml";;

This file should be used not compiled. 



*)

(* End strategies start here *)


 Hex_end_strategy_factory.fill_with_string Hex_persistent.winning_end_strategies_for_first_player_ref 

("
Beneficiary : 
1
Data : 

Basic linker : 
 |||

{b6,c4,c6,c9,d4,d6,d7,d8,e3} |||

{a11 - b11,b5 - c5,b10 - d10,c10 - d9,c11 - d11,e1 - f1,e2 - g2,f2 - f3,g1 - h1}");;








 Hex_end_strategy_factory.fill_with_string Hex_persistent.winning_end_strategies_for_second_player_ref 

("
Beneficiary : 
2
Data : 
");;


(* End strategies end here *)


(* Games start here *)


 Hex_persistent.games_ref:=Hex_fg_double_list.of_string(
"
Beneficiary : 
1
Active part : 
f6 - f8 - c9 - e7 - d7 - e6 - d6 - e4 - e5 - f4 - f5 - g4 - d4 - d5 - b6 - d3 - c4 - c3 - e3 - c8
<[<>]>
");;


(* Games end here *)