(* 

#use"Hex_analysis/hex_strategy_static_constructor.ml";;

*)

let summarize_in_string = function
    Hex_strategy_static_constructor_t.Basic_Linker(_,_)->"Basic linker"
   |Gluing -> "Gluing" 
   |Disjunction (_)->"Disjunction";;
   