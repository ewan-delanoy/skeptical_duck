(* 

#use"Hex_analysis/hex_cog_in_machine_t.ml";;

*)

type t= C of 
 ( Hex_strategy_static_constructor_t.t * 
  string * (int list) * 
  Hex_molecular_extraction_t.t * Hex_flattened_end_strategy_t.t ) ;;
   