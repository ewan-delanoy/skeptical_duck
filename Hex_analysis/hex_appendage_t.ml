(* 

#use"Hex_analysis/hex_appendage_t.ml";;

Appendages to basic linkers, as in 
Hex_strategy_static_constructor_t.Basic_Linker

*)

type t=
    Eyed_claw of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t 
    (* first direction indicates which side the eye is,
       second direction indicates which border is being joined
    *)
    ;;
