(* 

#use"Hex_analysis/hex_eyed_claw_t.ml";;

*)

type t=
    C of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t 
    (* first direction indicates which side the eye is,
       second direction indicates which border is being joined
    *)
    ;;
