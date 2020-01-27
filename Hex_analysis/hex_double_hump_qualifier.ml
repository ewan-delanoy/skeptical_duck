(* 

#use"Hex_analysis/hex_double_hump_qualifier.ml";;

*)

let to_int = function 
    (* the correspondance is arbitrary, it just needs to be bijective. *)
     Hex_double_hump_qualifier_t.Small_followed_by_big -> 1
    |Big_followed_by_small -> 2 ;;