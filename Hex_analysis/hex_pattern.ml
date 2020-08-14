(* 

#use"Hex_analysis/hex_pattern.ml";;

*)

let constructor (l_passive,l_active) =
    {
     Hex_pattern_t.passive = Hex_cell_set.of_int_pair_list l_passive ; 
     active  = Hex_cell_set.of_int_pair_list l_active ;
    };;


