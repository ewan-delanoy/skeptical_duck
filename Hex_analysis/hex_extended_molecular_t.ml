(* 

#use"Hex_analysis/hex_extended_molecular_t.ml";;

*)

type t= {
    molecular_part : Hex_molecular_linker_t.t ;
    nonmolecular_passive_part : Hex_cell_set_t.t ;
    active_part : Hex_cell_set_t.t 
} ;;
  
