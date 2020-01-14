(* 

#use"Hex_analysis/hex_mandatory_set_t.ml";;

*)

type t= 
  No_constraint
| Constraint of ( Hex_molecular_linker_t.t * Hex_cell_set_t.t ) ;;