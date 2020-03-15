(* 

#use"Hex_analysis/hex_kite_factory_t.ml";;

*)

type t= {
      dimension     : Hex_dimension_t.t ;
      winner        : Hex_player_t.t ;
      initial_data  : Hex_end_of_battle_t.t ;
      finished      : ( Hex_kite_element_t.t * Hex_kite_element_t.t * (Hex_kite_element_t.t list) * Hex_molecular_linker_t.t * Hex_cell_set_t.t ) list ;
      failures      : Hex_partial_kite_t.t list;
      unfinished    : Hex_partial_kite_t.t list;
};;

