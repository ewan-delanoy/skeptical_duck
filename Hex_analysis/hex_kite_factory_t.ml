(* 

#use"Hex_analysis/hex_kite_factory_t.ml";;

*)

type t= {
      dimension     : Hex_dimension_t.t ;
      winner        : Hex_player_t.t ;
      finished      : Hex_molecular_linker_t.t list ;
      unfinished    : Hex_partial_kite_t.t list;
};;

