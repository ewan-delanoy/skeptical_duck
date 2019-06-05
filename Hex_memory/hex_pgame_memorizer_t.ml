(* 

#use"Hex_memory/hex_pgame_memorizer_t.ml";;

First coordinate is column index, second is row index

*)

type t= {
   strategies_for_first_player : Hex_pgame_collection_t.t; (* odd length *)
   strategies_for_second_player : Hex_pgame_collection_t.t; (* even length *)
};;
