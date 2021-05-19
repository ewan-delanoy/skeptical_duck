(* 

#use"Hex_analysis/hex_reindex.ml";;

*)

exception Reindex_exn of int * ((int*int) list );;


let local_list_assoc l a=
   try List.assoc a l with 
   _->raise (Reindex_exn(a,l));;

let flattened_end_strategy reindexer fles = 
  let old_idx = Hex_flattened_end_strategy_automatic.index fles in 
  let new_idx = local_list_assoc reindexer old_idx in 
  Hex_flattened_end_strategy_automatic.set_index fles new_idx;;


let cog reindexer (Hex_cog_in_machine_t.C(statconstr,msg,indices,fles)) = 
  Hex_cog_in_machine_t.C(statconstr,msg,
   Image.image (local_list_assoc reindexer) indices,
   flattened_end_strategy reindexer fles)


let factory reindexer (Hex_end_strategy_factory_t.F(dim,player,l))=
 let new_l=Image.image (cog reindexer) l in 
 Hex_end_strategy_factory_t.F(dim,player,new_l);;