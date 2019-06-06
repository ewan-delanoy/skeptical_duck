(* 

#use"Hex_memory/hex_pgame_memorizer.ml";;

First coordinate is column index, second is row index

*)

let announce_first_player="\nFirst player : \n";;
let announce_second_player="\nSecond player : \n";;



let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_first_player) s in 
   let i1=Substring.leftmost_index_of_in announce_second_player s1 in
   let descr1=Cull_string.beginning (i1-1) s1 in
   let descr2=Cull_string.cobeginning (i1+(String.length announce_second_player)-1) s1 in 
   {
     Hex_pgame_memorizer_t.strategies_for_first_player=Hex_pgame_collection.of_string descr1;
     Hex_pgame_memorizer_t.strategies_for_second_player=Hex_pgame_collection.of_string descr2;
   };;
  

let to_string mmrzr=
  let descr1=Hex_pgame_collection.to_string(mmrzr.Hex_pgame_memorizer_t.strategies_for_first_player) 
  and descr2=Hex_pgame_collection.to_string(mmrzr.Hex_pgame_memorizer_t.strategies_for_second_player) in 
  announce_first_player^descr1^announce_second_player^descr2;;


let print_out (fmt:Format.formatter) memorizer=
   Format.fprintf fmt "@[%s@]" (to_string memorizer);;     