(* 

#use"Hex_memory/hex_pgame_collection.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun 
 (Hex_pgame_collection_t.L(l1)) (Hex_pgame_collection_t.L(l2)) ->
   (Total_ordering.lex_compare 
       Hex_partial_game.cmp l1 l2) :> 
       Hex_pgame_collection_t.t Total_ordering.t) );;

let joiner = " \n ";;

let of_string s =
  let temp1=Str.split (Str.regexp_string joiner) s in 
  let temp2=Image.image Hex_partial_game.of_string temp1 in 
  Hex_pgame_collection_t.L(
     Ordered.diforchan_plaen Hex_partial_game.cmp temp2
  );;

let to_string (Hex_pgame_collection_t.L(l))=
  String.concat joiner (Image.image Hex_partial_game.to_string l);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     