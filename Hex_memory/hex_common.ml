(* 

#use"Hex_memory/hex_common.ml";;

*)

let joiner = " - ";;

let cell_list_of_string s =
  let temp1=Str.split (Str.regexp_string joiner) s in 
    Image.image Hex_cell.of_string temp1;;

let cell_list_to_string l=
  String.concat joiner (Image.image Hex_cell.to_string l);;