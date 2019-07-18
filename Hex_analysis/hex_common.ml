(* 

#use"Hex_analysis/hex_common.ml";;

*)

let joiner = " - ";;

let cell_list_of_string s =
  let temp1=Str.split (Str.regexp_string joiner) s in 
    Image.image Hex_cell.of_string temp1;;

let cell_list_to_string l=
  String.concat joiner (Image.image Hex_cell.to_string l);;

let cell_pair_to_string (cell1,cell2)=cell_list_to_string [cell1;cell2];;
  

let cell_list_to_pretty_string l=
  let (pairs,optional_last)=Listennou.divide_by_two l in 
  let part1= String.concat " | " (Image.image cell_pair_to_string pairs) in 
  let part2=(match optional_last with 
     None->""
     |Some(cell)->Hex_cell.to_string cell
  ) in 
  if (part1="")||(part2="")
  then part1^part2
  else part1^joiner^part2;;