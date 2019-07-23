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

let next_one_to_play preceding_moves=
   if ((List.length preceding_moves) mod 2=0)  
   then Hex_player_t.First_player
   else Hex_player_t.Second_player;;

let split_fifo_list_in_half l=
   let temp1=Ennig.index_everything(List.rev l) in 
   let (temp2,temp3)=List.partition (fun (j,_)->(j mod 2)=1) temp1 in 
   (Image.image snd temp2,Image.image snd temp3);;
