(* 

#use"Hex_analysis/hex_common.ml";;

*)



module Private = struct 

let joiner = " - ";;

let cell_list_to_string l=
  String.concat joiner (Image.image Hex_cell.to_string l);;


let cell_pair_to_string (cell1,cell2)=cell_list_to_string [cell1;cell2];;

  

end ;;

let all_cells (Hex_dimension_t.D dimension)=
   let temp1=Ennig.doyle (fun j->String.make 1 (char_of_int(j+96))) 1 dimension 
   and temp2=Ennig.doyle (fun i->string_of_int i) 1 dimension in 
   let temp3=Cartesian.product temp1 temp2 in 
   let temp4=Image.image (fun (sj,si)->Hex_cell.of_string(sj^si)) temp3 in 
   Hex_cell_set.safe_set temp4;;



let cell_list_of_string s =
  let temp1=Str.split (Str.regexp_string Private.joiner) s in 
    Image.image Hex_cell.of_string temp1;;

let cell_list_to_pretty_string l=
  let (pairs,optional_last)=Listennou.divide_by_two l in 
  let part1= String.concat " | " (Image.image Private.cell_pair_to_string pairs) in 
  let part2=(match optional_last with 
     None->""
     |Some(cell)->Hex_cell.to_string cell
  ) in 
  if (part1="")||(part2="")
  then part1^part2
  else part1^Private.joiner^part2;;

let cell_list_to_string = Private.cell_list_to_string ;;

let cell_pair_of_string text =
   let l=cell_list_of_string text in 
   (List.nth l 0,List.nth l 1);;

let cell_pair_to_string = Private.cell_pair_to_string;;


let has_just_played preceding_moves=
   if ((List.length preceding_moves) mod 2=1)  
   then Hex_player_t.First_player
   else Hex_player_t.Second_player;;
  


let next_one_to_play preceding_moves=
   if ((List.length preceding_moves) mod 2=0)  
   then Hex_player_t.First_player
   else Hex_player_t.Second_player;;

let parse_list_of_moves s=
   let temp1=Cull_string.extract_intervals_in_wrt_separator s "," in 
   Image.image Hex_cell.of_string temp1;;

