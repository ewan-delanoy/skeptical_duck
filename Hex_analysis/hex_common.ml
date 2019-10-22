(* 

#use"Hex_analysis/hex_common.ml";;

*)


exception Translate_horizontally_exn of int * string;;
exception Translate_vertically_exn of int * int;;


let joiner = " - ";;

let cell_list_of_string s =
  let temp1=Str.split (Str.regexp_string joiner) s in 
    Image.image Hex_cell.of_string temp1;;

let cell_list_to_string l=
  String.concat joiner (Image.image Hex_cell.to_string l);;

let cell_pair_of_string text =
   let l=cell_list_of_string text in 
   (List.nth l 0,List.nth l 1);;

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

let all_cells dimension=
   let temp1=Ennig.doyle (fun j->String.make 1 (char_of_int(j+96))) 1 dimension 
   and temp2=Ennig.doyle (fun i->string_of_int i) 1 dimension in 
   let temp3=Cartesian.product temp1 temp2 in 
   let temp4=Image.image (fun (sj,si)->Hex_cell.of_string(sj^si)) temp3 in 
   Hex_cell_set.safe_set temp4;;

let translate_column dimension d s1=
   let i2=(int_of_char(String.get s1 0))+d-96 in 
   if (i2<1)||(i2>dimension)
   then raise(Translate_horizontally_exn(d,s1))
   else String.make 1 (char_of_int(i2+96));;

let translate_row dimension d i1=
   let i2=i1+d in 
   if (i2<1)||(i2>dimension)
   then raise(Translate_vertically_exn(d,i1))
   else i2;;

let translate_horizontally dim d (Hex_cell_t.C(s,i))= Hex_cell_t.C(translate_column dim d s,i);; 
let translate_vertically dim d (Hex_cell_t.C(s,i))= Hex_cell_t.C(s,translate_row dim d i);; 

let translate dim (dx,dy) cell = translate_vertically dim  dy (ranslate_horizontally dim dx cell);;

