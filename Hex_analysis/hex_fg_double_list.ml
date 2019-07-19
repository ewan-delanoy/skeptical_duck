(* 

#use"Hex_analysis/hex_fg_double_list.ml";;

*)

let empty_one = Hex_fg_double_list_t.DL([],[]);;

let joiner_for_two="\n<[<>]>\n";;

let of_string s=
   let n=String.length s 
   and i1=Substring.leftmost_index_of_in joiner_for_two s in 
   let j1=i1+(String.length joiner_for_two)-1 in 
   let part1=Cull_string.interval s 1 (i1-1) 
   and part2=Cull_string.interval s (j1+1) n in 
   Hex_fg_double_list_t.DL(
     (Hex_fg_list.of_string part1),
     (Hex_fg_list.of_string part2)
   ) ;;        

let to_string (Hex_fg_double_list_t.DL(l1,l2))=
  (Hex_fg_list.to_string l1)^joiner_for_two^(Hex_fg_list.to_string l2);;

