(* 

#use"Hex_memory/Hex_final_analysis/hex_dc_double_list.ml";;

*)

let joiner_in_list ="\n&&&\n"

let single_list_of_string s= 
    let temp1=Str.split (Str.regexp_string joiner_in_list) s in 
    let temp2=Image.image Hex_decisive_configuration.of_string temp1 in 
     Ordered.diforchan_plaen Hex_decisive_configuration.cmp temp2 ;;
  
let single_list_to_string l=String.concat joiner_in_list 
    (Image.image Hex_decisive_configuration.to_string l);;

let joiner_for_two="\n<<>>\n"

let of_string s=
   let n=String.length s 
   and i1=Substring.leftmost_index_of_in joiner_for_two s in 
   let j1=i1+(String.length joiner_for_two)-1 in 
   let part1=Cull_string.interval s 1 (i1-1) 
   and part2=Cull_string.interval s (j1+1) n in 
   Hex_dc_double_list_t.DL(
     (single_list_of_string part1),
     (single_list_of_string part2)
   ) ;;        

let to_string (Hex_dc_double_list_t.DL(l1,l2))=
  (single_list_to_string l1)^joiner_for_two^(single_list_to_string l2);;

let immediate_dangers (Hex_dc_double_list_t.DL(l1,l2))=function 
   Hex_player_t.First_player -> Hex_decisive_configuration.immediate_dangers (Ennig.index_everything l2) 
  |Hex_player_t.Second_player -> Hex_decisive_configuration.immediate_dangers (Ennig.index_everything l1);; 
