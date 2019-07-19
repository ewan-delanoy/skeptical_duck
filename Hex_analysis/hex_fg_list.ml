(* 

#use"Hex_analysis/hex_fg_list.ml";;

*)


let joiner_in_list ="\n&[&]&\n";;

let of_string s= 
    let temp1=Str.split (Str.regexp_string joiner_in_list) s in 
    let temp2=Image.image Hex_finished_game.of_string temp1 in 
     Ordered.diforchan_plaen Hex_finished_game.cmp temp2 ;;
  
let to_string l=String.concat joiner_in_list 
    (Image.image Hex_finished_game.to_string l);;

