(* 

#use"Hex_analysis/hex_so_list.ml";;

*)


let joiner_in_list =" | ";;

let of_string s= 
    let temp1=Str.split (Str.regexp_string joiner_in_list) s in 
    let temp2=Image.image Hex_strong_opening.of_string temp1 in 
     Ordered.diforchan_plaen Hex_strong_opening.cmp temp2 ;;
  
let to_string l=String.concat joiner_in_list 
    (Image.image Hex_finished_game.to_string l);;

let insert_in new_so l=
   if List.exists (fun so -> Hex_strong_opening.extends so new_so ) l
   then l 
   else 
     let cleaned_l=List.filter 
       (fun so->not(Hex_strong_opening.extends new_so so)) l in 
     Ordered.insert_plaen Hex_strong_opening.cmp new_so cleaned_l;;  

let simplify_by_move move l=
   Option.filter_and_unpack (Hex_strong_opening.simplify_by_move move) l;;     

let easy_advances l=Option.filter_and_unpack  Hex_strong_opening.easy_advance l;; 
