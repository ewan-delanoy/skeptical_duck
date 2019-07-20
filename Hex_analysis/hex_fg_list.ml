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

let remove_redundancies l=
 let rec tempf=(function
   (treated,to_be_treated)->match to_be_treated with
   []->treated
   |fgame::other_games->
     let cleaned_games=List.filter (
       fun fgame2->not(Hex_finished_game.is_more_detailed_than fgame fgame2)
     ) other_games in 
     tempf(fgame::treated,cleaned_games)
 ) in
 tempf([],List.rev l);;

let add_finished_game fgame l=
   let temp1= Ordered.insert_plaen Hex_finished_game.cmp fgame l  in 
   remove_redundancies temp1;;

let take_end_config_into_account end_config l=
    let temp1=Image.image 
    (Hex_finished_game.compute_largest_unconclusive_beginning end_config) l in 
    remove_redundancies temp1;;