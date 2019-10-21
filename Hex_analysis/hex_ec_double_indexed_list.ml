(* 

#use"Hex_analysis/hex_ec_double_list.ml";;

*)

let joiner_in_list ="\n&&&\n";;

let single_list_of_string s= 
    let temp1=Str.split (Str.regexp_string joiner_in_list) s in 
    let temp2=Image.image Hex_end_configuration.of_string temp1 in 
     Ordered.diforchan_plaen Hex_end_configuration.cmp temp2 ;;
  
let single_list_to_string l=String.concat joiner_in_list 
    (Image.image Hex_end_configuration.to_string l);;

let joiner_for_two="\n<<>>\n";;


let empty_one = Hex_ec_double_indexed_list_t.DL([],[]);;

let add_end_config ec (Hex_ec_double_indexed_list_t.DL(l1,l2))=
   match ec.Hex_flattened_end_strategy_t.beneficiary with 
   Hex_player_t.First_player -> 
        let new_l1=Hex_end_configuration.insert_carefully ec l1 in 
        Hex_ec_double_indexed_list_t.DL(new_l1,l2)
  |Hex_player_t.Second_player -> 
        let new_l2=Hex_end_configuration.insert_carefully ec l2 in
        Hex_ec_double_indexed_list_t.DL(l1,new_l2) ;;

let absorb_move (player,cell) (Hex_ec_double_indexed_list_t.DL(l1,l2))=
  match player with 
   Hex_player_t.First_player -> 
      Hex_ec_double_indexed_list_t.DL
      (Hex_end_configuration.use_ally_move_to_simplify_several cell l1,
       Hex_end_configuration.use_enemy_move_to_simplify_several cell l2)
  |Hex_player_t.Second_player -> 
      Hex_ec_double_indexed_list_t.DL
      (Hex_end_configuration.use_ally_move_to_simplify_several cell l1,
       Hex_end_configuration.use_enemy_move_to_simplify_several cell l2)
     ;; 
    
let immediate_dangers player (Hex_ec_double_indexed_list_t.DL(l1,l2))=
  match player with 
   Hex_player_t.First_player -> 
      Hex_end_configuration.immediate_dangers l2
  |Hex_player_t.Second_player -> 
      Hex_end_configuration.immediate_dangers l1;; 
    
let iterated_largest_unconclusive_beginning fgame (Hex_ec_double_indexed_list_t.DL(l1,l2))=
    Hex_finished_game.iterated_largest_unconclusive_beginning fgame (l1@l2);;

let of_string s=
   let n=String.length s 
   and i1=Substring.leftmost_index_of_in joiner_for_two s in 
   let j1=i1+(String.length joiner_for_two)-1 in 
   let part1=Cull_string.interval s 1 (i1-1) 
   and part2=Cull_string.interval s (j1+1) n in 
   Hex_ec_double_indexed_list_t.DL(
     (single_list_of_string part1),
     (single_list_of_string part2)
   ) ;;        

let to_string (Hex_ec_double_indexed_list_t.DL(l1,l2))=
  (single_list_to_string l1)^joiner_for_two^(single_list_to_string l2);;

