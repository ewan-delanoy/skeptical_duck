(* 

#use"Hex_analysis/hex_fg_list.ml";;

*)



let of_concrete_object crobj = Concrete_object_field.to_list Hex_finished_game.of_concrete_object crobj;;
let to_concrete_object l = Concrete_object_field.of_list Hex_finished_game.to_concrete_object l;;

let remove_redundancies l=
 let rec tempf=(function
   (treated,to_be_treated)->match to_be_treated with
   []->treated
   |fgame::other_games->
     let cleaned_games=List.filter (
       fun fgame2->not(Hex_finished_game.extends fgame fgame2)
     ) other_games in 
     tempf(fgame::treated,cleaned_games)
 ) in
 tempf([],List.rev l);;

let add_finished_game fgame l=
   let temp1= Erdurod.insert Hex_finished_game.cmp fgame l  in 
   remove_redundancies temp1;;

let take_new_end_strategy_into_account end_config l=
    let temp1=Image.image 
    (fun fgame->Hex_finished_game.compute_largest_unconclusive_beginning fgame end_config) l in 
    remove_redundancies temp1;;

let simplify_by_move move l=Option.filter_and_unpack (Hex_finished_game.simplify_by_move move) l;;     

let first_moves l=
    Hex_cell_set.safe_set (Image.image Hex_finished_game.first_move l);;

let lookup_dimension =function 
    []->None
   |fgame::_->Some(fgame.Hex_finished_game_t.dimension);;


