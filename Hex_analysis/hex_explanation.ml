(* 

#use"Hex_analysis/hex_explanation.ml";;

Here we explain of how some strategies are constructed.

*)

let basic_linker  cells pairs =
   let ceos = Hex_cell.of_string in 
   (Hex_cell_set.safe_set (Image.image ceos cells), 
    Hex_cell_pair_set.constructor 
      (Image.image (fun (x,y)->(ceos x,ceos y)) pairs));; 
   


let for_Black =
  ref(Hex_end_strategy_factory.empty_one Hex_player_t.First_player);;
let for_White =
  ref(Hex_end_strategy_factory.empty_one Hex_player_t.Second_player);;  
let for_both = (for_Black,for_White);;
let create1 = 
   Hex_end_strategy_factory.create_new_strategy 
     false for_both Hex_player_t.First_player;;
let create2 = 
   Hex_end_strategy_factory.create_new_strategy 
     false for_both Hex_player_t.Second_player;;
let bali (x,y) = Hex_strategy_static_constructor_t.Basic_Linker (Hex_octopus.empty_one,x,y);;      

(* Explaining a right-eyed upper claw *)   

let for_strat1=
  basic_linker  ["c4";"d3"] 
   ["d2","f2";"f1","g1";"d1","e1";"e2","e3"] ;;
let strat1_constructor = bali for_strat1;;
let fles1=create1 strat1_constructor "" [];;

let for_strat2=
  basic_linker  ["c3";"c4"] 
   ["b2","e2";"b1","c1";"e1","f1";
     "b3","c2";"d2","d3"] ;;
let strat2_constructor = bali for_strat2;;
let fles2=create1 strat2_constructor "" [];;

let for_strat3=
  basic_linker  ["b3";"c4"] 
   ["a2","c2";"a1","b1";"c1","d1";
     "a3","b2";"b4","c3"] ;;
let strat3_constructor = bali for_strat3;;
let fles3=create1 strat3_constructor "" [];;

let lonely_c4=Hex_cell_set.safe_set [Hex_cell.of_string "c4"];;

let list_for_right_eyed_claw = [1;2;3];;
let helper_for_right_eyed_claw =
   Hex_end_strategy_factory.reconstruct_disjunction
    (!for_Black) lonely_c4 list_for_right_eyed_claw;;
let fles_for_right_eyed_claw=
   create1 (Hex_strategy_static_constructor_t.Disjunction
    (helper_for_right_eyed_claw)) "" list_for_right_eyed_claw;;

let ipairs_in_right_eyed_upwards_claw =
   let (Hex_cell_set_t.S pp )=
     fles_for_right_eyed_claw.Hex_flattened_end_strategy_t.passive_part in 
   Ordered.sort Total_ordering.standard2 (Image.image Hex_ipair.of_cell pp);;

(* Explaining a left-eyed upper upwards claw *)   


let for_strat5=
  basic_linker  ["b3";"b4"] 
   ["a2","c2";"a1","b1";"c1","d1";"a3","b2"] ;;
let strat5_constructor = bali for_strat5;;
let fles5=create1 strat5_constructor "" [];;


let for_strat6=
  basic_linker  ["c3";"b4"] 
   ["a2","d2";"b1","c1";"e1","f1";
     "b3","c2";"d2","d3"] ;;
let strat6_constructor = bali for_strat6;;
let fles6=create1 strat6_constructor "" [];;


let for_strat7=
  basic_linker  ["b4";"d3"] 
   ["d2","f2";"d1","e1";"f1","g1";
     "c3","c4";"e2","e3"] ;;
let strat7_constructor = bali for_strat7;;
let fles7=create1 strat7_constructor "" [];;

let lonely_b4=Hex_cell_set.safe_set [Hex_cell.of_string "b4"];;

let list_for_left_eyed_claw = [5;6;7];;
let helper_for_left_eyed_claw =
   Hex_end_strategy_factory.reconstruct_disjunction
    (!for_Black) lonely_b4 list_for_left_eyed_claw;;
let fles_for_left_eyed_claw=
   create1 (Hex_strategy_static_constructor_t.Disjunction
    (helper_for_left_eyed_claw)) "" list_for_left_eyed_claw;;

let ipairs_in_left_eyed_upwards_claw =
   let (Hex_cell_set_t.S pp )=
     fles_for_left_eyed_claw.Hex_flattened_end_strategy_t.passive_part in 
   Ordered.sort Total_ordering.standard2 (Image.image Hex_ipair.of_cell pp);;
