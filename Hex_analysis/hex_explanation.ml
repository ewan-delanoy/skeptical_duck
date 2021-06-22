(* 

#use"Hex_analysis/hex_explanation.ml";;


Here we explain of how some strategies are constructed.

On the board, there are exactly two cells with valence 2 : the upper left and lower 
right corners. They must therefore be fixed or exchanegd by any symmetry of the board. 
It easily follows that the group of symmetries of the board is a Klein group, generated 
by sigma(x,y)=(y,x) (symmetry wrt to main diagonal) and tau(x,y)=(d+1-x,d+1-y) (central
symmetry).

Also,

sigma(left-eyed upwards)  = high-eyed leftwards
sigma(right-eyed upwards) = low-eyed leftwards

tau(left-eyed upwards)  = right-eyed downwards
tau(right-eyed upwards) = left-eyed downtwards

tau(high-eyed leftwards)  = high-eyed rightwards
tau(low-eyed leftwards) = low-eyed rightwards



*)

let molecular_with_actives  cells pairs =
   let ceos = Hex_cell.of_string in 
   (Hex_molecular_linker.constructor 
      (Image.image (fun (x,y)->Hex_atomic_linker.pair(ceos x,ceos y)) pairs),
    Hex_cell_set.safe_set (Image.image ceos cells) );; 
   


let for_Black =
  ref(Hex_end_strategy_factory.empty_one Hex_dimension.eleven Hex_player_t.First_player);;
let for_White =
  ref(Hex_end_strategy_factory.empty_one Hex_dimension.eleven Hex_player_t.Second_player);;  
let each_his_own = (for_Black,for_White);;
let create1 = 
   Hex_end_strategy_factory.create_new_strategy 
     false each_his_own Hex_player_t.First_player;;
let create2 = 
   Hex_end_strategy_factory.create_new_strategy 
     false each_his_own Hex_player_t.Second_player;;
let mol (x,y) = Hex_strategy_static_constructor_t.Molecular (x,y);;      

(* Explaining a right-eyed upper claw *)   

let for_strat1=
  molecular_with_actives  ["c4";"d3"] 
   ["d2","f2";"f1","g1";"d1","e1";"e2","e3"] ;;
let strat1_constructor = mol for_strat1;;
let fles1=create1 strat1_constructor "" [];;

let for_strat2=
  molecular_with_actives  ["c3";"c4"] 
   ["b2","e2";"b1","c1";"e1","f1";
     "b3","c2";"d2","d3"] ;;
let strat2_constructor = mol for_strat2;;
let fles2=create1 strat2_constructor "" [];;

let for_strat3=
  molecular_with_actives  ["b3";"c4"] 
   ["a2","c2";"a1","b1";"c1","d1";
     "a3","b2";"b4","c3"] ;;
let strat3_constructor = mol for_strat3;;
let fles3=create1 strat3_constructor "" [];;

let lonely_c4=Hex_cell_set.safe_set [Hex_cell.of_string "c4"];;

let list_for_right_eyed_claw = [1;2;3];;
let helper_for_right_eyed_claw = 
 Hex_strategy_static_constructor_t.Exhaustive_Disjunction(Image.image Hex_cell.of_string 
 ["d3";"c3";"b3"]);;



let fles_for_right_eyed_claw=
   create1 helper_for_right_eyed_claw "" list_for_right_eyed_claw;;

let ipairs_in_right_eyed_upwards_claw =
   let (Hex_cell_set_t.S pp )=
     Hex_flattened_end_strategy.passive_part fles_for_right_eyed_claw in 
   Ordered.sort Total_ordering.standard2 (Image.image Hex_cell.to_int_pair pp);;

(* Explaining a left-eyed upper upwards claw *)   


let for_strat5=
  molecular_with_actives  ["b3";"b4"] 
   ["a2","c2";"a1","b1";"c1","d1";"a3","b2"] ;;
let strat5_constructor = mol for_strat5;;
let fles5=create1 strat5_constructor "" [];;


let for_strat6=
  molecular_with_actives  ["c3";"b4"] 
   ["b2","e2";"b1","c1";"e1","f1";
     "b3","c2";"d2","d3"] ;;
let strat6_constructor = mol for_strat6;;
let fles6=create1 strat6_constructor "" [];;


let for_strat7=
  molecular_with_actives  ["b4";"d3"] 
   ["d2","f2";"d1","e1";"f1","g1";
     "c3","c4";"e2","e3"] ;;
let strat7_constructor = mol for_strat7;;
let fles7=create1 strat7_constructor "" [];;

let lonely_b4=Hex_cell_set.safe_set [Hex_cell.of_string "b4"];;

let list_for_left_eyed_claw = [5;6;7];;
let helper_for_left_eyed_claw = 
 Hex_strategy_static_constructor_t.Exhaustive_Disjunction(Image.image Hex_cell.of_string 
 ["b3";"c3";"d3"]);;

let fles_for_left_eyed_claw=
   create1 helper_for_left_eyed_claw "" list_for_left_eyed_claw;;

let ipairs_in_left_eyed_upwards_claw =
   let (Hex_cell_set_t.S pp )=
     Hex_flattened_end_strategy.passive_part fles_for_left_eyed_claw in 
   Ordered.sort Total_ordering.standard2 (Image.image Hex_cell.to_int_pair pp);;

(*

One can make the following checks : 

let g1=Hex_explanation.ipairs_in_left_eyed_upwards_claw ;;
let g2=(Hex_movable_ground_linker_data.Private.eyed_claw left up (4,2)).Hex_movable_ground_linker_data_t.support;;
let check1=(g1=g2);;

let g3=Hex_explanation.ipairs_in_right_eyed_upwards_claw ;;
let g4=(Hex_movable_ground_linker_data.Private.eyed_claw right up (4,3)).Hex_movable_ground_linker_data_t.support;;
let check2=(g3=g4);;


*)


