(* 

#use"Hex_analysis/hex_kite_element.ml";;

*)

exception Doubled_cell_in_bridge of Hex_cell_t.t ;; 

module Private = struct 

let level = function 
   Hex_kite_element_t.Active_cell(_)->1
  |Bridge(_,_)->2;;

let unveil = function 
   Hex_kite_element_t.Active_cell(cell)->(cell,cell)
  |Bridge(cell1,cell2)->(cell1,cell2);;

(*
let increment_unveil = function 
    Hex_kite_element_t.Eyed_claw(_,_,cell) -> (cell,cell)
   |Noneyed_claw(_,_,cell) -> (cell,cell)
   |Pyramid(_,cell) -> (cell,cell);;
*)

let is_active = function 
    Hex_kite_element_t.Active_cell(cell)->true
   |Bridge(cell1,cell2)->false;;   

(*
let increment_is_active = function 
    Hex_kite_element_t.Eyed_claw(_,_,_) -> false
   |Noneyed_claw(_,_,_) -> false
   |Pyramid(_,_) -> false;;   
*)

let support = function 
    Hex_kite_element_t.Active_cell(cell)-> [cell]
   |Bridge(cell1,cell2)->[cell1;cell2];;   



end ;;

let active_cell cell = Hex_kite_element_t.Active_cell(cell);;

let bridge (cell1,cell2) = match Hex_cell.cmp cell1 cell2 with 
   Total_ordering.Lower -> Hex_kite_element_t.Bridge(cell1,cell2)
   |Equal -> raise(Doubled_cell_in_bridge(cell1))
   |Greater -> Hex_kite_element_t.Bridge(cell2,cell1);;

(*
let eyed_claw d1 d2 cell= Hex_kite_element_t.Eyed_claw(d1,d2,cell);;
*)

let check_compatiblity end_of_battle elt = 
 let expected_result = (
     if Private.is_active elt 
     then Hex_eob_result_t.Ally_territory
     else Hex_eob_result_t.Unoccupied) in 
 List.for_all (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell)=expected_result ) (Private.support elt);;   

let cmp = ((fun elt1 elt2->
   let i1=Private.level elt1 in 
   let trial1 = Total_ordering.standard i1 (Private.level elt2) in 
   if trial1 <> Total_ordering.Equal then trial1 else 
   Total_ordering.standard (Private.unveil elt1) (Private.unveil elt2) 
):> Hex_kite_element_t.t Total_ordering.t);;


let is_final (dim,direction) elt = 
   let opp = Hex_cardinal_direction.opposite direction in 
   match elt with  
   Hex_kite_element_t.Active_cell(cell)->
     Hex_cardinal_direction.test_for_border (dim,opp) cell
  |Bridge(cell1,cell2)->
      List.for_all (Hex_cardinal_direction.test_for_border (dim,opp)) [cell1;cell2];;


let neighbors dim = function 
   Hex_kite_element_t.Active_cell(cell)->
      let temp1=Hex_bridge.bridges_touching_a_cell dim cell in 
      Image.image bridge temp1
  |Bridge(cell1,cell2)->
      let temp1=Hex_bridge.cells_touching_a_bridge dim (cell1,cell2) in 
      Hex_cell_set.image active_cell temp1;;

(*
neighbors Hex_dimension.eleven (active_cell(Hex_cell.of_string "f7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "g7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "g6"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "f6"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "e7",Hex_cell.of_string "f7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "e8",Hex_cell.of_string "f7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "f8"));; 
*)

let support elt = Hex_cell_set.safe_set (Private.support elt);;

let to_atomic_linker = function 
    Hex_kite_element_t.Active_cell(cell)->None
   |Bridge(cell1,cell2)->Some(Hex_atomic_linker.pair(cell1,cell2));;                    