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

end ;;

let active_cell cell = Hex_kite_element_t.Active_cell(cell);;

let bridge (cell1,cell2) = match Hex_cell.cmp cell1 cell2 with 
   Total_ordering.Lower -> Hex_kite_element_t.Bridge(cell1,cell2)
   |Equal -> raise(Doubled_cell_in_bridge(cell1))
   |Greater -> Hex_kite_element_t.Bridge(cell2,cell1);;

let cmp = ((fun elt1 elt2->
   let i1=Private.level elt1 in 
   let trial1 = Total_ordering.standard i1 (Private.level elt2) in 
   if trial1 <> Total_ordering.Equal then trial1 else 
   Total_ordering.standard (Private.unveil elt1) (Private.unveil elt2) 
):> Hex_kite_element_t.t Total_ordering.t);;

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

