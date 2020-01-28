(* 

#use"Hex_analysis/hex_kite_element.ml";;

*)

exception Doubled_cell_in_bridge of Hex_cell_t.t ;; 

module Private = struct 

let level = function 
   Hex_kite_element_t.Active_cell(_)->1
  |Bridge(_,_)->2
  |Hex_kite_element_t.Planar(plnr,_) -> 3 + (Hex_planar_linker.level plnr);;

let unveil = function 
   Hex_kite_element_t.Active_cell(cell)->(cell,cell)
  |Bridge(cell1,cell2)->(cell1,cell2)
  |Hex_kite_element_t.Planar(_,cell) -> (cell,cell);;


let is_active = function 
    Hex_kite_element_t.Active_cell(cell)->true
   |Bridge(cell1,cell2)->false
   |Hex_kite_element_t.Planar(_,cell) -> false ;;
  

let support = function 
    Hex_kite_element_t.Active_cell(cell)-> [cell]
   |Bridge(cell1,cell2)->[cell1;cell2]
   |Hex_kite_element_t.Planar(plnr,cell) -> 
     let (Hex_cell_set_t.S l)=Hex_planar_linker.support plnr cell in l;;

let to_readable_string = function
  Hex_kite_element_t.Active_cell(cell)-> Hex_cell.to_string cell
   |Bridge(cell1,cell2)->(Hex_cell.to_string cell1)^"-"^(Hex_cell.to_string cell2)
   |Hex_kite_element_t.Planar(plnr,cell) -> 
      (Hex_planar_linker.to_readable_string plnr)^"("^(Hex_cell.to_string cell)^")";;

end ;;

module Constructors = struct 

let active_cell cell = Hex_kite_element_t.Active_cell(cell);;

let bridge (cell1,cell2) = match Hex_cell.cmp cell1 cell2 with 
   Total_ordering.Lower -> Hex_kite_element_t.Bridge(cell1,cell2)
   |Equal -> raise(Doubled_cell_in_bridge(cell1))
   |Greater -> Hex_kite_element_t.Bridge(cell2,cell1);;


let planar dim plnr cell= 
   let _=Hex_planar_linker.check dim plnr cell in 
   Hex_kite_element_t.Planar(plnr,cell);;
   

end;;

let active_cell = Constructors.active_cell ;;
let bridge = Constructors.bridge ;;


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
     Hex_cardinal_direction.Border.test dim opp cell
  |Bridge(cell1,cell2)->
      List.for_all (Hex_cardinal_direction.Border.test  dim opp ) [cell1;cell2]
  |Hex_kite_element_t.Planar(plnr,_)-> ((Hex_planar_linker.ground plnr)=opp);;



let neighbors_for_element dim = function 
   Hex_kite_element_t.Active_cell(cell)->
      let temp1=Hex_cell.neighbors dim cell 
      and temp2=Hex_bridge.bridges_touching_a_cell dim cell 
      and temp3=Hex_planar_linker.unfold_all_around_cell dim cell in
      (Image.image active_cell temp1) 
      @(Image.image bridge temp2)
      @(Image.image (fun plnr ->Hex_kite_element_t.Planar(plnr,cell)) temp3)
   |Bridge(cell1,cell2)->
      let temp1=Hex_bridge.cells_touching_a_bridge dim (cell1,cell2) in 
      Hex_cell_set.image active_cell temp1
   |Hex_kite_element_t.Planar(plnr,cell)->[active_cell cell];;



(*
neighbors Hex_dimension.eleven (active_cell(Hex_cell.of_string "f7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "g7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "g6"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "f6"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "e7",Hex_cell.of_string "f7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "e8",Hex_cell.of_string "f7"));; 
neighbors Hex_dimension.eleven (bridge(Hex_cell.of_string "f7",Hex_cell.of_string "f8"));; 
*)

let neighbors_for_side formal_dim  side =
   let (Hex_dimension_t.D dim) = formal_dim in 
   let be = Hex_cardinal_direction.Border.enumerate formal_dim side in 
   let part1 = Ennig.doyle (fun idx->active_cell(be idx)) 1 dim 
   and part2 = Ennig.doyle (fun idx->bridge(be idx,be (idx+1)) ) 1 (dim-1)  
   and part3 = Image.image (fun (plnr,cell)->
       Hex_kite_element_t.Planar(plnr,cell)
   ) (Hex_planar_linker.unfold_all_around_side formal_dim side)   in 
   part1@part2@part3;;    

let planar = Constructors.planar ;;

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     


let support elt = Hex_cell_set.safe_set (Private.support elt);;


let to_molecular_linker = function 
    Hex_kite_element_t.Active_cell(cell)->None
   |Bridge(cell1,cell2)->Some(Hex_molecular_linker.constructor[Hex_atomic_linker.pair(cell1,cell2)])
   |Planar (plnr,cell) -> Some(Hex_planar_linker.to_molecular_linker plnr cell);;

let to_readable_string = Private.to_readable_string;;   