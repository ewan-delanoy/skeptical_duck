(*

#use"lib/sudoku.ml";;

*)



type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 


type forbidden_configuration = Fc of (cell * int) list ;;

type watcher_for_forbidden_configurations = 
  Wfc of (forbidden_configuration * forbidden_configuration) list ;;

type grid = G of 
  watcher_for_forbidden_configurations * 
  ((cell * ((int list) * bool)) list);;

type deduction_tip =
    Simple of cell
   |Indirect of box * int ;; 

type deduction = Ded of deduction_tip * (cell * int) ;;



let i_order = Total_ordering.for_integers ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_sort = Ordered.sort i_order ;;

module Cell = struct 

let from_matrix_coordinates i j = C(i,j) ;; 

let horizontal_coordinate (C (_i,j)) = j;; 
 
let vertical_coordinate  (C (i,_j)) = i;; 
 
let square_coordinate (C (i,j)) =
   (Basic.frac_ceiling j 3) + 3* ((Basic.frac_ceiling i 3)-1) ;;

let all = Image.image (fun (i,j)->C(i,j)) (Cartesian.square(Int_range.range 1 9)) ;;

let test_for_neighborhood c1 c2 = 
      ((horizontal_coordinate c1)=(horizontal_coordinate c2))
      ||
      ((vertical_coordinate c1)=(vertical_coordinate c2))
      ||
      ((square_coordinate c1)=(square_coordinate c2))
    ;;


let first_in_given_square square_idx = List.find (fun c->square_coordinate(c)=square_idx) all;;     
let single_index (C(i,j)) = j+9*(i-1) ;;
let at_single_index idx = let q = (idx-1)/9 in C(q+1,idx-9*q);;
let to_short_string (C(i,j)) = "("^(string_of_int i)^","^(string_of_int j)^")" ;;
 

end ;;

module Box = struct 

  let content = function 
   Row (i) -> Int_range.scale (fun j->Cell.from_matrix_coordinates i j) 1 9
  |Column (j) -> Int_range.scale (fun i->Cell.from_matrix_coordinates i j) 1 9
  |Square (k) -> 
     let (C(x0,y0)) = Cell.first_in_given_square k in 
     Image.image (fun (i,j)->Cell.from_matrix_coordinates i j)
     (Cartesian.product [x0;x0+1;x0+2] [y0;y0+1;y0+2]) 
     ;; 
     
  let all =
      (Int_range.scale (fun i->Row i) 1 9) 
      @  
      (Int_range.scale (fun j-> Column j) 1 9)
      @  
      (Int_range.scale (fun k-> Square k) 1 9)
       
  let to_short_string = function 
      Row (i) -> "Row("^(string_of_int i)^")"
     |Column (j) -> "Column("^(string_of_int j)^")"
     |Square (k) -> "Square("^(string_of_int k)^")";; 
           

end ;;   

module Forbidden_configuration = struct 

let assign fc   cell v = 
   let (Fc l)=fc in 
   match List.assoc_opt cell l with 
   None -> Some fc 
   |Some original_v ->
    if original_v=v 
    then Some(Fc(List.filter (fun (cell2,_)->cell2<>cell) l))
    else None ;;     

let is_realized (Fc l) = (l=[]) ;;

end ;;  

module Watcher_for_forbidden_configurations = struct 

let empty_one = Wfc [] ;;

let assign (Wfc old_whole) cell v = 
    let new_whole = List.filter_map (
        fun (original,old_state) ->
          Option.map (fun state->(original,state))
         (Forbidden_configuration.assign old_state cell v)
    ) old_whole in 
    Wfc new_whole ;;

let forbidden_values (Wfc l) cell = 
   i_sort( List.filter_map (
        fun (_original,Fc current_state) ->
         if List.length current_state <>1 
         then None 
         else 
         let (cell2,v)=List.hd current_state in 
         if cell2<>cell 
         then None 
         else Some v   
    ) l) ;;  


end ;;  

module Grid = struct 

exception Assign_exn of cell ;;  
exception Repeated_assignment_exn of cell ;;  


module Private = struct

let origin = 
  let small_base = Int_range.range 1 9 in 
  G(Watcher_for_forbidden_configurations.empty_one,
  Image.image (fun cell->(cell,(small_base,false))) Cell.all) ;;

let possibilities_at_cell (G (_wfc,l)) cell = fst(List.assoc cell l) ;;  

let cell_is_already_assigned (G (_wfc,l)) cell = snd(List.assoc cell l);;

let uncurried_assign gr (cell,v) = 
   let (G (wfc,l)) = gr in 
   let possibilities = possibilities_at_cell gr cell  in 
   let is_old = cell_is_already_assigned gr cell   in 
   if (not(List.mem v possibilities))
   then raise(Assign_exn(cell))
   else 
   if is_old
   then raise(Repeated_assignment_exn(cell))
   else  
   let new_l = Image.image (
     fun (cell2,(poss,is_old))->
        if cell2=cell then (cell2,([v],true)) else 
        if Cell.test_for_neighborhood cell cell2 
        then (cell2,(i_outsert v poss,is_old))
        else (cell2,(poss,is_old))       
   ) l in 
   G (Watcher_for_forbidden_configurations.assign wfc cell v,new_l) ;;  

let cells_with_fewest_possibilities gr = 
  let (G (_wfc,l)) = gr in 
  let temp1 = List.filter_map (
    fun (cell,(_poss,is_old)) ->
       if is_old then None else 
        Some(cell,possibilities_at_cell gr cell)
  ) l in 
  Min.minimize_it_with_care (fun (_cell,poss)->List.length poss) temp1 ;;

end ;;

let assign gr cell v = Private.uncurried_assign gr (cell,v) ;; 

let assign_several gr assignments = 
     List.fold_left  Private.uncurried_assign gr assignments ;; 

let assoc (G (_wfc,l)) cell = List.assoc cell l ;;  

let cells_with_fewest_possibilities = Private.cells_with_fewest_possibilities ;;
let initialize_with l =
    let temp1 = List.combine Cell.all l in 
    let temp2 = List.filter (fun (_cell,v)->v<>0) temp1 in 
    assign_several Private.origin temp2 ;; 

let low_hanging_fruit gr=    
  let (G (_wfc,l))=gr in 
  List.find_map (fun (cell,(_poss,is_old))->
     let poss = Private.possibilities_at_cell gr cell in 
    if (List.length(poss)<=1)&&(not is_old)
    then Some(cell,poss)
    else  None) l ;;

let possibilities_at_cell = Private.possibilities_at_cell ;; 

end ;;

module Display = struct 

module Private = struct 

      let eval_small_grid_using_matrix_coordinates gr (i,j) = 
         let cell = Cell.from_matrix_coordinates i j in 
         let (poss,is_old) = Grid.assoc gr cell in 
         let m = List.length poss in 
         if m = 0 then "B" else 
         if (m = 1)&&is_old then  string_of_int(List.hd poss) else
         " " ;; 

      let eval_large_grid_using_matrix gr large_i large_j =
          let small_i =  List_again.find_index_of_in large_i [2;3;4;6;7;8;10;11;12]
          and small_j =  List_again.find_index_of_in large_j [2;3;4;6;7;8;10;11;12] in 
      if (small_i<0)||(small_j<0)
      then "*"
      else eval_small_grid_using_matrix_coordinates gr (small_i,small_j);;
      
      let large_line bg large_i = String.concat "" 
        (Int_range.scale(eval_large_grid_using_matrix bg large_i) 1 13) ;;
        
      let large_lines bg = Int_range.scale (large_line bg ) 1 13 ;;
  
      let large_grid bg  = (String.concat "\n" (large_lines bg));;   

  let to_string bg = (large_grid bg);;    

  let to_surrounded_string gr = "\n\n\n"^(to_string gr)^"\n\n\n" ;;  
  
  

end ;; 

let print_out_grid (fmt:Format.formatter) gr=
  Format.fprintf fmt "@[%s@]" (Private.to_surrounded_string gr);;

let grid_to_string = Private.to_string ;; 


end ;;  

module Deduce = struct 

module Private = struct  

let test_for_indirect_deduction gr (box,v)=
  let cells = Box.content box in 
  let temp1 = List.filter_map 
    (fun cell->
        let poss = Grid.possibilities_at_cell gr cell in 
        if i_mem v poss
        then Some cell  
        else None) cells in
  if List.length(temp1)=1
  then ( let cell = List.hd temp1 in 
       let (_,is_old) = Grid.assoc gr cell in 
       if is_old
       then None  
       else Some(box,v,List.hd temp1) )
  else None ;;         
     
type walker = W of  grid * deduction list * cell option * bool ;;

exception Treat_simple_deduction_exn ;;   

let treat_simple_deduction  (W(gr,older_deds,_impossible_cell_opt,_end_reached)) cell0 poss0 = 
  let m0 = List.length(poss0) in 
  if m0=0 then (W(gr,older_deds,Some cell0,false)) else
  if m0=1 
  then let v0 = List.hd poss0 in 
     (W(Grid.assign gr cell0 v0,Ded(Simple cell0,(cell0,v0))::older_deds,None,false)) 
  else raise(Treat_simple_deduction_exn) ;; 


let pusher walker =
  let (W(gr,older_deds,impossible_cell_opt,end_reached)) = walker in 
  if end_reached then walker else
  if impossible_cell_opt <> None then W(gr,older_deds,impossible_cell_opt,true) else
  match Grid.low_hanging_fruit gr  with 
  Some(cell0,poss0)-> treat_simple_deduction walker cell0 poss0 
  |None ->
     let proposals = Cartesian.product Box.all (Int_range.range 1 9) in 
     (match List.find_map (test_for_indirect_deduction gr) proposals with 
       (Some(box1,v1,cell1)) ->
        W(Grid.assign gr cell1 v1,(Ded(Indirect(box1,v1),(cell1,v1)))::older_deds,None,false) 
       | None -> W(gr,older_deds,None,true) 
     );;
       
(* let immediate_simple_deductions gr = 
  let temp1 = Grid.low_hanging_fruit gr in 
  ()
;;  *)

let rec iterator walker =
  let (W(gr,older_deds,impossible_cell_opt,end_reached)) = walker in 
  if end_reached then (gr,impossible_cell_opt,List.rev older_deds) else
  iterator(pusher(walker)) ;;  

end ;;

let deduce_easily_as_much_as_possible gr = 
    Private.iterator(Private.W(gr,[],None,false))
;;  

end ;;   

(* 

open Sudoku ;; 
#install_printer Display.print_out_grid ;;

let g0 = Grid.initialize_with 
[
   0;0;0;  0;0;0;  0;0;0;
   0;0;0;  0;0;0;  0;0;0; 
   0;0;0;  0;0;0;  0;0;0; 

   0;0;0;  0;0;0;  0;0;0;
   0;0;0;  0;0;0;  0;0;0;
   0;0;0;  0;0;0;  0;0;0;

   0;0;0;  0;0;0;  0;0;0;
   0;0;0;  0;0;0;  0;0;0;
   0;0;0;  0;0;0;  0;0;0;

];; 

let (g1,contr_opt,deds) = Deduce.deduce_easily_as_much_as_possible g0 ;; 

*)