(*

#use"lib/sudoku.ml";;

*)

type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 
   
type cell_state =
    Initialized of int 
   |Assumed of int 
   |Deduced of int * (cell list)
   |Usual of ((cell list) option) list ;;

type bare_grid = BG of cell_state list ;; 


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
       

end ;;   

module Cell_state = struct 

    let display = function
      Initialized(v)->string_of_int v
     |Assumed(v)->string_of_int v
     |Deduced(v,_)->string_of_int v
     |Usual(_)->" ";;


end ;;  

module Bare_Grid = struct 

  let possibilities (BG states) cell= 
     let idx = Cell.single_index cell in 
     match List.nth states (idx-1) with
      Initialized(v2)->[v2]
     |Assumed(v3)->[v3]
     |Deduced(v4,_)->[v4]
     |Usual(l)->
     let temp1 = Int_range.index_everything l in 
     List.filter_map (
       fun (v,opt)-> match opt with 
        None -> Some v 
       |Some _ -> None
     ) temp1 ;;

   let check_before_assignment (BG states) cell v = 
     let possible_values = possibilities (BG states) cell in 
     if not(List.mem v  possible_values)
     then let msg = "Incorrect assignment attempt at "^(Cell.to_short_string cell) in 
          let _ = (print_string msg;flush stdout) in 
          false
     else true ;;   
     
    
   let assign_and_update (BG old_states) cell0 v0 object0 prereqs0=
       let indexed_old_states = Int_range.index_everything old_states in 
       let new_states = Image.image (
         fun (idx,state) ->
            let cell = Cell.at_single_index idx in 
            if cell=cell0
            then object0
            else  
            if (cell=cell0)||(not(Cell.test_for_neighborhood cell0 cell)) 
            then state
            else
             match state with 
              Initialized _
             |Assumed _ 
             |Deduced(_,_)-> state 
             |Usual(l) ->
                let indexed_l = Int_range.index_everything l in 
                Usual(Image.image (
                  fun (v1,opt1)->
                    if v1<>v0 
                    then opt1
                    else match opt1 with 
                         Some(_)->opt1
                        |None -> Some prereqs0 
                ) indexed_l)  
       ) indexed_old_states in
       BG(new_states) ;; 
   
    let initialize grid cell0 v0 =
        if check_before_assignment grid cell0 v0
        then assign_and_update grid cell0 v0 (Initialized v0) [cell0]
        else grid;;  

    let assume grid cell0 v0 =
        if check_before_assignment grid cell0 v0
        then assign_and_update grid cell0 v0 (Assumed v0) [cell0]
        else grid;;  

    let fail_during_deduction grid cell = 
      let msg = "Incorrect deduction attempt at "^(Cell.to_short_string cell) in 
      let _ = (print_string msg;flush stdout) in 
      grid;;   

    let deduce grid cell0 =
      let (BG(states)) = grid in 
      let idx = Cell.single_index cell0 in 
      match List.nth states (idx-1) with
       Initialized(_)
      |Assumed(_)
      |Deduced(_,_)-> fail_during_deduction grid cell0
      |Usual(l)->  
        let temp1 = Int_range.index_everything l in 
        let (poss,imposs)=List.partition (fun (_v,opt)-> opt = None) temp1 in 
        if List.length(poss)<>1
        then fail_during_deduction grid cell0
        else 
        let v0=fst(List.hd poss) 
        and pre_prereqs0 = List.flatten(Image.image (fun (_v,opt)->
            Image.image Cell.single_index (Option.get opt) ) imposs) in 
        let prereqs0 = List.filter_map (
           fun idx->if List.mem idx pre_prereqs0 then Some(Cell.at_single_index idx) else None
        ) (Int_range.range 1 81) in 
        assign_and_update grid cell0 v0 (Deduced(v0,prereqs0)) prereqs0 ;;

        module Display = struct 

          let eval_small_grid_using_matrix_coordinates (BG(states)) (i,j) = 
             let idx = Cell.single_index (Cell.from_matrix_coordinates i j) in 
             Cell_state.display(List.nth states (idx-1));;
          
          let eval_large_grid_using_matrix bg large_i large_j =
              let small_i =  List_again.find_index_of_in large_i [2;3;4;6;7;8;10;11;12]
              and small_j =  List_again.find_index_of_in large_j [2;3;4;6;7;8;10;11;12] in 
          if (small_i<0)||(small_j<0)
          then "*"
          else eval_small_grid_using_matrix_coordinates bg (small_i,small_j);;
          
          let large_line vg large_i = String.concat "" 
            (Int_range.scale(eval_large_grid_using_matrix vg large_i) 1 13) ;;
            
          let large_lines vg = Int_range.scale (large_line vg) 1 13 ;;
      
          let large_grid vg = (String.concat "\n" (large_lines vg));;  
      
        end ;;  
      
      let to_string = Display.large_grid ;;   
      
      let show bg = print_string("\n\n\n"^(to_string bg)^"\n\n\n");flush stdout ;;  

      let origin = 
          let common = Usual (Int_range.scale (fun _->None) 1 9) in 
          BG(Int_range.scale (fun _->common) 1 81);;

end ;;   

