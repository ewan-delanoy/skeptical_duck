(*

#use"lib/sudoku.ml";;

*)

type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 
   
type inverse = IV of box * int ;; 

type deductor =
     Direct_ded of cell
    |Inverse_ded of inverse 
    |Inverse_for_inverse_ded of inverse * inverse ;; 

type cell_state =
    Initialized of int 
   |Assumed of int 
   |Deduced of cell * int * deductor
   |Usual of (cell option) list ;;

type bare_grid = BG of cell_state list ;; 

type grid_with_deductions = GWD of bare_grid * ((cell * int) list)  ;;

let i_order = Total_ordering.for_integers ;;
let i_fold_merge = Ordered.fold_merge i_order ;;

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
let fold_merge ll =
    let temp1 = Image.image (Image.image single_index) ll in 
    Image.image at_single_index (i_fold_merge temp1) ;; 

let possibilities  l= 
   let temp1 = Int_range.index_everything l in 
   List.filter_map (
     fun (v,opt)-> match opt with 
      None -> Some v 
     |Some _ -> None
   ) temp1 ;;      

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
   |Deduced(_,v,_)->string_of_int v
   |Usual(_)->" ";;

  let new_state v0 acting_cell state =   
    match state with 
    Initialized _
   |Assumed _ 
   |Deduced(_,_,_)-> state 
   |Usual(l) ->
      let indexed_l = Int_range.index_everything l in 
      Usual(Image.image (
        fun (v1,opt1)->
          if v1<>v0 
          then opt1
          else match opt1 with 
               Some(_)->opt1
              |None -> Some acting_cell 
      ) indexed_l);;

  let possibilities = function 
   Initialized(v2)->[v2]
  |Assumed(v3)->[v3]
  |Deduced(_,v4,_)->[v4]
  |Usual(l)-> Cell.possibilities l ;;  

  let to_direct_deduction_opt cell0 = function 
     Initialized(_)
    |Assumed(_)
    |Deduced(_,_,_)-> None
    |Usual(l)->  
      let temp1 = Int_range.index_everything l in 
    let poss=List.filter (fun (_v,opt)-> opt = None) temp1 in 
    if List.length(poss)<>1
    then None
    else 
    let v0=fst(List.hd poss) in 
    Some(cell0,v0);; 


end ;;  

module Bare_grid = struct 

  let constructor l= BG l ;; 
  let states_in_bare_grid (BG l) = l;;

  let possibilities bg cell= 
    let states= states_in_bare_grid bg in 
     let idx = Cell.single_index cell in 
     Cell_state.possibilities (List.nth states (idx-1));;

   let check_before_assignment bg cell v = 
     let possible_values = possibilities bg cell in 
     if not(List.mem v  possible_values)
     then let msg = "Incorrect assignment attempt at "^(Cell.to_short_string cell) in 
          let _ = (print_string msg;flush stdout) in 
          false
     else true ;;   
     
   

   let assign_and_update bg cell0 v0 object0 = 
       let old_states = states_in_bare_grid bg in 
       let indexed_old_states = Int_range.index_everything old_states 
       and ref_for_deds=ref [] in 
       let new_states = Image.image (
         fun (idx,state) ->
            let cell = Cell.at_single_index idx in 
            if cell=cell0
            then object0
            else  
            if (cell=cell0)||(not(Cell.test_for_neighborhood cell0 cell)) 
            then state
            else
            let new_state = Cell_state.new_state v0 cell state in   
            let _ =(
                if ((Cell_state.to_direct_deduction_opt cell state)=None)
                then  
               match Cell_state.to_direct_deduction_opt cell new_state with 
                None -> ()
                |Some (cell1,v1) ->  ref_for_deds:= (cell1,v1) :: (!ref_for_deds)
            ) in 
             new_state  
       ) indexed_old_states in
       (constructor new_states,List.rev (!ref_for_deds)) ;; 
   
    let initialize_single_cell bg cell0 v0 =
        if check_before_assignment bg cell0 v0
        then assign_and_update bg cell0 v0 (Initialized v0) 
        else (bg,[]);;  

    let assume bg cell0 v0 =
        if check_before_assignment bg cell0 v0
        then assign_and_update bg cell0 v0 (Assumed v0) 
        else (bg,[]);;  

        module Display = struct 

          let eval_small_grid_using_matrix_coordinates bg special_cells (i,j) = 
             let states = states_in_bare_grid bg in 
             let cell = Cell.from_matrix_coordinates i j in 
             if List.mem cell special_cells 
             then "#"
             else 
             let idx = Cell.single_index (Cell.from_matrix_coordinates i j) in 
             Cell_state.display(List.nth states (idx-1));;
          
          let eval_large_grid_using_matrix bg special_cells large_i large_j =
              let small_i =  List_again.find_index_of_in large_i [2;3;4;6;7;8;10;11;12]
              and small_j =  List_again.find_index_of_in large_j [2;3;4;6;7;8;10;11;12] in 
          if (small_i<0)||(small_j<0)
          then "*"
          else eval_small_grid_using_matrix_coordinates bg special_cells (small_i,small_j);;
          
          let large_line bg special_cells large_i = String.concat "" 
            (Int_range.scale(eval_large_grid_using_matrix bg special_cells  large_i) 1 13) ;;
            
          let large_lines bg special_cells  = Int_range.scale (large_line bg special_cells ) 1 13 ;;
      
          let large_grid bg special_cells  = (String.concat "\n" (large_lines bg special_cells ));;  
      
        end ;;  

      let to_string bg special_cells = (Display.large_grid bg special_cells);;    

      let origin = 
          let common = Usual (Int_range.scale (fun _->None) 1 9) in 
          constructor(Int_range.scale (fun _->common) 1 81);;

    let minimizers bg = 
      let states = states_in_bare_grid bg in 
      let temp1 = List.combine Cell.all states in 
      let temp2 = List.filter_map (fun (cell,state)->
          match state with   
          Initialized(_)
         |Assumed(_)
         |Deduced(_)-> None
         |Usual(l)-> Some(cell,Cell.possibilities l)
      ) temp1 in 
      let (_,sols) = Min.minimize_it_with_care (fun (_cell,l)->List.length l) temp2 in 
      (Min.minimize_it_with_care (fun (_cell,l)->List.length l) temp2,sols) ;; 

    let possibilities_for_inverse bg (IV(box,v)) = 
        List.filter (fun cell->List.mem v (possibilities bg cell)) ( Box.content box) ;;
 
    let analysis_for_double_inverse bg (iv1,iv2) = 
        let (IV(_box1,v1)) = iv1 in 
        let cases = Image.image (
          fun cell -> 
            let (new_bg,_) = assume bg cell v1 in 
            (cell,possibilities_for_inverse new_bg iv2)
        ) (possibilities_for_inverse bg iv1) in 
        (Cell.fold_merge (Image.image snd cases),cases);;

    let check_before_inverse_deduction bg iv = 
          let l = possibilities_for_inverse bg iv in 
          if List.length(l)<>1
          then let msg = "Incorrect inverse deduction attempt " in 
               let _ = (print_string msg;flush stdout) in 
               false
          else true ;;       

    

end ;;   


module Grid_with_deductions = struct 

    let grid (GWD(bg,_)) = bg ;;
    let easy_deductions (GWD(_,deds)) = deds ;;
    let constructor bg deds = GWD(bg,deds) ;;

    let fail_during_deduction gwd cell = 
      let msg = "Incorrect deduction attempt at "^(Cell.to_short_string cell) in 
      let _ = (print_string msg;flush stdout) in 
      gwd;;   

    let deduce_directly gwd cell0 =
      let old_grid = grid gwd  in 
      let states = Bare_grid.states_in_bare_grid old_grid in 
      let idx = Cell.single_index cell0 in 
      match Cell_state.to_direct_deduction_opt cell0 (List.nth states (idx-1)) with
       None -> fail_during_deduction gwd cell0
      |Some(cell0,v0) ->
          let formal_ded = Deduced(cell0,v0,Direct_ded(cell0)) in 
          let (new_grid,new_direct_deds)=Bare_grid.assign_and_update old_grid cell0 v0 formal_ded in 
          constructor new_grid ((easy_deductions gwd)@(new_direct_deds));;

      let initialize_single_cell gwd cell0 v0 = 
        let old_grid = grid gwd in
        let (new_grid,new_direct_deds)=Bare_grid.initialize_single_cell old_grid cell0 v0  in 
        constructor new_grid ((easy_deductions gwd)@(new_direct_deds));; 
    
      let assume gwd cell0 v0 = 
          let old_grid = grid gwd in
          let (new_grid,new_direct_deds)=Bare_grid.assume old_grid cell0 v0  in 
          constructor new_grid ((easy_deductions gwd)@(new_direct_deds));;   

      module Display = struct 

        let to_string (cell,v0) = (Cell.to_short_string(cell))^" -> "^(string_of_int v0) ;;
        let list_to_string l = String.concat " , " (Image.image to_string l) ;; 
            
      end ;; 
            


      let to_string gwd special_cells = (Bare_grid.to_string (grid gwd) special_cells)^"\n\n"^
             (Display.list_to_string(easy_deductions gwd));;   
      
      let to_surrounded_string gwd special_cells = "\n\n\n"^(to_string gwd special_cells)^"\n\n\n" ;;  

      let print_out (fmt:Format.formatter) bg=
      Format.fprintf fmt "@[%s@]" (to_surrounded_string bg []);;

      let origin = constructor  Bare_grid.origin [] ;; 
        

      let initialize_with l =
         let temp1 = Int_range.index_everything l in 
         let temp2 = List.filter(fun (_idx,v)->(v>=1)&&(v<=9)) temp1
         and walker = ref origin in 
         let apply=(fun (idx,v)->
            let cell = Cell.at_single_index idx in
            walker:=initialize_single_cell (!walker) cell v
          ) in 
         let _ = List.iter apply temp2 in 
         !walker ;;  
                
    

    let minimizers gwd = 
      let grid = grid gwd in 
      let (answer,sols) = Bare_grid.minimizers grid in 
      let msg = to_surrounded_string gwd (Image.image fst sols) in 
      let _=(print_string msg;flush stdout) in
      answer ;; 

    let deduce_directly_several bg cells = List.fold_left deduce_directly bg cells ;;   


end ;;   






