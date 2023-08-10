(*

#use"lib/sudoku.ml";;

*)

type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 
   


type deduction = D of cell * int * (cell list) ;; 

type cell_state =
    Initialized of int 
   |Assumed of int 
   |Deduced of deduction
   |Usual of ((cell list) option) list ;;

type bare_grid = BG of cell_state list * (deduction list);; 


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


module Deduction = struct 

let vaalue (D(_,v0,_)) = v0 ;; 

let of_usual_list_opt cell0 l =
  let temp1 = Int_range.index_everything l in 
  let (poss,imposs)=List.partition (fun (_v,opt)-> opt = None) temp1 in 
  if List.length(poss)<>1
  then None
  else 
  let v0=fst(List.hd poss) in 
  Some(D(cell0,v0,Cell.fold_merge(Image.image(fun (_v,opt)->Option.get opt) imposs)));;

let update_list updater l_to_be_updated =
    let (D(cell,_,_)) = updater in 
    List.filter (fun (D(cell2,_,_))->cell2<>cell) l_to_be_updated ;;

let to_string (D(cell,v0,_)) = (Cell.to_short_string(cell))^" -> "^(string_of_int v0) ;;
let list_to_string l = String.concat " , " (Image.image to_string l) ;; 

let push_if_necessary updater l_to_be_updated =
  let (D(cell,_,_)) = updater in 
  if List.exists (fun (D(cell2,_,_))->cell2=cell) l_to_be_updated 
  then l_to_be_updated
  else updater :: l_to_be_updated ;;

end ;;  

module Cell_state = struct 

  let display = function
    Initialized(v)->string_of_int v
   |Assumed(v)->string_of_int v
   |Deduced(D(_,v,_))->string_of_int v
   |Usual(_)->" ";;

  let new_state v0 prereqs0 state =   
    match state with 
    Initialized _
   |Assumed _ 
   |Deduced(_)-> state 
   |Usual(l) ->
      let indexed_l = Int_range.index_everything l in 
      Usual(Image.image (
        fun (v1,opt1)->
          if v1<>v0 
          then opt1
          else match opt1 with 
               Some(_)->opt1
              |None -> Some prereqs0 
      ) indexed_l);;

  let possibilities = function 
   Initialized(v2)->[v2]
  |Assumed(v3)->[v3]
  |Deduced(ded)->[Deduction.vaalue ded]
  |Usual(l)->
  let temp1 = Int_range.index_everything l in 
  List.filter_map (
    fun (v,opt)-> match opt with 
     None -> Some v 
    |Some _ -> None
  ) temp1 ;;  

  let to_deduction_opt cell0 = function 
     Initialized(_)
    |Assumed(_)
    |Deduced(_)-> None
    |Usual(l)->  Deduction.of_usual_list_opt cell0 l;;

  let update_list_of_deductions updater l_to_be_updated = match updater with 
    Initialized(_)
   |Assumed(_)
   |Usual(_)-> l_to_be_updated
   |Deduced(ded)-> Deduction.update_list ded l_to_be_updated ;;
  

end ;;  


module Bare_Grid = struct 

  let possibilities (BG (states,_)) cell= 
     let idx = Cell.single_index cell in 
     Cell_state.possibilities (List.nth states (idx-1));;

   let check_before_assignment bg cell v = 
     let possible_values = possibilities bg cell in 
     if not(List.mem v  possible_values)
     then let msg = "Incorrect assignment attempt at "^(Cell.to_short_string cell) in 
          let _ = (print_string msg;flush stdout) in 
          false
     else true ;;   
     
   

   let assign_and_update (BG (old_states,old_deds)) cell0 v0 object0 prereqs0 =
       let indexed_old_states = Int_range.index_everything old_states 
       and ref_for_new_deds=ref[] in 
       let new_states = Image.image (
         fun (idx,state) ->
            let cell = Cell.at_single_index idx in 
            if cell=cell0
            then object0
            else  
            if (cell=cell0)||(not(Cell.test_for_neighborhood cell0 cell)) 
            then state
            else
            let new_state = Cell_state.new_state v0 prereqs0 state in   
            let _ =(match Cell_state.to_deduction_opt cell new_state with 
                None -> ()
                |Some new_ded ->  ref_for_new_deds:=
                  Deduction.push_if_necessary  new_ded (!ref_for_new_deds)
            ) in 
             new_state  
       ) indexed_old_states in
       let new_deds = old_deds @ (List.rev(!ref_for_new_deds)) in 
       BG(new_states,new_deds) ;; 
   
    let initialize_single_cell grid cell0 v0 =
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
      let (BG(states,_)) = grid in 
      let idx = Cell.single_index cell0 in 
      match Cell_state.to_deduction_opt cell0 (List.nth states (idx-1)) with
       None -> fail_during_deduction grid cell0
      |Some(ded) ->
          let (D(_,v0,prereqs0)) = ded in  
          let (BG(states,old_deds))=assign_and_update grid cell0 v0 (Deduced(ded)) prereqs0 in 
          let new_deds = Deduction.update_list ded old_deds  in 
          BG(states,new_deds);;

        module Display = struct 

          let eval_small_grid_using_matrix_coordinates (BG(states,_)) (i,j) = 
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
      
        let easy_deductions (BG(_,deds)) = deds ;;    

      let to_string bg = (Display.large_grid bg)^"\n\n"^
             (Deduction.list_to_string(easy_deductions bg));;   
      
      let to_surrounded_string bg = "\n\n\n"^(to_string bg)^"\n\n\n" ;;  

      let origin = 
          let common = Usual (Int_range.scale (fun _->None) 1 9) in 
          BG(Int_range.scale (fun _->common) 1 81,[]);;

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
                
    let print_out (fmt:Format.formatter) bg=
         Format.fprintf fmt "@[%s@]" (to_surrounded_string bg);;

end ;;   

