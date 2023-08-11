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
    |Inverse_for_inverse_ded of inverse * inverse 
    |Inverse_for_direct_ded of inverse * cell;; 

type cell_state =
    Initialized of int 
   |Assumed of int 
   |Deduced of cell * int * deductor
   |Usual of (cell option) list ;;

type bare_grid = BG of cell_state list ;; 

type grid_with_deductions = GWD of bare_grid * ((cell * int) list) * (cell list) ;;

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
       
  let to_short_string = function 
      Row (i) -> "Row("^(string_of_int i)^")"
     |Column (j) -> "Column("^(string_of_int j)^")"
     |Square (k) -> "Square("^(string_of_int k)^")";; 
           

end ;;   


module Inverse = struct 

let to_short_string (IV(box,v))="IV("^(Box.to_short_string box)^","^(string_of_int v)^")";; 

end ;;  

module Deductor = struct 

let to_short_string = function

    Direct_ded (cell)-> "Direct_ded("^(Cell.to_short_string cell)^")"
  |Inverse_ded (iv) -> "Inverse_ded("^(Inverse.to_short_string iv)^")"
  |Inverse_for_inverse_ded(iv1,iv2) ->  
    "Inverse_for_inverse_ded("^(Inverse.to_short_string iv1)^","^(Inverse.to_short_string iv2)^")" 
  |Inverse_for_direct_ded(iv,cell) ->  
      "Inverse_for_direct_ded("^(Inverse.to_short_string iv)^","^(Cell.to_short_string cell)^")";; 

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

  let to_breakdown_opt cell0 = function 
    Initialized(_)
   |Assumed(_)
   |Deduced(_,_,_)-> None
   |Usual(l)->  
   if List.exists(fun opt-> opt = None) l
   then None
   else Some(cell0);;   

   let is_yet_undecided = function 
   Initialized(_)
  |Assumed(_)
  |Deduced(_,_,_)-> false
  |Usual(_)->  true;;

end ;;  

module Bare_grid = struct 

  module Private = struct 

  let constructor l= BG l ;; 
  let states_in_bare_grid (BG l) = l;;

  let possibilities_for_cell bg cell= 
    let states= states_in_bare_grid bg in 
     let idx = Cell.single_index cell in 
     Cell_state.possibilities (List.nth states (idx-1));;

   let check_before_assignment bg cell v = 
     let possible_values = possibilities_for_cell bg cell in 
     if not(List.mem v  possible_values)
     then let msg = "Incorrect assignment attempt at "^(Cell.to_short_string cell) in 
          let _ = (print_string msg;flush stdout) in 
          false
     else true ;;   

   let assign_and_update bg cell0 v0 explanation0 = 
       if not(check_before_assignment bg cell0 v0) 
       then bg
       else
       let old_states = states_in_bare_grid bg in 
       let indexed_old_states = Int_range.index_everything old_states in 
       let new_states = Image.image (
         fun (idx,state) ->
            let cell = Cell.at_single_index idx in 
            if cell=cell0
            then explanation0
            else  
            if not(Cell.test_for_neighborhood cell0 cell)
            then state
            else Cell_state.new_state v0 cell state 
       ) indexed_old_states in
       constructor new_states ;; 
   
    let compute_easy_deductions bg = 
      let base= List.combine Cell.all (states_in_bare_grid bg) in 
       List.filter_map (fun (cell,state)->Cell_state.to_direct_deduction_opt cell state) base ;; 

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

      let compute_breakdowns bg = 
        let base= List.combine Cell.all (states_in_bare_grid bg) in 
         List.filter_map (fun (cell,state)->Cell_state.to_breakdown_opt cell state) base ;; 

      let get_state bg cell = 
         List.nth (states_in_bare_grid bg) ((Cell.single_index cell)-1);;   

    end ;;  

    let assign_and_update= Private.assign_and_update ;; 
    let compute_easy_deductions = Private.compute_easy_deductions ;; 
    let compute_breakdowns = Private.compute_breakdowns ;; 
    let get_state = Private.get_state ;; 
    let minimizers= Private.minimizers ;; 
    let origin = Private.origin ;; 
    let states_in_bare_grid = Private.states_in_bare_grid ;;
    let to_string = Private.to_string ;; 

end ;;   


module Possibilities = struct 

  module Private = struct 

  let for_cell bg cell= 
    let states= Bare_grid.states_in_bare_grid bg in 
     let idx = Cell.single_index cell in 
     Cell_state.possibilities (List.nth states (idx-1));;

  let for_inverse bg (IV(box,v)) = 
      List.filter (fun cell->List.mem v (for_cell bg cell)) ( Box.content box) ;;

  let analysis_for_double_inverse bg iv1 iv2 = 
        let (IV(_box1,v1)) = iv1 in 
        let cases = Image.image (
          fun cell -> 
            let new_bg = Bare_grid.assign_and_update bg cell v1 (Assumed v1) in 
            (cell,for_inverse new_bg iv2)
        ) (for_inverse bg iv1) in 
    (Cell.fold_merge (Image.image snd cases),cases);;

  let for_double_inverse bg iv1 iv2 = fst(analysis_for_double_inverse bg iv1 iv2) ;; 

  let analysis_for_ifd bg iv1 cell2 = 
    let (IV(_box1,v1)) = iv1 in 
    let cases = Image.image (
      fun cell -> 
        let new_bg = Bare_grid.assign_and_update bg cell v1 (Assumed v1) in 
        (cell,for_cell new_bg cell2)
    ) (for_inverse bg iv1) in 
  (i_fold_merge (Image.image snd cases),cases);;

  let for_ifd bg iv1 cell2 = fst(analysis_for_ifd bg iv1 cell2) ;;

  let for_deductor bg = function 
    Direct_ded (cell) -> Image.image (fun v->(cell,v)) (for_cell bg cell)
  |Inverse_ded (iv) -> let (IV(_,v))=iv in Image.image (fun cell->(cell,v)) (for_inverse bg iv) 
  |Inverse_for_inverse_ded (iv1,iv2) -> 
    let (IV(_,v2))=iv2 in Image.image (fun cell->(cell,v2)) (for_double_inverse bg iv1 iv2)
  |Inverse_for_direct_ded (iv1,cell2) ->  
    Image.image (fun v2->(cell2,v2)) (for_ifd bg iv1 cell2)
  ;; 

  end ;;
  
  let for_cell = Private.for_cell ;;
  let for_deductor = Private.for_deductor ;; 

end ;;  


module Grid = struct 

    module Private = struct 

    let grid (GWD(bg,_,_)) = bg ;;
    let easy_deductions (GWD(_,deds,_)) = deds ;;
    let breakdowns (GWD(_,_,bks)) = bks ;;
    let constructor bg = GWD(bg,Bare_grid.compute_easy_deductions bg,Bare_grid.compute_breakdowns bg) ;;
      
      let assign_and_update gwd cell0 v0 explanation0= 
        let old_grid = grid gwd in
        let new_grid=Bare_grid.assign_and_update old_grid cell0 v0 explanation0  in 
        constructor new_grid ;; 


      let initialize_single_cell bg cell0 v0 = assign_and_update bg cell0 v0 (Initialized v0) ;;

      let assume bg cell0 v0 =assign_and_update bg cell0 v0 (Assumed v0) ;;      


      module Display = struct 

        let to_string (cell,v0) = (Cell.to_short_string(cell))^" -> "^(string_of_int v0) ;;
        let pair_list_to_string l = String.concat " , " (Image.image to_string l) ;; 
        let cell_list_to_string cells =
           if cells = [] then "" else 
           "\n Contradictions : "^(String.concat "," (Image.image Cell.to_short_string cells)) ;;    

      end ;; 
            


      let to_string gwd special_cells = (Bare_grid.to_string (grid gwd) special_cells)^"\n\n"^
             (Display.pair_list_to_string(easy_deductions gwd))^
            (Display.cell_list_to_string(breakdowns gwd));;   
      
      let to_surrounded_string gwd special_cells = "\n\n\n"^(to_string gwd special_cells)^"\n\n\n" ;;  

      let print_out (fmt:Format.formatter) bg=
      Format.fprintf fmt "@[%s@]" (to_surrounded_string bg []);;

      let origin = constructor  Bare_grid.origin  ;; 
        

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

    let fail_during_deduction gwd ded = 
        let msg = "Incorrect deduction attempt. "^(Deductor.to_short_string ded) in 
        let _ = (print_string msg;flush stdout) in 
        gwd;;   
  
    let deduce gwd ded =
      let old_grid = grid gwd  in 
      let poss = Possibilities.for_deductor old_grid ded  in 
      if List.length(poss)<>1
      then fail_during_deduction gwd ded 
      else 
      let (cell0,v0)=List.hd poss in 
      assign_and_update gwd cell0 v0 (Deduced(cell0,v0,Direct_ded(cell0)))  ;;
    
    let deduce_several gwd deds = List.fold_left deduce gwd deds ;;   
    
    let deduce_several_directly gwd cells =
        let deds = Image.image (fun cell->Direct_ded cell) cells in 
        deduce_several gwd deds ;;
     
    let get_state gwd cell = Bare_grid.get_state (grid gwd) cell ;; 

    end ;; 
    
    let assume = Private.assume ;; 
    let deduce_several = Private.deduce_several ;;
    let deduce_several_directly = Private.deduce_several_directly ;;
    let get_state = Private.get_state ;; 
    let initialize_with = Private.initialize_with ;;
    let minimizers = Private.minimizers ;; 
    let possibilities gwd cell = Possibilities.for_cell (Private.grid gwd) cell ;;
    let possibilities_for_deductor gwd ded= Possibilities.for_deductor (Private.grid gwd) ded ;;
    let print_out = Private.print_out ;; 

end ;;   

module Helper = struct 

let all_inverses = 
   let base = Cartesian.product Box.all (Int_range.range 1 9) in 
   Image.image (fun (box,k)->IV(box,k)) base;;  

let all_helpers =
    (Image.image (fun cell->Direct_ded(cell)) Cell.all) 
    @ (Image.image (fun iv->Inverse_ded(iv)) all_inverses) ;;



let base1 gwd = List.filter_map
    (fun helper->
      let poss = Grid.possibilities_for_deductor gwd helper in 
      if List.length(poss)<>1
      then Some(helper,poss)
      else let (cell,_) = List.hd poss in 
           if Cell_state.is_yet_undecided(Grid.get_state gwd cell)
           then Some(helper,poss)        
          else None) all_helpers ;;

let level0 gwd = Min.minimize_it_with_care (fun (_,l)->List.length l) (base1 gwd);;

let use_one_shield_step_1 gwd helped (helper,poss)  =
  let cases = Image.image (
    fun (cell,v) -> 
      let new_gwd = Grid.assume gwd cell v  in 
      (cell,Grid.possibilities_for_deductor new_gwd helped)
  ) poss in 
(helper,Ordered.sort Total_ordering.standard (List.flatten(Image.image snd cases)),cases);;      

let use_one_shield_step_2 (gwd,base1_for_gwd) helped =
   let temp1 = Image.image (use_one_shield_step_1 gwd helped) base1_for_gwd in 
   List.hd(snd(Min.minimize_it_with_care (fun (_,l,_)->List.length l) temp1)) ;;

let base2 gwd = 
   let base1_for_gwd = base1 gwd in 
   Image.image (
     fun (helped,_) -> (helped,use_one_shield_step_2 (gwd,base1_for_gwd) helped)
   ) base1_for_gwd ;;
       
let level1 gwd = 
    let (m,temp1) = Min.minimize_it_with_care (fun (_,(_,l,_))->List.length l) (base2 gwd) in 
     (m,Image.image (fun (h1,(h2,_,_))->(h1,h2)) temp1);;   
    

end ;;   




