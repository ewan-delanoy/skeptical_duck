(*

#use"lib/sudoku.ml";;

*)



type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 

type grid = G of 
  (cell * ((int list) * bool)) list;;

type value_holder =
    Simple of cell
   |Indirect of box * int ;; 

type deduction = Ded of value_holder * (cell * int) ;;

type obstruction =
    Unassignable_cells of cell  list
   |Mutually_inconsistent_deductions of (cell * (int * value_holder list) list) list 
   |Mutually_inconsistent_extensions of ( (cell * int * (value_holder list)) * 
                            (cell * int * (value_holder list)) ) list;;

type raking_result =
    Smooth of  (cell * int * value_holder list) list
   |Obstruction of obstruction ;; 
   
type drill_element = {
    self_idx : int ;
    ancestor_idx : int option ;
    biography : (cell * int) list ;
    improvements : deduction list ;
    birth_state : grid ;
    current_state : grid ;
    forbidden_extensions : (cell * int) list;
} ;;

type drill = Dr of drill_element list ;;

let i_order = Total_ordering.for_integers ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_sort = Ordered.sort i_order ;;
let i_setminus = Ordered.setminus i_order ;;

module Cell = struct 

module Private = struct 

let single_index (C(i,j)) = j+9*(i-1) ;;
let at_single_index idx = let q = (idx-1)/9 in C(q+1,idx-9*q);;

let square_coordinate (C (i,j)) =
   (Basic.frac_ceiling j 3) + 3* ((Basic.frac_ceiling i 3)-1) ;;
let vertical_coordinate  (C (i,_j)) = i;; 

end ;;  

let all = Image.image (fun (i,j)->C(i,j)) (Cartesian.square(Int_range.range 1 9)) ;;

let at_single_index = Private.at_single_index ;;

let first_in_given_square square_idx = List.find (fun c->Private.square_coordinate(c)=square_idx) all;;     

let from_matrix_coordinates i j = C(i,j) ;; 

let horizontal_coordinate (C (_i,j)) = j;; 
 
let order = ((fun (C(i1,j1)) (C(i2,j2)) ->
    (Total_ordering.product i_order i_order) (i1,j1) (i2,j2) 
  ) : cell Total_ordering_t.t) ;;

let single_index = Private.single_index ;;
let square_coordinate = Private.square_coordinate ;;

let test_for_neighborhood c1 c2 = 
      ((horizontal_coordinate c1)=(horizontal_coordinate c2))
      ||
      ((Private.vertical_coordinate c1)=(Private.vertical_coordinate c2))
      ||
      ((square_coordinate c1)=(square_coordinate c2))
    ;;

let to_short_string (C(i,j)) = "("^(string_of_int i)^","^(string_of_int j)^")" ;;    

let translate_along_single_index d c = 
   let old_idx = Private.single_index c  in
   let attempted_new_idx = old_idx + d in 
   let new_idx =(
      if attempted_new_idx <= 81
      then attempted_new_idx
      else attempted_new_idx - 81
   ) in 
   Private.at_single_index new_idx ;;    

let vertical_coordinate  = Private.vertical_coordinate ;; 
 



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



module Value_holder = struct 

let all = 
   let from_cells = Image.image ( 
      fun cell -> Simple(cell)
   ) Cell.all 
   and from_pairs = Image.image ( 
      fun (bx,v) -> Indirect(bx,v)
   ) (Cartesian.product Box.all (Int_range.range 1 9)) in 
   from_cells @ from_pairs ;;


end ;;  

module Grid = struct 

exception Assign_exn of cell ;;  
exception Repeated_assignment_exn of cell ;;  


module Private = struct

let origin = 
  let small_base = Int_range.range 1 9 in 
  G(
  Image.image (fun cell->(cell,(small_base,false))) Cell.all) ;;

let possibilities_at_cell (G (l)) cell = 
   let temp1 = fst(List.assoc cell l) in 
  temp1 ;;  



let cell_is_already_assigned (G (l)) cell = snd(List.assoc cell l);;

let uncurried_assign gr (cell,v) = 
   let (G (l)) = gr in 
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
   G (new_l) ;;  

let cells_with_fewest_possibilities gr = 
  let (G (l)) = gr in 
  let temp1 = List.filter_map (
    fun (cell,(_poss,is_old)) ->
       if is_old then None else 
        Some(cell,possibilities_at_cell gr cell)
  ) l in 
  Min.minimize_it_with_care (fun (_cell,poss)->List.length poss) temp1 ;;

let assign_several gr assignments = 
     List.fold_left  uncurried_assign gr assignments ;; 

let assign_several_opt gr assignments = 
   try Some(assign_several gr assignments) with 
     Assign_exn _
   | Repeated_assignment_exn _ -> None ;; 

let freedom_left (G(l)) =
  List.length(List.filter (fun (_cell,(vals,_))->List.length(vals)>1) l) ;;

let living_cells gr =
  let (G(l)) = gr in 
  List.filter_map (fun (cell,(_vals,is_old))->
    if is_old then None else 
    let poss = possibilities_at_cell gr cell in    
    if List.length(poss)>1
    then Some cell  
    else None  ) l ;;

let expand_grid_at_cell cell gr = 
   let (G(l)) = gr in 
   let (_,is_old) = List.assoc cell l in 
   if is_old then [gr] else
   let poss = possibilities_at_cell gr cell in 
   Image.image (fun v->uncurried_assign gr (cell,v)) poss;;

let expand_grids_at_cell l_gr cell =
   List.flatten (Image.image (expand_grid_at_cell cell) l_gr) ;;   

let expand_grids_at_cells l_gr l_cell = 
  List.fold_left expand_grids_at_cell l_gr l_cell  ;;     


end ;;

let assign gr cell v = Private.uncurried_assign gr (cell,v) ;; 

let assign_if_unoccupied gr cell v = 
   try Private.uncurried_assign gr (cell,v) with 
    Repeated_assignment_exn(_) -> gr;; 

let assign_several gr assignments = 
     Private.assign_several gr assignments ;; 

let assign_several_opt gr assignments = 
     Private.assign_several_opt gr assignments ;;      

let assoc (G (l)) cell = List.assoc cell l ;;  

let cells_with_fewest_possibilities = Private.cells_with_fewest_possibilities ;;

let empty_grid = Private.origin ;;
    
let expand = Private.expand_grids_at_cells ;;

let freedom_left = Private.freedom_left ;;

let horizontal_summary (G(l)) = List.filter_map 
  (fun (cell,(poss,is_old)) ->
      if is_old then Some(cell,List.hd poss) else None 
   ) l;;

let initialize_with l =
    let temp1 = List.combine Cell.all l in 
    let temp2 = List.filter (fun (_cell,v)->v<>0) temp1 in 
    assign_several Private.origin temp2 ;; 

let living_cells = Private.living_cells ;;

let possibilities_at_cell = Private.possibilities_at_cell ;; 


end ;;

module Deduction = struct 

module Private = struct 
  
let test_for_indirect_deduction gr (box,v)=
  let cells = Box.content box in 
  let compatible_cells = List.filter_map 
    (fun cell->
        let poss = Grid.possibilities_at_cell gr cell in 
        if i_mem v poss
        then Some cell  
        else None) cells in
  if List.length(compatible_cells)=1
  then ( let only_compatible_cell = List.hd compatible_cells in 
       let (_,is_old) = Grid.assoc gr only_compatible_cell in 
       if is_old
       then None  
       else Some(box,v,List.hd compatible_cells) )
  else None ;;         
     
let immediate_simple_deductions gr =   
  let (G (l))=gr in 
   List.filter_map (fun (cell,(_poss,is_old))->
     let poss = Grid.possibilities_at_cell gr cell in 
    if (List.length(poss)=1)&&(not is_old)
    then Some(cell,List.hd poss)
    else  None) l ;;  

let immediate_indirect_deductions gr = 
  let proposals = Cartesian.product Box.all (Int_range.range 1 9) in 
     List.filter_map (
      fun (box,v)->
        Option.map
        (fun (_,_,cell)->
          Ded(Indirect(box,v),(cell,v))
          )
        (test_for_indirect_deduction gr (box,v))) proposals ;;   
let immediate_deductions gr =  
 let temp1 = Image.image (fun (cell,v)->
       Ded(Simple(cell),(cell,v))
    ) (immediate_simple_deductions gr)
   and temp2 =  immediate_indirect_deductions gr in 
   temp1 @ temp2  ;;
  
let possibilities_at_indirect_cell gr (bx,v) =
   let candidates = Box.content bx in 
   List.filter (
     fun cell -> List.mem v (Grid.possibilities_at_cell gr cell)
   ) candidates ;;


end ;;  
  
let immediate_deductions = Private.immediate_deductions ;;
let possibilities_for_value_holder gr = function 
  (Simple(cell)) -> Image.image (fun v->(cell,v))
     (Grid.possibilities_at_cell gr cell) 
  |Indirect(bx,v) -> 
     Image.image (fun cell->(cell,v))
    (Private.possibilities_at_indirect_cell gr (bx,v)) ;; 

end ;;  

module Deduce = struct 

module Private = struct  

 

(*
Rake means collect all immediate deductions
*)

let rake gr = 
  let (G(l)) = gr in 
   let impossibilities = List.filter_map (fun (cell,(vals,_is_old))->
       if vals=[] then Some cell else None
    ) l in 
   if impossibilities<>[]
   then Obstruction(Unassignable_cells(impossibilities))
  else
  let temp3 = Deduction.immediate_deductions gr in 
   let ref_for_adequate_results=ref[]
   and ref_for_overflow_results=ref[] in 
   let _ = List.iter (
     fun cell -> 
      let local_results=List.filter_map (
        fun v->
          let pair=(cell,v) in 
          let fits = List.filter_map(fun
            (Ded(tip,pair2)) -> 
              if pair2=pair then Some tip else None
          ) temp3 in 
          if fits = []
          then None 
          else Some(v,fits)   
      ) (Int_range.range 1 9) in 
      if local_results=[] then () else 
      if List.length(local_results)=1
      then let (v,deds) = List.hd local_results in 
           ref_for_adequate_results:=(cell,v,deds)::(!ref_for_adequate_results) 
      else ref_for_overflow_results:=(cell,local_results)::(!ref_for_overflow_results)     
   ) Cell.all in 
   let overflow_results = (!ref_for_overflow_results) in 
   if overflow_results <> []
   then Obstruction(Mutually_inconsistent_deductions(overflow_results))
   else 
   let adequate_results = !ref_for_adequate_results in 
   let pairs = Uple.list_of_pairs adequate_results in 
   let bad_pairs = List.filter (fun 
     ((cell1,v1,_),(cell2,v2,_)) ->
       (v2=v1) && (Cell.test_for_neighborhood cell1 cell2)
   ) pairs in 
   if bad_pairs <> []
   then Obstruction(Mutually_inconsistent_extensions(bad_pairs))
   else 
   Smooth(!ref_for_adequate_results) ;;



type walker = W of  
grid * 
((cell * int * value_holder list) list) list * 
(obstruction option) *
bool ;;

let push_more_easy_deductions walker =
  let (W(gr,older_deds,obstr_opt,end_reached)) = walker in 
  if end_reached then walker else
  if (obstr_opt <> None) 
  then W(gr,older_deds,obstr_opt,true) 
  else
    match rake gr  with 
   (Smooth new_decorated_deds)-> 
    let new_deds = Image.image (
       fun (cell,v,_expl) ->(cell,v)
     ) new_decorated_deds in 
     W(Grid.assign_several gr new_deds,
        older_deds@[new_decorated_deds],None,
        new_deds=[])
  |Obstruction(obstr) -> 
      W(gr,older_deds,Some obstr,true);;


let rec iterate_easy_deductions walker =
  let (W(gr,older_deds,obstr_opt,end_reached)) = walker in 
  if end_reached then (gr,obstr_opt,List.rev older_deds) else
  iterate_easy_deductions(push_more_easy_deductions(walker)) ;;  

let deduce_easily_as_much_as_possible gr = 
    iterate_easy_deductions(W(gr,[],None,false)) ;;

let fails_after_some_easy_deductions gr = 
   let (_,obstr_opt,_) = 
     deduce_easily_as_much_as_possible gr in 
    (obstr_opt<>None) ;;


let apply_rake gr = 
  match rake gr with 
   Obstruction(_) -> None
  |(Smooth new_decorated_deds)-> 
    let new_deds = Image.image (
       fun (cell,v,_expl) ->(cell,v)
     ) new_decorated_deds in 
     Some(Grid.assign_several gr new_deds) ;;

let rec helper_for_raking_depth (count,to_be_treated) =
   if to_be_treated = []
   then count 
   else helper_for_raking_depth (count+1,
     List.filter_map apply_rake to_be_treated) ;;  

let raking_depth grids = 
    helper_for_raking_depth (0,grids) ;;
     

let fixed_cells grids domain = 
  let finished_grids = List.filter_map apply_rake grids in 
  List.filter_map (fun cell ->
     let temp2 = Image.image (
       fun gr -> i_sort (Grid.possibilities_at_cell gr cell)
     ) finished_grids in 
     let whole = i_fold_merge temp2 in 
     if List.length(whole)=1
     then Some cell 
     else None 
  ) domain ;;

let common_good grid k =
   let domain = Grid.living_cells grid in 
   let uples = Uple.l_naive_combinations k domain in 
   let temp = Explicit.image (
      fun u ->
        let close_descendants = Grid.expand [grid] u in 
        (u,fixed_cells close_descendants domain)
   ) uples in 
   List.filter (fun (_,advances)->advances<>[]) temp ;;



end ;;

let deduce_easily_as_much_as_possible = 
    Private.deduce_easily_as_much_as_possible;;  

let common_good = Private.common_good ;; 

let fails_after_some_easy_deductions =
  Private.fails_after_some_easy_deductions ;;
    
let rake = Private.rake ;;

let raking_depth = Private.raking_depth ;; 


end ;;   

module Drill = struct 

exception Forbidden_extension of cell * int ;;
exception Impossible_extension of cell * int ;;
exception Wrong_deduction of value_holder ;;
exception No_breaking_point_found of value_holder ;;
exception No_break_present of value_holder ;;

module Private = struct 
  
let initialize gr = Dr [ {
    self_idx = 1 ;
    ancestor_idx = None ;
    biography = [] ;
    improvements = [] ;
    birth_state = gr ;
    current_state = gr ;
    forbidden_extensions = []
} ];;  
  
  

let assign (Dr(l)) cell v = 
  let old_elt = List.hd l in 
  if List.mem (cell,v) old_elt.forbidden_extensions 
  then raise(Forbidden_extension(cell,v))
  else  
  let old_grid = old_elt.current_state in 
  let poss = Grid.possibilities_at_cell old_grid cell in 
  if not(List.mem v poss)
  then raise(Impossible_extension(cell,v))  
  else
  let new_grid = Grid.assign old_grid cell v in 
  let new_elt = {
    self_idx = (old_elt.self_idx)+1 ;
    ancestor_idx = Some(old_elt.self_idx) ;
    biography = (cell,v) :: (old_elt.biography) ;
    improvements = [] ;
    birth_state = new_grid ;
    current_state = new_grid ;
    forbidden_extensions = old_elt.forbidden_extensions
  }  in 
  Dr(new_elt::l) ;;

let possibilities_for_value_holder (Dr(l)) vh =
   let current_elt = List.hd l in 
  let current_grid = current_elt.current_state in 
   let candidates = Deduction.possibilities_for_value_holder current_grid vh in 
   let forbidden = current_elt.forbidden_extensions in 
   List.filter (fun p->not(List.mem p forbidden)) candidates ;;

let single_deduce dr vh =
    let poss = possibilities_for_value_holder dr vh in 
    if List.length(poss)<>1
    then raise(Wrong_deduction(vh))  
    else
    let (cell,v) = List.hd poss  
    and (Dr(l)) = dr in 
    let old_elt = List.hd l in 
    let old_grid = old_elt.current_state in 
    let new_grid = Grid.assign old_grid cell v in 
    let new_elt = {
        old_elt with 
        improvements = (Ded(vh,(cell,v))) ::old_elt.improvements;
        current_state = new_grid;
    } in
    Dr(new_elt::(List.tl l)) ;;

let deduce dr vhs = List.fold_left single_deduce dr vhs ;;    

let rec iterator_for_bp_finding vh (treated,to_be_treated)= 
   if possibilities_for_value_holder (Dr(to_be_treated)) vh <>[]
   then (treated,to_be_treated)
   else 
   match to_be_treated with 
   [] -> raise (No_breaking_point_found(vh))
  |head::others -> iterator_for_bp_finding vh (head,others) ;;
      
let find_breaking_point vh dr = 
   if possibilities_for_value_holder dr vh <>[]
   then raise (No_break_present(vh))
   else
   let (Dr l) = dr in 
   iterator_for_bp_finding vh (List.hd l,List.tl l) ;;
   
let step_back dr vh =
   let (head,others) = find_breaking_point vh dr in 
   let culprit = List.hd(head.biography) 
   and innocent = List.hd others in 
   let new_innocent = {
     innocent with 
     forbidden_extensions = culprit :: innocent.forbidden_extensions;
   } in 
   Dr(new_innocent ::(List.tl others)) ;;

let possibilities_at_cell dr cell = 
   Image.image snd
   (possibilities_for_value_holder dr (Simple(cell))) ;;

let horizontal_view dr x0= 
    let (Dr l) = dr in  
    let elt = List.hd l in 
    let grid = elt.current_state in 
    List.filter_map (
      fun y ->
        let c = C(x0,y) in 
        if snd(Grid.assoc grid c)
        then None 
        else Some(c,possibilities_at_cell dr c)  
    ) (Int_range.range 1 9);;

end ;;  

let assign = Private.assign ;;

let deduce = Private.deduce ;;

let head (Dr l) = List.hd l ;;

let horizontal_view = Private.horizontal_view ;;
let initialize_with l = Private.initialize (Grid.initialize_with l) ;;

let possibilities_at_cell = Private.possibilities_at_cell ;;
    
let possibilities_for_value_holder = Private.possibilities_for_value_holder ;;

let size (Dr l) = List.length l ;;
let step_back = Private.step_back ;;

end ;;  


module Display = struct 

module Private = struct 

      let display_for_unassignable_cell = ref 'B';;
      let eval_small_grid_using_matrix_coordinates gr (i,j) = 
         let cell = Cell.from_matrix_coordinates i j in 
         let (poss,is_old) = Grid.assoc gr cell in 
         let m = List.length poss in 
         if m = 0 then String.make 1 (!display_for_unassignable_cell) else 
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

let display_for_unassignable_cell = Private.display_for_unassignable_cell ;;
let grid_to_string = Private.to_string ;; 
let print_out_grid (fmt:Format.formatter) gr=
  Format.fprintf fmt "@[%s@]" (Private.to_surrounded_string gr);;

let print_out_drill (fmt:Format.formatter) (Dr l)=
  let current_grid = (List.hd l).current_state in
  Format.fprintf fmt "@[%s@]" (Private.to_surrounded_string current_grid);;




end ;; 

(* 

open Sudoku ;; 

#install_printer Display.print_out_grid ;;
#install_printer Display.print_out_drill ;;


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