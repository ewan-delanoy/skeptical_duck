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

type value_holder =
    Simple of cell
   |Indirect of box * int ;; 

type deduction = Ded of value_holder * (cell * int) ;;

type raking_result =
    Smooth of  (cell * int * value_holder list) list
   |Obstruction0_found of cell  list
   |Obstruction1_found of (cell * (int * value_holder list) list) list 
   |Obstruction2_found of ( (cell * int * (value_holder list)) * 
                            (cell * int * (value_holder list)) ) list;;

type pool = Pool of (value_holder * ((cell * int) list)) list ;;

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
  G(Watcher_for_forbidden_configurations.empty_one,
  Image.image (fun cell->(cell,(small_base,false))) Cell.all) ;;

let possibilities_at_cell (G (wfc,l)) cell = 
   let temp1 = fst(List.assoc cell l) in 
    i_setminus temp1 
    (Watcher_for_forbidden_configurations.forbidden_values
     wfc cell);;  

let possibilities_at_indirect_cell gr (bx,v) =
   let candidates = Box.content bx in 
   List.filter (
     fun cell -> List.mem v (possibilities_at_cell gr cell)
   ) candidates ;;


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

let assign_several gr assignments = 
     List.fold_left  uncurried_assign gr assignments ;; 

let freedom_left (G(_wfc,l)) =
  List.length(List.filter (fun (_cell,(vals,_))->List.length(vals)>1) l) ;;

let living_cells gr =
  let (G(_wfc,l)) = gr in 
  List.filter_map (fun (cell,(_vals,is_old))->
    if is_old then None else 
    let poss = possibilities_at_cell gr cell in    
    if List.length(poss)>1
    then Some cell  
    else None  ) l ;;

let expand_grid_at_cell cell gr = 
   let (G(_wfc,l)) = gr in 
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

let assoc (G (_wfc,l)) cell = List.assoc cell l ;;  

let cells_with_fewest_possibilities = Private.cells_with_fewest_possibilities ;;

let empty_grid = Private.origin ;;
    
let expand = Private.expand_grids_at_cells ;;

let freedom_left = Private.freedom_left ;;

let horizontal_summary (G(_wfc,l)) = List.filter_map 
  (fun (cell,(poss,is_old)) ->
      if is_old then Some(cell,List.hd poss) else None 
   ) l;;

let initialize_with l =
    let temp1 = List.combine Cell.all l in 
    let temp2 = List.filter (fun (_cell,v)->v<>0) temp1 in 
    assign_several Private.origin temp2 ;; 

let living_cells = Private.living_cells ;;

let possibilities_at_cell = Private.possibilities_at_cell ;; 

let possibilities_for_value_holder gr = function 
  (Simple(cell)) -> Image.image (fun v->(cell,v))
     (Private.possibilities_at_cell gr cell) 
  |Indirect(bx,v) -> 
     Image.image (fun cell->(cell,v))
    (Private.possibilities_at_indirect_cell gr (bx,v)) ;; 

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
  let (G (_wfc,l))=gr in 
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

(*
Rake means collect all immediate deductions
*)

let rake gr = 
  let (G(_wfc,l)) = gr in 
   let impossibilities = List.filter_map (fun (cell,(vals,_is_old))->
       if vals=[] then Some cell else None
    ) l in 
   if impossibilities<>[]
   then Obstruction0_found(impossibilities)
  else
  let temp1 = Image.image (fun (cell,v)->
       Ded(Simple(cell),(cell,v))
    ) (immediate_simple_deductions gr)
   and temp2 =  immediate_indirect_deductions gr in 
   let temp3 = temp1 @ temp2 in 
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
   then Obstruction1_found(overflow_results)
   else 
   let adequate_results = !ref_for_adequate_results in 
   let pairs = Uple.list_of_pairs adequate_results in 
   let bad_pairs = List.filter (fun 
     ((cell1,v1,_),(cell2,v2,_)) ->
       (v2=v1) && (Cell.test_for_neighborhood cell1 cell2)
   ) pairs in 
   if bad_pairs <> []
   then Obstruction2_found(bad_pairs)
   else 
   Smooth(!ref_for_adequate_results) ;;



type walker = W of  
grid * 
((cell * int * value_holder list) list) list * 
((cell * (int * value_holder list) list) list) * 
(((cell * int * (value_holder list)) * 
  (cell * int * (value_holder list)))  list) * 
  (cell list) *
bool ;;

let push_more_easy_deductions walker =
  let (W(gr,older_deds,obstruction1,obstruction2,imps,end_reached)) = walker in 
  if end_reached then walker else
  if (obstruction1 <> []) || (obstruction2 <> []) || (imps <> []) 
  then W(gr,older_deds,obstruction1,obstruction2,imps,true) 
  else
    match rake gr  with 
   (Smooth new_decorated_deds)-> 
    let new_deds = Image.image (
       fun (cell,v,_expl) ->(cell,v)
     ) new_decorated_deds in 
     W(Grid.assign_several gr new_deds,
        older_deds@[new_decorated_deds],[],[],[],
        new_deds=[])
  |Obstruction0_found(unassignable) ->
     W(gr,older_deds,[],[],unassignable,true)
  |Obstruction1_found(obstr1) ->
     W(gr,older_deds,obstr1,[],[],true)
  |Obstruction2_found(obstr2) ->
     W(gr,older_deds,[],obstr2,[],true)   
     ;;


let rec iterate_easy_deductions walker =
  let (W(gr,older_deds,obstruction1,obstruction2,imps,end_reached)) = walker in 
  if end_reached then (gr,obstruction1,obstruction2,imps,List.rev older_deds) else
  iterate_easy_deductions(push_more_easy_deductions(walker)) ;;  

let deduce_easily_as_much_as_possible gr = 
    iterate_easy_deductions(W(gr,[],[],[],[],false)) ;;

let fails_after_some_easy_deductions gr = 
   let (_,obstr1,obstr2,imps,_) = 
     deduce_easily_as_much_as_possible gr in 
    (obstr1<>[])||(obstr2<>[])||(imps<>[]) ;;

let expand_grid_at_cell cell gr =
   let poss = Grid.possibilities_at_cell gr cell in 
   List.filter_map (
     fun v ->
       let temp_gr = Grid.assign_if_unoccupied gr cell v in 
       let (final_gr,obstr1,obstr2,imps,_) =
           deduce_easily_as_much_as_possible temp_gr in 
     if (obstr1<>[])||(obstr2<>[])||(imps<>[])     
     then None
     else Some(final_gr)  
   ) poss ;;

let expand_grids_at_cell l_gr cell =
   List.flatten (Image.image (expand_grid_at_cell cell) l_gr) ;;   

let expand_grids_at_cells l_gr l_cell = 
  List.fold_left expand_grids_at_cell l_gr l_cell  ;;   

let ref_for_expansion_result = 
  ref (([]: grid list)) ;;  
let expand original_grid cells = 
   let final_grids = expand_grids_at_cells [original_grid] cells in 
   let temp1 = Image.image (
     fun gr -> 
       Image.image (fun 
        cell -> (cell,List.hd(fst(Grid.assoc gr cell)))
       ) cells
   ) final_grids in
   let fixed_cells = List.filter_map (
     fun cell ->
       let vals = i_sort(Image.image (List.assoc cell) temp1) in 
       if List.length(vals)=1 
       then Some(cell,List.hd vals)
       else None
   ) cells in 
   let summaries = Image.image (Image.image snd) temp1 in 
   let _ = (ref_for_expansion_result:=final_grids) in 
   (List.length final_grids,fixed_cells,summaries) ;; 

let apply_rake gr = 
  match rake gr with 
   Obstruction0_found(_)
  |Obstruction1_found(_)  
  |Obstruction2_found(_) -> None
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

let expand = Private.expand ;;

let fails_after_some_easy_deductions =
  Private.fails_after_some_easy_deductions ;;
    
let rake = Private.rake ;;

let raking_depth = Private.raking_depth ;; 


end ;;   

module AdvancedDeduce = struct 

module Private = struct 
  
let two_to_twos_in_individual_box gr bx =
   let box_content = Box.content bx in 
   let poss_for_cells = Image.image (
     fun cell -> (cell,Grid.possibilities_at_cell gr cell)
   ) box_content in 
   let poss_at_cell = (fun cell -> List.assoc cell poss_for_cells) in 
   let poss_for_vals = Int_range.scale (
     fun v -> (v,List.filter (fun cell->List.mem v (poss_at_cell cell)) box_content)
   ) 1 9 in 
   let interesting_vals = List.filter (
    fun (_v,poss) -> List.length(poss) = 2
   ) poss_for_vals in 
   let interesting_pairs = Uple.list_of_pairs interesting_vals in 
   List.filter_map (
     fun ((v1,poss1),(v2,poss2)) -> 
        
        if poss1 <> poss2 
        then None
        else 
        let cell1 = List.nth poss1 0
        and cell2 = List.nth poss1 1 in 
        if (poss_at_cell cell1=[v1;v2]) && 
           (poss_at_cell cell2=[v1;v2]) 
        then None 
        else Some((bx,v1,v2,cell1,cell2))
   ) interesting_pairs ;;

let two_to_twos gr =
  List.flatten(
   Explicit.image (two_to_twos_in_individual_box gr) Box.all 
  ) ;;  

let use_two_to_twos gr = 
   let ttts = two_to_twos gr in 
   let explore_ttts_opt = (
      fun cell -> 
        List.find_map (
         fun (_bx,v1,v2,cell1,cell2) -> 
          if (cell=cell1)||(cell=cell2)
          then Some [v1;v2]
          else None
        ) ttts 
   ) in   
   let (G(wfc,l)) = gr in 
   let new_l = Image.image (
      fun (cell,(_,is_old)) -> 
        let vals = 
         (
           match explore_ttts_opt cell with 
           Some better_answer -> better_answer 
           | None -> Grid.possibilities_at_cell gr cell
         ) in 
         (cell,(vals,is_old))
   ) l in 
   G(wfc,new_l) ;;
   


end ;;  

let two_to_twos = Private.two_to_twos ;;

end ;;  

module Pool = struct 

module Private = struct 

let compute gr = 
   Pool( List.filter_map (
     fun val_holder ->
      let possibilities = 
        Grid.possibilities_for_value_holder gr val_holder  in
      if List.length(possibilities)>1   
      then Some (val_holder,possibilities) 
      else None  
   ) Value_holder.all );; 

let support (Pool l) = 
 List.filter_map (fun cell->
  (List.assoc_opt (Simple cell) l)
  ) Cell.all
;;


end ;;  

let compute = Private.compute ;;


end ;;  

module UseWatcher = struct 

exception Unforbiddable_config of (cell * int) list ;;  

module Private = struct
  let check_forbidden_configuration config =
     let gr =  Grid.assign_several Grid.empty_grid config in 
     if Deduce.fails_after_some_easy_deductions gr 
     then ()
     else raise (Unforbiddable_config(config)) ;; 

  let check_forbidden_configurations configs = 
     List.iter check_forbidden_configuration configs ;;

  let initialize_with configs initial_data = 
    let _ = check_forbidden_configurations configs in 
    let wfc = Wfc (
      Image.image (fun config->(Fc config,Fc config)) configs
    ) 
    and (G(_,l_empty_grid)) = Grid.empty_grid in 
    let initial_grid = G(wfc,l_empty_grid) in 
    Grid.assign_several initial_grid initial_data ;;


end ;;   

let initialize_with = Private.initialize_with ;;

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