(*

#use"Van_der_waerden/vdw_common.ml";;

*)

exception Set_too_large_to_be_solved_naively ;; 

let diameter soi =
  if Set_of_integers.length(soi)<2 then 0 else 
  (Set_of_integers.max soi) - (Set_of_integers.min soi) + 1  ;;  

let look_for_arithmetic_progressions_in_with_width_equal_to
   soi width=
  if Set_of_integers.length(soi)<3 then [] else 
  let temp1 = Set_of_integers.image (fun x->Set_of_integers.safe_set [x;x+width;x+2*width]) soi in 
  List.filter (fun obstruction ->Set_of_integers.is_included_in obstruction soi) temp1 ;;  

let look_for_arithmetic_progressions_in_with_width_up_to
  width soi=
    let max_width = (if width<1 then ((diameter soi)-1)/2 else width) in 
    List.rev(List.flatten(Ennig.doyle (look_for_arithmetic_progressions_in_with_width_equal_to soi) 1 max_width));;

let test_for_admissibility constraints soi = match constraints with 
  Vdw_list_of_constraints_t.Defined_by_max_width(max_width) ->
    ((look_for_arithmetic_progressions_in_with_width_up_to max_width soi) = [])
 |General_case obstructions ->
  List.for_all (fun obs->not(Set_of_integers.is_included_in obs soi )) obstructions ;;
  

let max_easy_length = 15 ;;

let naive_power_set soi =
  if (Set_of_integers.length soi) > max_easy_length 
  then raise Set_too_large_to_be_solved_naively
  else Listennou.power_set (Set_of_integers.forget_order soi) ;;  

let naive_restricted_power_set constraints soi =
    let temp1 = naive_power_set soi in 
    List.filter (fun l-> test_for_admissibility constraints (Set_of_integers.safe_set l)) temp1 ;;
   

let naive_solver constraints soi =
    let temp1 = naive_restricted_power_set constraints soi in 
    let (optimal_size,temp2) = Max.maximize_it_with_care  List.length temp1 in 
    (optimal_size,Ordered.sort (Total_ordering.lex_compare Total_ordering.standard) temp2) ;;

(* naive_solver (Vdw_list_of_constraints_t.Defined_by_max_width(0)) 
         (Set_of_integers.safe_set(Ennig.ennig 1 9)) ;; *)    

let get_obstructions constraints soi= match constraints with
    Vdw_list_of_constraints_t.Defined_by_max_width(max_width) ->
      (look_for_arithmetic_progressions_in_with_width_up_to max_width soi) 
   |General_case obstructions -> List.filter 
      (fun obs->Set_of_integers.is_included_in obs soi) 
    obstructions ;;

let optimize_constraint constraints soi= match constraints with
    Vdw_list_of_constraints_t.Defined_by_max_width(max_width) -> constraints
   |General_case obstructions -> let effective_obstructions =List.filter 
      (fun obs->Set_of_integers.is_included_in obs soi) obstructions in 
      Vdw_list_of_constraints_t.General_case effective_obstructions ;;    

let key_vertex constraints soi =
   let obstructions = get_obstructions constraints soi in 
   let temp2 =  Set_of_integers.image (fun x->(x,List.length(List.filter (Set_of_integers.mem x) obstructions))) soi in 
   let (_,temp3) = Max.maximize_it_with_care snd temp2 in
   fst(List.hd(List.rev temp3));;
   
let rec optimistic_solver constraints soi =
    if test_for_admissibility constraints soi 
    then (Set_of_integers.length soi,soi) 
    else
    if (Set_of_integers.length soi) <= max_easy_length 
    then let first_sol = List.hd (snd(naive_solver constraints soi)) in 
         (List.length first_sol,Set_of_integers.safe_set first_sol) 
    else   
    let k = key_vertex constraints soi in 
    let new_soi = Set_of_integers.outsert k soi in  
    let new_constraints = optimize_constraint constraints new_soi in 
    optimistic_solver new_constraints new_soi ;;

let first_cut constraints soi = 
    let (n1,sol1) = optimistic_solver constraints soi in 
    let rec tempf = (fun  (head_constraint,sub_constraints) ->
      let (n2,sol2) = optimistic_solver sub_constraints soi in 
      if n2 <> n1
      then (n1,sol1,head_constraint)
      else let obs = get_obstructions sub_constraints soi in 
           tempf(List.hd obs,Vdw_list_of_constraints_t.General_case(List.tl obs)) 
    ) in 
    let obs2 = get_obstructions constraints soi in 
    tempf(List.hd obs2,Vdw_list_of_constraints_t.General_case(List.tl obs2)) 

let silex_order = ((fun x y->Total_ordering.silex_compare Total_ordering.for_integers 
   (Set_of_integers.forget_order x)  (Set_of_integers.forget_order y))
  :> Set_of_integers_t.t Total_ordering.t );;    
let is_silex_lower_than x y= (silex_order x y)=Total_ordering.Lower  ;;

let naive_half_power_set soi =
  let temp1 = naive_power_set soi in 
  List.filter (fun x->
    let sx = Set_of_integers.safe_set x in 
    let sy = Set_of_integers.setminus soi sx in 
    (List.length x>0)&& (is_silex_lower_than sx sy)
  ) temp1 ;;

(*

naive_half_power_set (Set_of_integers.safe_set (Ennig.ennig 1 3));;

*)

let naively_compute_minimal_orthogonal_parts hg_vertices hg_edges =
    let edge1 = List.hd hg_edges in 
    let temp1 = Image.image Set_of_integers.safe_set (naive_half_power_set hg_vertices) 
    and is_orthogonal = (
      fun part ->
          let n1 = Set_of_integers.size_of_intersection part edge1 in 
          List.for_all (fun edge ->(Set_of_integers.size_of_intersection part edge)=n1) hg_edges
    ) in
    let temp2 = List.filter is_orthogonal temp1 in
    let is_minimal = (fun part->List.for_all 
      (fun part2->(part2=part)||(not(Set_of_integers.is_included_in part2 part)) ) temp2) in 
    let temp3 =List.filter is_minimal temp2 in 
    Ordered.sort silex_order temp3;; 

(*

let z1 = Set_of_integers.safe_set(Ennig.ennig 1 9) ;;
let z2 = naive_solver (Vdw_list_of_constraints_t.Defined_by_max_width(0)) z1 ;;
let z3 = Image.image Set_of_integers.safe_set z2;;
let z4 = naively_compute_minimal_orthogonal_parts z1 z3;;

*)    

let naively_compute_decomposers constraints soi =
    let (n1,temp1) = naive_solver constraints soi in 
    let solutions = Image.image Set_of_integers.safe_set temp1 in 
    let temp2 = naively_compute_minimal_orthogonal_parts soi solutions   
    and is_a_decomposer = (
       fun  sx->
        let sy = Set_of_integers.setminus soi sx in 
        let nx=fst(naive_solver constraints sx)
        and ny=fst(naive_solver constraints sy) in 
        nx+ny = n1
    )  in 
    List.filter is_a_decomposer temp2 ;;

(*

let z1 = Set_of_integers.safe_set(Ennig.ennig 1 11) ;;
let z2 = naively_compute_decomposers (Vdw_list_of_constraints_t.Defined_by_max_width(0)) z1 ;;

*)        

let test_for_handle_passage width soi=
   let w1 = Vdw_list_of_constraints_t.Defined_by_max_width width 
   and w2 = Vdw_list_of_constraints_t.Defined_by_max_width (width-1) in 
   let (n1,_) = optimistic_solver w1 soi 
   and (n2,_) = optimistic_solver w2 soi in 
   n1 = n2 ;;  

let write_fork head_constraint (past,soi) =
    Set_of_integers.image (
      fun cell-> 
        let remains = Set_of_integers.outsert cell head_constraint in 
        ((cell,Set_of_integers.forget_order remains)::past,Set_of_integers.outsert cell soi)
        
    ) head_constraint  ;;


let helper_for_computing_cartesian_handle width cases=
   let rec tempf = (fun (treated,to_be_treated) ->
   match to_be_treated with 
   [] -> treated
   | (past,soi) :: others ->
       if test_for_handle_passage width soi 
       then  tempf((past,soi)::treated,others)
       else  
       let w = Vdw_list_of_constraints_t.Defined_by_max_width width in 
       let (_,_,head_constraint) = first_cut w soi in
       let new_cases = write_fork head_constraint (past,soi) in
       tempf((past,soi)::treated,List.rev_append new_cases others)
   ) in 
   tempf([],cases);;         

let compute_cartesian_handle width case = 
  helper_for_computing_cartesian_handle width [[],case] ;;   

let force_remove removed_elts (soi,obstructions) =
    let new_soi = Set_of_integers.setminus soi removed_elts in 
    let new_obstructions = List.filter (
      fun obs -> (Set_of_integers.size_of_intersection obs removed_elts) = 0
    ) obstructions in 
    (new_soi,new_obstructions) ;;
   
let add_possibly_singleton_obstructions new_obstructions (soi,obstructions) =
    let (singles,nonsingles) = List.partition 
      (fun obs -> Set_of_integers.length obs =1) 
     new_obstructions in 
     let removed_elts = Set_of_integers.safe_set (Image.image Set_of_integers.min singles) in 
     force_remove removed_elts (soi,nonsingles@obstructions) ;;
        
let force_insert inserted_elt (soi,obstructions) =
   let (touched,untouched) = List.partition (Set_of_integers.mem inserted_elt) obstructions in 
   let new_obstructions = Image.image (Set_of_integers.outsert inserted_elt) touched in 
   add_possibly_singleton_obstructions 
      new_obstructions (Set_of_integers.outsert inserted_elt soi,untouched) ;;


(*      
let rec iterator_for_smallest_solution (treated,(soi,obstructions,opt_size)) =
    if obstructions = [] 
    then let sol = Set_of_integers.merge treated soi in 
         (Set_of_integers.length sol,sol)
    else let a = Set_of_integers.min soi in
         let (new_soi,new_obstructions) = force_insert a (soi,obstructions) in 
         let (n1,_) = optimistic_solver 
              (Vdw_list_of_constraints_t.General_case(new_obstructions)) new_soi in 
         if n1 = opt_size -1
         then  iterator_for_smallest_solution 
             (Set_of_integers.insert a treated,(new_soi,new_obstructions,n1))     
         else     
         let (soi2,obstructions2) = force_remove
            (Set_of_integers.singleton a)  (soi,obstructions) in  
          iterator_for_smallest_solution (treated,(soi2,obstructions2,opt_size))  ;;
*)          