(*

#use"Van_der_Waerden/vdw_common.ml";;

*)

exception Unknown_width_in_measure_exn of int ;;
exception Unknown_width_in_lower_measure_exn of int ;;

module Private = struct 

  let oord = Total_ordering.silex_compare Total_ordering.for_integers ;;   
  let oint = Total_ordering.for_integers ;;     
  
  let extract_core_and_simplify ll = 
      if ll = [] then ([],[]) else 
      let core = Ordered.fold_intersect oint ll in 
      (core,Image.image (fun l->Ordered.setminus oint l core) ll) ;;          
  
  let diameter soi =
        if Set_of_integers.length(soi)<2 then 0 else 
        (Set_of_integers.max soi) - (Set_of_integers.min soi) + 1  ;;  
      
  let look_for_arithmetic_progressions_in_with_width_equal_to
         soi width=
        if Set_of_integers.length(soi)<3 then [] else 
        let temp1 = Set_of_integers.image (fun x->Set_of_integers.safe_set [x;x+width;x+2*width]) soi in 
        List.filter (fun obstruction ->Set_of_integers.is_included_in obstruction soi) temp1 ;;  

  let look_for_arithmetic_progressions_in_with_width_up_to width soi=
      let max_width = (if width<1 then ((diameter soi)-1)/2 else width) in 
      List.rev(List.flatten(Ennig.doyle 
       (look_for_arithmetic_progressions_in_with_width_equal_to soi) 1 max_width));;
  
  

  let test_for_admissibility constraints soi = match constraints with 
      Vdw_list_of_constraints_t.Defined_by_max_width(max_width) ->
        ((look_for_arithmetic_progressions_in_with_width_up_to max_width soi) = [])
     |General_case obstructions ->
      List.for_all (fun obs->not(Set_of_integers.is_included_in obs soi )) obstructions ;;
          
  let test_joinability criterion l1 l2 =
    test_for_admissibility criterion 
    (Set_of_integers.safe_set (l1@l2)) ;;

  let level_two_translate translation ll=
    Image.image (Ordered.merge oint translation) ll ;;
  
  let max_easy_length = 15 ;;

  exception Set_too_large_to_be_solved_naively ;; 
  
  let naive_power_set soi =
      if (Set_of_integers.length soi) > max_easy_length 
      then raise Set_too_large_to_be_solved_naively
      else Listennou.power_set (Set_of_integers.forget_order soi) ;;  
    
  let naive_restricted_power_set constraints soi =
        let temp1 = naive_power_set soi in 
        List.filter (fun l-> test_for_admissibility constraints (Set_of_integers.safe_set l)) temp1 ;;
  
  let obstructions_passing_through_point_above width x =
     Ennig.doyle (fun t->[x-2*t;x-t]) 1 width ;;
  
  let obstructions_passing_through_one_of_points_above 
     (width,bound) soi =
    let l = Set_of_integers.forget_order soi in  
    let temp1 = List.flatten 
      (Image.image 
      (obstructions_passing_through_point_above width) l)  in 
    List.filter (List.for_all(fun x->x<bound)) temp1  ;;

  let obstructions_passing_through_two_points_above
    (width,bound) soi = 
    let l = Set_of_integers.forget_order soi in  
    let temp1 = Option.filter_and_unpack (
      fun (x,y)->
        let z = 2*x -y in 
        if (y-x<=width)&&(z<bound)
        then Some z
      else None  
    ) (Uple.list_of_pairs l) in 
    Ordered.sort oint temp1 ;;

  let minimal_obstructions_corresponding_to_above 
    (width,bound) soi =
    let part1 = obstructions_passing_through_one_of_points_above (width,bound) soi
    and pre_part2 = obstructions_passing_through_two_points_above (width,bound) soi  in 
    let part2 = Image.image (fun x->[x]) pre_part2 in 
    let temp1 = Ordered.select_minimal_elements_for_inclusion oint (part1 @ part2) in 
    Ordered.sort Total_ordering.cardinality_then_diameter temp1 ;;
    

  let measure_in_width_four n =
      if n<1 then 0 else 
      let q=(n/9) in 
      match n mod 9 with
        0 -> 4*q+1 
      |1 -> 4*q+1
      |2 -> 4*q+2  
      |3 -> 4*q+2
      |4 -> 4*q+3
      |5 -> 4*q+4
      |6 -> 4*q+4  
      |7 -> 4*q+4
      |8 -> 4*q+4 
      | _ -> failwith("unforeseen");;     

  let lower_measure_in_width_four n =
        if n<1 then 0 else 
        let q=(n/9) in 
        match n mod 9 with
          0 -> 4*q
        |1 -> 4*q
        |2 -> 4*q 
        |3 -> 4*q
        |4 -> 4*q+1
        |5 -> 4*q+1
        |6 -> 4*q+2  
        |7 -> 4*q+2
        |8 -> 4*q+3 
        | _ -> failwith("unforeseen");;  

let measure (Vdw_max_width_t.MW mw) n=
  match mw with 
  4 -> measure_in_width_four n 
  | _ -> raise( Unknown_width_in_measure_exn mw);;

let lower_measure (Vdw_max_width_t.MW mw) n=
  match mw with 
  4 -> lower_measure_in_width_four n 
  | _ -> raise( Unknown_width_in_lower_measure_exn mw);;  

let optify (n,d) = if d<0 then None else Some(n,d) ;;

end ;;  

module Unused = struct

let test_for_admissibility = Private.test_for_admissibility;;

let naive_solver constraints soi =
    let temp1 = Private.naive_restricted_power_set constraints soi in 
    let (optimal_size,temp2) = Max.maximize_it_with_care  List.length temp1 in 
    (optimal_size,Ordered.sort (Total_ordering.lex_compare Total_ordering.standard) temp2) ;;

(* naive_solver (Vdw_list_of_constraints_t.Defined_by_max_width(0)) 
         (Set_of_integers.safe_set(Ennig.ennig 1 9)) ;; *)    

let get_obstructions constraints soi= match constraints with
    Vdw_list_of_constraints_t.Defined_by_max_width(max_width) ->
      (Private.look_for_arithmetic_progressions_in_with_width_up_to max_width soi) 
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
    if (Set_of_integers.length soi) <= Private.max_easy_length 
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
  :> Set_of_integers_t.t Total_ordering_t.t );;    
let is_silex_lower_than x y= (silex_order x y)=Total_ordering_result_t.Lower  ;;

let naive_half_power_set soi =
  let temp1 = Private.naive_power_set soi in 
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
         
let optimistic_silex_smallest_solution width soi =
   let obstructions = Private.look_for_arithmetic_progressions_in_with_width_up_to width soi in 
   let formal = Vdw_list_of_constraints_t.General_case obstructions in 
   let (opt_size,_) = optimistic_solver formal soi in 
   iterator_for_smallest_solution (Set_of_integers.safe_set [],(soi,obstructions,opt_size)) ;; 

let translate d soi = Set_of_integers.safe_set (Set_of_integers.image (fun x->x+d) soi);;

let test_for_disjointness ll=
      let temp1 = Uple.list_of_pairs ll in 
      List.for_all (fun (x,y)->(Set_of_integers.size_of_intersection x y) = 0) temp1 ;; 
   
let solution_in_disjoint_case obstructions soi = 
      let temp1 = Image.image Set_of_integers.max obstructions in
      Set_of_integers.setminus soi (Set_of_integers.safe_set temp1) ;;
   
let set_start_to_one soi =
        let d = (Set_of_integers.min soi)-1 in 
        (d,translate (-d) soi);;
   
let level1 soi = 
        let l = Set_of_integers.forget_order soi in 
        let intervals = Listennou.decompose_into_connected_components l in 
        let temp1 = List.flatten(Image.image (
          fun (a,b)-> List.filter (fun x->List.mem ((x-a) mod 3)[0;1] ) (Ennig.ennig a b)
        ) intervals) in 
        (List.length temp1,Set_of_integers.safe_set temp1) ;;
       
let partial_level3 n = 
      match n with 
         9 -> [1; 2; 4; 8; 9] 
       | 11 -> [1; 2; 4; 5; 10; 11]
      | _ ->List.filter (fun x->List.mem(x mod 8)[1;2;4;5]) (Ennig.ennig 1 n);;     
   
let check_for_precomputed_value hashtbl (width,soi) =
      let (d,relocated_soi) = set_start_to_one soi in 
      match Hashtbl.find_opt hashtbl (width,relocated_soi) with 
      (Some(optimal_size,sol)) -> Some(optimal_size,translate d sol)
      |None ->
         let obstructions = Private.look_for_arithmetic_progressions_in_with_width_up_to width soi in 
         if test_for_disjointness obstructions
         then let sol = solution_in_disjoint_case obstructions soi in 
               Some(Set_of_integers.length sol,sol)
         else      
         if width=1 
         then Some(level1 soi) 
         else None;;
 

    






end ;;



let extended_partition selector  ll= 
    let (temp1,temp2) = List.partition 
    (Ordered.is_included_in Private.oint selector ) ll in 
     (Private.extract_core_and_simplify temp1,
      Private.extract_core_and_simplify temp2) ;;

exception Homogeneous_translation_exn of (int list) * ( (int list) * (int list) );;

let homogeneous_translation criterion ll translation =
   match ll with 
   [] -> Vdw_homogeneous_translation_result_t.Nothing_taken
   | head :: others ->
     let tester = Private.test_joinability criterion translation in 
     let is_joinable = tester head in 
     match Option.seek (fun l1->(tester l1)<>is_joinable) others with 
      (Some l1) -> raise (Homogeneous_translation_exn(translation,(head,l1)))
     | None -> 
       if is_joinable
       then Vdw_homogeneous_translation_result_t.All_taken(Image.image (fun l->
        Ordered.safe_set Private.oint (l@translation)) ll)
       else Vdw_homogeneous_translation_result_t.Nothing_taken;;

 
let reconstruct parts =
        let temp1 = Image.image (fun (a,b)->
          Private.level_two_translate a b) parts in 
        Ordered.fold_merge Private.oord temp1 ;;

let lower_measure = Private.lower_measure ;; 
let measure = Private.measure ;;        
let minimal_obstructions_corresponding_to_above = Private.minimal_obstructions_corresponding_to_above ;;

let decompose max_width n d=
    let delta = (measure max_width n) -(measure max_width  (n-1)) in 
    let draft = [ (n-1,d-delta+1),[n] ; (n-1,d-delta),[]] in 
    List.filter ( fun ((n1,d1),l) -> d1>0 ) draft ;;

let generic_computer (Vdw_max_width_t.MW max_width) n =   
      let unordered_base = 
        Private.naive_restricted_power_set
       ( Vdw_list_of_constraints_t.Defined_by_max_width max_width) 
         (Set_of_integers.safe_set(Ennig.ennig 1 n))
      in 
      Ordered.sort Private.oord unordered_base ;;

