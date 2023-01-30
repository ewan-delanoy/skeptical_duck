(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz_types_for_third_stab ;; 
open Sz_preliminaries_for_stab ;;


let cil_order = ((fun (C x) (C y)->il_order x y) : constraint_t Total_ordering_t.t) ;;

let test_for_admissibility_up_to_max_with max_width z =
  if max_width<1 then true else 
  Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;

let test_for_admissiblity width breadth z =
   (test_for_admissibility_up_to_max_with (width-1) z)
   &&
   (List.for_all (fun t->
    not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;

let remove_one_element (S n,scrappers) k=
  let new_scrappers = i_insert k scrappers in 
  if k <> n then (S n,new_scrappers) else 
  let new_z =  Finite_int_set.of_pair (S n,new_scrappers) in 
  let new_max = List.hd(List.rev new_z) in 
  (S new_max,List.filter (fun t->t<new_max) new_scrappers) ;;


(*

remove_one_element (10,[3;7;8;9]) 10 ;;

*)





let low_hashtbl = Hashtbl.create 50 ;;
  

let access_opt  pt = 
 let (pt2,adj) = Simplest_reduction.decompose pt in 
 if pt2 = Empty_point 
 then Some (Bulk_result.atomic_case pt)
 else
 let pre_res=Rose.try_precomputed_results pt2 in 
 Bulk_result.extend_with_opt pt2 pre_res adj ;;

let this_path = 
  home^
     "/Teuliou/OCaml/skeptical_duck/watched/watched_and_githubbed/"^
     "Szemeredi_problem/" ;; 

let ap_for_head =
    Absolute_path.of_string(this_path^
     "current_head_position_for_szemeredi_problem.ml") ;; 

let ap_for_this_file =
      Absolute_path.of_string(this_path^
       "current_stab_at_szemeredi_problem.ml") ;; 

let update_head () = 
    let new_text = Io.read_whole_file ap_for_this_file in 
    Replace_inside.overwrite_between_markers_inside_file 
     ~overwriter:new_text 
        (
          "(*"^" Reproduced stab starts here *)",
          "(*"^" Reproduced stab ends here *)"
        ) ap_for_head ;; 
   
let superificial_result_in_jump_case  pt_after_jump =
  let (width,scrappers,B breadth, S n) = Point.unveil pt_after_jump in 
  let pt_before_jump = P(width-1,scrappers,B(n-2*(width-1)),S n) in  
  let (pt2,adj2) = Simplest_reduction.decompose pt_before_jump in 
  ([],Some(Decomposable(pt2,adj2))) ;; 
    
let access_with_helper_opt pt helper =
    match List.assoc_opt pt helper with 
    Some answer -> Some answer
    | None ->  access_opt pt ;;

let compute_superficial_result_partially pt helper =  
  if pt = Empty_point then ([],Some Atomic) else
  let (width,scrappers,B breadth,n) = Point.unveil pt in 
  let (pt2,adj2) = Simplest_reduction.decompose pt in 
  if ((width,breadth)=(1,0))||(pt2=Empty_point)
  then ([],Some Atomic)
  else 
  if adj2<>[]
  then ([],Some(Decomposable(pt2,adj2)))
  else     
  if breadth = 0
  then superificial_result_in_jump_case pt   
  else
  let (width2,scrappers2,B breadth2,n2) = Point.unveil pt2 in   
  let _ = assert(breadth2>0) in 
  let front_constraint = C [breadth2;breadth2+width2;breadth2+2*width2] 
  and preceding_point = P(width2,scrappers2,B(breadth2-1),n2) in 
  match access_with_helper_opt  preceding_point helper with 
    None -> ([preceding_point],None)
   |Some bres ->
       (match Bulk_result.impose_one_more_constraint_opt preceding_point front_constraint bres  with 
       None -> let tooths = Int_range.scale (fun k->
                let (m,scr) = remove_one_element  (n2,scrappers2)  (breadth2+k*width2) in 
                let pt3 = P(width2,scr,B(breadth2-1),m) in 
                Simplest_reduction.decompose(pt3) 
               ) 0 2  in 
              ([],Some(Fork tooths))
      |Some bres2 -> ([],Some(Contraction(preceding_point,front_constraint)))) ;; 

let fork_case_in_bulk_result_computation old_f cases helper = 
      let (last_pt,last_adj) = List.nth cases 2 in 
      let partial_res5 = old_f last_pt helper in 
      match snd partial_res5 with 
      None -> (fst partial_res5,None) 
     |Some br5 -> 
       let (BR(_,M(reps,_))) = Bulk_result.extend_with last_pt br5 last_adj in 
       let new_mold = M(reps,Image.image (
        fun (pt6,adj6)-> Q(pt6,[],adj6)
      ) cases) in 
      ([],Some (BR(Fork cases,new_mold))) ;; 

exception Bad_contraction of point * constraint_t ;; 

let rec compute_bulk_result_partially pt helper=  
  if pt = Empty_point then ([],Some(Bulk_result.atomic_case pt)) else 
   let partial_res1 = compute_superficial_result_partially pt helper in 
   match snd partial_res1 with 
    None -> (fst partial_res1,None) 
   |Some sr ->(match sr with 
     Atomic -> ([],Some(Bulk_result.atomic_case pt)) 
   | Decomposable(pt2,adj2) -> 
       let partial_res2 = compute_bulk_result_partially pt2 helper in 
       (
        match snd partial_res2 with 
        None -> (fst partial_res2,None) 
       |Some br2 -> ([],Some (Bulk_result.extend_with pt2 br2 adj2))
       )
   | Contraction (pt5,cstr) ->
    let partial_res4 = compute_bulk_result_partially pt5 helper in 
    (
     match snd partial_res4 with 
     None -> (fst partial_res4,None) 
    |Some br4 -> 
      match Bulk_result.impose_one_more_constraint_opt pt5 cstr br4 with 
        None -> raise(Bad_contraction(pt5,cstr))
        |Some new_br4 ->([],Some new_br4)
    ) 
   | Fork cases ->
    fork_case_in_bulk_result_computation compute_bulk_result_partially cases helper
   ) ;; 

   
let add_if_necessary (a,b) assoc_list = 
  if List.mem_assoc a assoc_list 
  then assoc_list 
  else (a,b) :: assoc_list ;;   

exception Pusher_stop ;;

let pusher_for_needed_subcomputations  
   (treated,to_be_treated) = match to_be_treated with 
         [] -> raise Pusher_stop
         | pt1 :: other_pts ->
           let partial_res1 = compute_bulk_result_partially pt1 treated in 
           match snd partial_res1 with 
            None -> (treated,(fst partial_res1)@to_be_treated)
           |Some answer -> (add_if_necessary (pt1,answer) treated,other_pts) ;;

let rec needed_subcomputations walker =
    let (treated,to_be_treated) = walker in 
    let subcomps =( match to_be_treated with 
    [] -> treated
    | _ -> needed_subcomputations (pusher_for_needed_subcomputations walker) ) in 
    let new_subcomps = List.filter (
    fun (_,bres) -> Bulk_result.is_not_atomic bres
   ) subcomps in 
   let _ = List.iter (fun (pt2,bres)->
    Hashtbl.replace low_hashtbl pt2 bres ) new_subcomps in 
    subcomps ;;

let compute_bulk_result pt =
   let subcomps = needed_subcomputations ([],[pt]) in 
   List.assoc pt subcomps ;;   

