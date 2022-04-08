(*

#use "Githubbed_archive/Szemeredi_problem/third_stab_at_szemeredi_problem.ml" ;;

A more in-depth analysis of the width=2 case.

The contents of the "Enclosed" module is copy-pasted from the first stab.
Interface : 



*)

open Needed_values ;;



let i_order = Total_ordering.for_integers ;;
let i_does_not_intersect = Ordered.does_not_intersect i_order ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_setminus = Ordered.setminus i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_mem = Ordered.mem il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.safe_set il_order ;;



module Enclosed = struct

let this_file = Absolute_path.of_string "Githubbed_archive/Szemeredi_problem/first_stab_at_szemeredi_problem.ml" ;;

let basic_string_of_il l = "["^(String.concat ";" (Image.image string_of_int l))^"]" ;;
let string_of_interval (a,b) =
    if b-a<=2 then basic_string_of_il(Int_range.ennig a b) else 
    "(Ennig.ennig "^(string_of_int a)^" "^(string_of_int b)^")" ;;
let string_of_il l =
     if List.length(l)<=3 then basic_string_of_il l else 
     let temp1 = Arithmetic_list.decompose_into_connected_components l in 
     let draft =String.concat "@" (Image.image string_of_interval temp1) in 
    if List.length(temp1)<2 then draft else "("^draft^")";;       
let string_of_ill l = "["^(String.concat ";" (Image.image string_of_il l))^"]" ;;

type level_t = L of int ;;

module Case = struct

type t = 
   Compatible 
 | Decomposable of int list list
 | Ramifiable of int list ;;

let level = function 
  Compatible -> 1
| Decomposable _ -> 2 
| Ramifiable _ -> 3 ;;

let order = ((fun case1 case2 ->
   let trial1 = Total_ordering.for_integers (level case1) (level case2) in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else  
   Total_ordering.standard case1 case2  
) : t Total_ordering_t.t);;



let order_for_analysis_pairs = ((fun (il1,cs1) (il2,cs2) ->
  let trial1 =order cs1 cs2 in
  if trial1<>Total_ordering_result_t.Equal then trial1 else 
  il_order il1 il2
  ) : ( int list * t) Total_ordering_t.t);;
    
let order_for_detailed_pairs = 
    Total_ordering.product il_order order_for_analysis_pairs ;;

end ;;

module Detailed_solution= struct

type t = {
   level : level_t ;
   argument : int list ;
   case : Case.t ;
   solution : int list ;
   is_compatible : bool ;
} ;;

let one_command dsol  = 
  let (L i_level) = dsol.level in 
  let s_x=   string_of_il dsol.argument 
  and lvl = string_of_int i_level in 
  match dsol.case with
  Compatible -> 
                let opt_part = ( 
                  if dsol.is_compatible
                  then "None"
                  else "(Some("^(string_of_il dsol.solution)^"))"  
                ) in 
                "g_add_compatible "^lvl^" "^s_x^" "^opt_part^" ;;"
 |Decomposable(ll) -> "g_add_decomposable "^lvl^" "^s_x^" "^(string_of_ill ll)^" "^(string_of_il dsol.solution)^" ;;"
 |Ramifiable(obs) -> "g_add_ramification "^lvl^" "^s_x^" "^(string_of_il obs)^" ;;" ;;

 exception Order_exn of t * t ;;

 let order dsol1 dsol2 = 
  let (L level1) = dsol1.level and  (L level2) = dsol2.level in 
  let trial1 =i_order level1 level2 in
  if trial1 <>Total_ordering_result_t.Equal then trial1 else
   let case1 = dsol1.case and case2 = dsol2.case in 
   if (case1 = Case.Compatible) && (case2 <> Case.Compatible) then Total_ordering_result_t.Lower else
   if (case2 = Case.Compatible) && (case1 <> Case.Compatible) then Total_ordering_result_t.Greater else 
   let usual =il_order dsol1.argument dsol2.argument in
   if usual <>Total_ordering_result_t.Equal then usual else
   raise(Order_exn(dsol1,dsol2)) ;; 

let is_suitable_for_display dsol =
    match Int_range.test_for_interval dsol.argument with
    None -> true 
    | Some(a0,b0) ->
      let (L i_level) = dsol.level 
      and length_of_interval = b0-a0+1 in 
      if i_level < 3 
      then false
      else 
      if i_level = 3 
      then List.mem length_of_interval [7;8] 
      else (i_level = 4) && (length_of_interval>8);; 

let one_qualified_command stars_should_be_present (dsols,is_new) = 
 let prefix = (
  if is_new && stars_should_be_present then "(* new *) " else ""
 ) in 
 let lines = Image.image (fun dsol->prefix^(one_command dsol)) dsols in 
 String.concat "\n" lines;;
   
let commands_for_one_heavy_block stars_should_be_present (lvl,(unchecked_l1,l2)) = 
  let l1 = List.filter is_suitable_for_display unchecked_l1 in 
  let temp1 = Ordered_misc.underline_new_elements order l1 l2 in 
  (String.concat "\n" (Image.image (one_qualified_command stars_should_be_present) temp1))^"\n\n\n"  ;;

let message_for_heavy_commands stars_should_be_present ll=
   "\n\n\n"^(String.concat "\n\n" 
  (List.rev_map (commands_for_one_heavy_block stars_should_be_present)  ll))^"\n\n\n"  ;;

let show_heavy_commands  ll=
  let msg = message_for_heavy_commands true ll in 
  print_string msg ;;

let commands_for_one_lightweight_block (lvl,(l1,l2)) = 
    let temp1 = Ordered.setminus order l2 l1 in 
    (String.concat "\n" (Image.image one_command  temp1))^"\n\n\n"  ;;
  


let markers_for_level k =
   let sk = string_of_int k in 
   ("(* Be"^"ginnning of precomputed data for level "^sk^" *)",
    "(* En"^"d of precomputed data for level "^sk^" *)") ;;

let show_lightweight_commands_without_reacting ll=
  let msg = "\n\n\n"^(String.concat "\n\n" 
    (List.rev_map commands_for_one_lightweight_block  ll))^"\n\n\n" in 
    print_string msg  ;;

let show_lightweight_commands ll=
    let _ = Image.image (fun (lvl,pair)->
      let txt = "\n\n\n"^(commands_for_one_heavy_block true (lvl,pair)) 
      and markers = markers_for_level lvl in 
      Replace_inside.overwrite_between_markers_inside_file  
        (Overwriter.of_string txt) markers this_file
      ) ll in 
      show_lightweight_commands_without_reacting ll ;;  
    ;;


let is_strongly_compatible dsol =  
  (dsol.case = Case.Compatible) && dsol.is_compatible ;;


  

end ;;  

let small_n = 100 ;;

exception Nondisjoint_parts of (int list) * (int list) ;;
exception Bad_merger of (int list) * (int list) ;;

let check_for_partition x parts =
  let temp1 = Uple.list_of_pairs parts in 
  match Option.seek (fun (part1,part2)->i_intersects part1 part2) temp1 with
  Some(part3,part4) -> raise(Nondisjoint_parts(part3,part4))
  |None ->
  let merger = i_fold_merge parts in 
  if merger <> x
  then raise(Bad_merger (merger,x)) 
  else ();;  


exception Incorrect_solution of level_t * (int list) * (int list) ;;
exception Impatient_measure_exn of level_t * (int list) ;;
exception Add_compatible_exn of level_t * (int list) * (int list);;
exception Add_decomposable_exn of level_t * (int list) * ((int list) list);;
exception Compute_ramification_exn of level_t * (int list) ;;
exception Pusher_for_decomposition_exn of level_t ;;   
exception Pusher_for_analysis_exn of level_t ;;   
exception Already_treated_exn of level_t * (int list) ;;


module type HAS_MEASURE = sig 
  val current_width : int
  val impatient_measure : int list -> int list 
  val patient_measure : int list -> int list 
  val analize : int list list -> (int * ((Detailed_solution.t list)* (Detailed_solution.t list))) list
end ;;



module Next_level = functor (Preceding_level:HAS_MEASURE) -> struct

let current_width = (Preceding_level.current_width) + 1 ;; 
let current_level = L current_width ;;
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let old_measure = Preceding_level.impatient_measure ;;
let old_patient_measure = Preceding_level.patient_measure ;;
let old_analize = Preceding_level.analize ;;



let check_solution sol_x x =
    if (i_is_included_in sol_x x) && (is_admissible sol_x)
    then ()
    else raise(Incorrect_solution(current_level,sol_x,x)) ;;   

let current_obstructions x = 
  let temp0 = Image.image (fun t->[t-2*current_width;t-current_width;t]) x in 
  List.filter (fun obstruction -> i_is_included_in obstruction x) temp0 ;;

let latest_obstruction x = 
  let temp0 = List.rev_map (fun t->[t-2*current_width;t-current_width;t]) x in 
  Listennou.force_find (fun obstruction -> i_is_included_in obstruction x) temp0 ;;

let hashtbl_for_patient_measure = 
  (( Hashtbl.create 100 ):  
   (int list, int list) Hashtbl.t );;

let force_compute_patient_measure old_f x=  
   let candidate = old_patient_measure x in 
   if is_admissible candidate 
   then candidate 
   else 
  let latest_obs = latest_obstruction x  in 
  let temp1 = Image.image (fun t->old_f (i_outsert t x)) latest_obs in 
  let (_,temp2) = Max.maximize_it_with_care List.length temp1 in 
  let temp3 = il_sort temp2 in 
  List.hd temp3 ;;
    
let rec patient_measure x = 
  let (opt_good,opt_bad) =
  Sz_preliminaries.evaluate_using_translation_and_distancing 
    current_width (Hashtbl.find_opt hashtbl_for_patient_measure) x in 
  match opt_good with 
   Some old_answer -> old_answer 
    |None ->
       let (whole,bad_part) = Option.unpack opt_bad in 
       let fixed_bad_part = Image.image (fun
         (opt_good2,opt_bad2) ->
            let (_,y) = Option.unpack opt_bad2 in 
            let z = force_compute_patient_measure patient_measure y in 
            let _ = Hashtbl.add hashtbl_for_patient_measure y z in 
            (y,force_compute_patient_measure patient_measure y)
       ) bad_part in 
       let parts = Image.image (
         fun (opt_good3,opt_bad3) -> match opt_good3 with 
           Some old_answer2 -> old_answer2 
           | None -> let (d2,y2) = Option.unpack opt_bad3 in 
                     let z2 = List.assoc y2 fixed_bad_part in 
                     Image.image (fun t->t+d2) z2  
       ) whole in 
      List.flatten parts;;  


let hashtbl_for_impatient_measure = 
        (( Hashtbl.create 100 ):  
         (int list, int list) Hashtbl.t );;


let impatient_measure_opt x = 
  let (opt_good,opt_bad) =
  Sz_preliminaries.evaluate_using_translation_and_distancing current_width 
    (Hashtbl.find_opt hashtbl_for_impatient_measure) x in 
  opt_good  ;;  

   
let  impatient_measure x =  
    let (opt_good,opt_bad) =
    Sz_preliminaries.evaluate_using_translation_and_distancing current_width 
    (Hashtbl.find_opt hashtbl_for_impatient_measure) x in 
    match opt_good with 
     Some old_answer -> old_answer 
      |None -> 
        let (whole,bad_part) = Option.unpack opt_bad in 
        let summary_of_bad = Image.image (fun 
          (opt_good2,opt_bad2) -> snd(Option.unpack opt_bad2)
        ) bad_part in 
        raise(Impatient_measure_exn(current_level,List.hd summary_of_bad)) ;;    

let add_to_both x y = 
  (
    Hashtbl.add hashtbl_for_patient_measure x y ;
    Hashtbl.add hashtbl_for_impatient_measure x y
  ) ;;

let ref_for_additions = ref [] ;;  

let add_to_refs dsol = 
  (ref_for_additions:=Ordered.insert Detailed_solution.order dsol (!ref_for_additions)) ;;

let check_nonredundancy arg =
   if List.exists (fun dsol2->dsol2.Detailed_solution.argument =arg ) (!ref_for_additions) 
   then raise(Already_treated_exn(current_level,arg))
   else () ;; 

let add_compatible x opt_sol_for_x=
  let _ = check_nonredundancy x in 
  let old_sol = old_measure x in 
  let sol_for_x = (
    match opt_sol_for_x with 
     Some sol -> sol 
     | None -> old_sol
  ) in 
  let old_length = List.length old_sol in 
  if not(
      (is_admissible sol_for_x)&&
     (i_is_included_in sol_for_x x)&&
     (List.length sol_for_x = old_length)
    ) 
  then raise(Add_compatible_exn(current_level,x,sol_for_x)) 
  else let dsol = {
       Detailed_solution.level = current_level ;
       argument = x ;
       case = Case.Compatible ;
       solution = sol_for_x ;
       is_compatible = (opt_sol_for_x = None) ;
       } in (
        add_to_both x sol_for_x ;
        add_to_refs dsol ;
       );;

let add_decomposable x parts sol_for_x = 
   let _ = check_nonredundancy x in 
   let _ = check_for_partition x parts in 
   let _ = check_solution sol_for_x x in 
   let combined_length = Basic.fold_sum(Image.image (fun part->
    List.length(impatient_measure part)) parts) in 
   if (List.length(sol_for_x)<>combined_length)
   then raise(Add_decomposable_exn(current_level,sol_for_x,parts)) 
   else let dsol = {
          Detailed_solution.level = current_level ;
          argument = x ;
          case = Case.Decomposable(parts) ;
          solution = sol_for_x ;
          is_compatible = false ;
        } in 
        (
          add_to_both x sol_for_x ;
          add_to_refs dsol ;
        );;

  
let add_ramification x obs = 
  let _ = check_nonredundancy x in 
  let temp1 = Image.image (fun t-> impatient_measure (i_outsert t x)) obs in 
  let (_,temp2) = Max.maximize_it_with_care List.length temp1 in 
  let temp3 = il_sort temp2 in 
  let answer = List.hd temp3 in 
  let dsol = {
          Detailed_solution.level = current_level ;
          argument = x ;
          case = Case.Ramifiable(obs) ;
          solution = answer ;
          is_compatible = false ;
        } in 
        (
          add_to_both x answer ;
          add_to_refs dsol ;
        );;
   
  


let start_decomposing  x = 
  let n = List.length x 
  and m =List.length (patient_measure x) in 
  Option.seek (
     fun k->
       let (rleft,right) = Listennou.big_rht k x in
       let left = List.rev rleft in 
       let size_left  = List.length(patient_measure left)
       and size_right = List.length(patient_measure right) in 
       m = size_left + size_right  
  ) (Int_range.ennig 1 (n-1)) ;;

let pusher_for_full_decomposition  (treated,to_be_treated) = 
       match to_be_treated with 
       [] -> raise( Pusher_for_decomposition_exn current_level)
       | part :: other_parts ->
         (match  start_decomposing part with 
          None -> (part::treated,other_parts)
          | Some k ->
           let (rleft,right) = Listennou.big_rht k part in
           let left = List.rev rleft in 
           (left::treated,right::other_parts))  ;;


let full_decomposition x =
   let rec tempf = (fun (treated,to_be_treated)->
      match to_be_treated with 
      [] -> List.rev treated 
      | _ -> let next =  pusher_for_full_decomposition  (treated,to_be_treated) in 
          tempf next 
    ) in 
    tempf([],[x]) ;;

let latest_extra extra_obstructions = 
  let m = List.length (List.hd extra_obstructions) in 
  let temp1 = List.filter (fun z -> List.length z = m) extra_obstructions in 
  let n = Max.list(List.flatten temp1) in 
  let reverter = (fun z->List.rev_map (fun t->n+1-t) z) in 
  let temp2 = il_sort(Image.image reverter temp1) in 
  reverter(List.hd temp2) ;;

let extended_old_patient_measure = Memoized.recursive( fun old_f (x,extra_obstructions) ->
   if extra_obstructions = [] 
   then old_patient_measure x 
   else
    let latest_obs = latest_extra extra_obstructions in 
    let temp1 = Image.image (fun t->old_f (i_outsert t x,
     il_sort(List.filter (fun z->not(i_mem t z)) extra_obstructions)
    )) latest_obs in 
    let (_,temp2) = Max.maximize_it_with_care List.length temp1 in 
    let temp3 = il_sort temp2 in 
    List.hd temp3    
) ;;  

let compute_ramification x=
  let obses = current_obstructions x in 
  let n = List.length obses and m=List.length(patient_measure x) in 
  match Option.seek (
    fun k->
       let limited_obses = Listennou.big_head k obses in 
       List.length(extended_old_patient_measure(x,limited_obses))<=m
  )(Int_range.ennig 1 n) with 
  Some k0 -> List.nth obses (k0-1)
  | None -> raise (Compute_ramification_exn(current_level,x)) ;;

let first_analysis x= 
  if List.length(patient_measure x)=List.length(old_patient_measure x) 
  then Case.Compatible 
  else 
  let dec_for_x=full_decomposition  x in 
  if dec_for_x<>[x] 
  then Case.Decomposable(dec_for_x) 
  else Case.Ramifiable(compute_ramification x) 
;;



let pusher_for_analysis (treated,to_be_treated) =
  match to_be_treated with 
  [] -> raise(Pusher_for_analysis_exn current_level)
  |x1 :: other_xs ->
    if impatient_measure_opt x1<>None 
    then (treated,other_xs)
    else    
    let y1 = first_analysis x1 in 
    let towards_new_work = (
        match y1 with 
           Case.Compatible -> []
          |Case.Decomposable(parts)-> parts 
          |Case.Ramifiable(breakers) -> List.rev_map (fun t->i_outsert t x1) breakers
        ) in 
    let pre_new_work = List.flatten(Image.image 
     (Sz_preliminaries.decompose_using_translation_and_distancing current_width) towards_new_work) in     
    let new_work = List.filter (
           fun z -> impatient_measure_opt z=None  
        ) pre_new_work in 
  (Ordered.insert Case.order_for_analysis_pairs (x1,y1) treated,new_work@other_xs) ;;

let rec helper_for_analysis (treated,to_be_treated) =
  match to_be_treated with 
   [] -> treated 
  |x1 :: other_xs ->
    let next = pusher_for_analysis (treated,to_be_treated) in 
    helper_for_analysis next ;;

let old_measure_opt x = try Some(old_measure x) with _ -> None ;;

let analize lx = 
   let unordered_data_for_current_level =
   Image.image (fun (a,b)->
    let old_a = old_patient_measure a
    and new_a = patient_measure a in   
    {
      Detailed_solution.level = current_level ;
      argument = a ;
      case = b ;
      solution = new_a ;
      is_compatible = (old_a = new_a) ;
   } ) 
    (helper_for_analysis ([],lx)) in
   let data_for_current_level = Ordered.sort Detailed_solution.order unordered_data_for_current_level in 
   if data_for_current_level = [] then [] else  
   let related_to_preceding_level = Option.filter_and_unpack (
      fun  dsol -> 
        let arg = dsol.Detailed_solution.argument in 
        if (dsol.Detailed_solution.case = Case.Compatible)&&(old_measure_opt arg=None) 
        then Some(arg) 
        else None
   ) data_for_current_level in 
   (current_width,(!ref_for_additions,data_for_current_level)) :: 
   (old_analize related_to_preceding_level);;

end ;;

module Level_one = struct 

  let current_width = 1 ;;

  let impatient_measure x =  
    let temp1 = Arithmetic_list.decompose_into_connected_components x in 
    let temp2 = Image.image (
       fun (a,b) -> List.filter (fun x->
         ((x-a) mod 3)<2
        ) (Int_range.ennig a b)) temp1 in 
    List.flatten temp2 ;;      
  
  let patient_measure = impatient_measure ;;
  let analize lx = [] ;;

  end ;;  

module Level_two = Next_level (Level_one) ;;  
module Level_three = Next_level (Level_two) ;;  
module Level_four = Next_level (Level_three) ;; 

exception Unknown_depth ;;

let bare_g_add_compatible = function 
   2 -> Level_two.add_compatible 
  |3 -> Level_three.add_compatible  
  |4 -> Level_four.add_compatible  
  |_ -> raise Unknown_depth ;;

let bare_g_add_ramification = function 
  2 -> Level_two.add_ramification 
 |3 -> Level_three.add_ramification  
 |4 -> Level_four.add_ramification 
 |_ -> raise Unknown_depth ;;

let bare_g_add_decomposable = function 
   2 -> Level_two.add_decomposable 
  |3 -> Level_three.add_decomposable  
  |4 -> Level_four.add_decomposable
  |_ -> raise Unknown_depth ;;

exception G_add_compatible_exn of int * (int list) * (int list option) ;;
exception H_add_compatible_exn of int * (int list) * (int list option) * level_t * (int list) ;;

let thoughtful_g_add_compatible lvl x y_opt =
  try bare_g_add_compatible lvl x y_opt with 
  Impatient_measure_exn (lvl2, bad_arg) -> raise (H_add_compatible_exn(lvl,x,y_opt,lvl2,bad_arg))
  | _ -> raise (G_add_compatible_exn(lvl,x,y_opt)) ;; 

exception G_add_ramification_exn of int * (int list) * (int list) ;;

let thoughtful_g_add_ramification lvl x y =
  try bare_g_add_ramification lvl x y with 
  _ -> raise (G_add_ramification_exn(lvl,x,y)) ;; 
    
exception G_add_decomposable_exn of int * (int list) * (int list list) * (int list) ;;

let thoughtful_g_add_decomposable lvl x ll z =
  try bare_g_add_decomposable lvl x ll z with 
  _ -> raise (G_add_decomposable_exn(lvl,x,ll,z)) ;;     

let thoughtful_mode = ref true ;;

let g_add_compatible = if !thoughtful_mode then thoughtful_g_add_compatible else bare_g_add_compatible ;;
let g_add_ramification = if !thoughtful_mode then thoughtful_g_add_ramification else bare_g_add_ramification ;;
let g_add_decomposable = if !thoughtful_mode then thoughtful_g_add_decomposable else bare_g_add_decomposable ;;

let a2 l = Level_two.analize [l] ;;
let a3 l = Level_three.analize [l] ;;
let a4 l = Level_four.analize [l] ;;
let sc = Detailed_solution.show_lightweight_commands ;;
let zc = Detailed_solution.show_lightweight_commands_without_reacting ;;
let sa n = sc  (a4 (Int_range.ennig 1 n)) ;; 

for k = 1 to small_n do g_add_compatible 2 (Int_range.ennig 1 k) None done ;;

(* Beginnning of precomputed data for level 2 *)


g_add_compatible 2 [1;3] None ;;
(* new *) g_add_compatible 2 [1;3;4] None ;;
g_add_compatible 2 ([1;2;3]@[5]) None ;;
g_add_compatible 2 ([1]@[3;4;5]) None ;;
g_add_compatible 2 ((Int_range.ennig 1 4)@[6]) (Some(([1]@[3;4]@[6]))) ;;
g_add_compatible 2 ([1;2;3]@[5;6]) None ;;
g_add_compatible 2 ([1;2;3]@[5]@[7]) None ;;
g_add_compatible 2 ([1;2]@[4;5;6]) None ;;
g_add_compatible 2 ([1]@(Int_range.ennig 3 6)) None ;;
g_add_compatible 2 ([1]@[3]@[5;6;7]) (Some(([1]@[3]@[6;7]))) ;;
g_add_compatible 2 ([1;2;3]@[5;6;7]) None ;;
g_add_compatible 2 ([1;2;3]@[6;7;8]) None ;;
g_add_ramification 2 [1;3;5] [1;3;5] ;;
g_add_decomposable 2 ([1]@[3]@[5]@[7]) [[1];[3;5;7]] [1;3;7] ;;
(* new *) g_add_decomposable 2 ([1;2]@[4]@[6]@[8]) [[1];[2];[4;6;8]] ([1;2]@[4]@[8]) ;;
g_add_decomposable 2 ([1]@[3;4]@[6]@[8]) [[1];[3];[4;6;8]] ([1]@[3;4]@[6]) ;;
g_add_decomposable 2 ([1]@[3]@[5;6]@[8]) [[1;3;5];[6];[8]] ([1]@[3]@[6]@[8]) ;;
g_add_decomposable 2 ([1]@[3]@[5]@[7;8]) [[1];[3;5;7];[8]] ([1]@[3]@[7;8]) ;;
g_add_decomposable 2 ((Int_range.ennig 1 4)@[6]@[8]) [[1;2;3];[4;6;8]] ([1;2]@[4]@[8]) ;;
g_add_decomposable 2 ([1;2;3]@[5]@[7]@[9]) [[1;2;3];[5;7;9]] ([1;2]@[5]@[7]) ;;
g_add_ramification 2 ([1]@(Int_range.ennig 3 6)@[8]) [4;6;8] ;;
g_add_decomposable 2 ([1]@[3]@(Int_range.ennig 5 8)) [[1;3;5];[6;7;8]] ([1]@[3]@[6;7]) ;;
g_add_decomposable 2 ([1]@[3]@[5]@[7;8;9]) [[1;3;5];[7;8;9]] ([1]@[3]@[7;8]) ;;
g_add_decomposable 2 ([1;2]@[4]@[6;7]@[9]@[11]) [[1];[2;4;6];[7;9;11]] ([1;2]@[4]@[7]@[9]) ;;
g_add_decomposable 2 ([1]@[3]@(Int_range.ennig 5 8)@[10]@[12]@[14]) [[1;3;5];[6;7;8];[10;12;14]] ([1]@[3]@[6;7]@[10]@[12]) ;;
g_add_decomposable 2 ([1]@[3]@[5]@(Int_range.ennig 7 10)@[12]@[14]) [[1;3;5];[7;8;9];[10;12;14]] ([1]@[3]@[7;8]@[10]@[14]) ;;


(* End of precomputed data for level 2 *)

end ;;

let measure = Enclosed.Level_two.patient_measure ;;

let typical_question = Memoized.make (
   fun (x,k,constraints) ->
     let m = List.length(measure x) in 
     if (k<0)||(k>m) then None else 
     let versals = Ordered_misc.minimal_transversals constraints in 
     Option.find_and_stop (
        fun versal -> 
        let modified_x = i_setminus x versal in 
        let sol = measure modified_x in 
        let d = m - (List.length sol) in 
        if d > 0 then None else 
        if d = 0 then Some sol else 
        let shortened_sol = List.rev(Listennou.big_tail d (List.rev sol)) in 
        Some shortened_sol          
     )  versals
);;   

let current_width = 2 ;; 
let current_maxwidth = Sz_max_width_t.MW current_width ;;
let constraints_on_the_left n =
     let bound = (n-1)/2 in 
     Int_range.descending_scale (fun j->[n-2*j;n-j]) bound 1;;

     



let derivation_for_easy_case (base_set,size,constraints)= 
   let (n,temp1) = Listennou.ht(List.rev base_set) in 
   let new_base_set = List.rev temp1 
   and new_constraints = List.filter (fun constr->not(i_mem n constr)) constraints in 
   (new_base_set,size,new_constraints) ;;

let derivation_for_hard_case (base_set,size,constraints)= 
   let (n,temp1) = Listennou.ht(List.rev base_set) in 
   let new_base_set = List.rev temp1 
   and new_constraints = List.filter (fun constr->not(i_mem n constr)) constraints in 
   (new_base_set,size,new_constraints) ;;