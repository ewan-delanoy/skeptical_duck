(************************************************************************************************************************
Snippet 71 : 
************************************************************************************************************************)


(************************************************************************************************************************
Snippet 70 : Add painful debugging session on  initialize_toplevel
************************************************************************************************************************)





(************************************************************************************************************************
Snippet 69 : Enumerating subgroups of S4 
************************************************************************************************************************)


let i_order = Total_ordering.for_integers ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  Total_ordering.for_integers ;;
let il_sort  = Ordered.sort il_order ;;

let current_order = 4 ;;
let base = Permutation.iii current_order ;;

let eval_list_permutation sigma k = List.nth sigma (k-1) ;;

let compose_list_permutations sigma1 sigma2 = 
   Ennig.doyle (fun k-> eval_list_permutation sigma1 (eval_list_permutation sigma2 k)) 1 current_order ;;

let uncurried_compose = Memoized.make(fun (i,j) ->
   let sigma1 = List.nth base (i-1)   
   and sigma2 = List.nth base (j-1) in 
   Listennou.find_index (compose_list_permutations sigma1 sigma2) base
);;     

let compose  i j = uncurried_compose (i,j) ;;

let base_size = List.length base ;;

let subset_product l1 l2 =
    let temp1 = Cartesian.product l1 l2 in 
    let temp2 = Image.image uncurried_compose temp1 in 
    i_sort temp2 ;; 

let rec helper_for_generated_subgroup (treated,seed) = 
      let possibly_new = subset_product treated seed in 
      let really_new = Ordered.setminus Total_ordering.for_integers possibly_new treated in 
      if really_new = [] 
      then treated 
      else let new_whole = Ordered.merge Total_ordering.for_integers really_new treated in 
            helper_for_generated_subgroup (new_whole,seed) ;;
        
let generated_subgroup seed = helper_for_generated_subgroup ([1],seed) ;; 

let trivial_subgroup = [1] ;;
let full_subgroup = Ennig.ennig 1 base_size ;;

let level1  = 
  il_sort (Ennig.doyle (fun k->generated_subgroup [k]) 2 base_size) ;; 

let pre_level2 = 
    let temp1 = Uple.list_of_pairs level1 in 
    let temp2 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp1 in 
    il_sort temp2 ;;

let (next_to_level2,level2) = List.partition (fun x->List.mem x level1) pre_level2 ;;

let pre_level3 = 
  let temp1 = Cartesian.product level1 level2 in 
  let temp2 = List.filter (fun (x,y)->not(i_is_included_in x y)) temp1 in 
  let temp3 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp2 in 
  il_sort temp3 ;;

let (next_to_level3,level3) = List.partition (fun x->List.mem x level2) pre_level3 ;;  

let halves x =
   let n = (List.length x)/2 in 
   List.filter (fun y->((List.length y)=n)&&(i_is_included_in y x) ) (level1@level2) ;;

let is_transitive sg =
   let temp1 = Image.image (fun sigma -> 
    eval_list_permutation sigma 1
   ) sg in 
   (i_sort temp1) = (Ennig.ennig 1 current_order) ;;
   

let d4 = List.hd(List.filter (fun x->List.length x=8) level2) ;; 
let halves_for_d4 = halves d4 ;; 

let a4 = List.hd(List.filter (fun x->List.length x=12) level2) ;; 
let halves_for_a4 = halves a4 ;; 

let halves_for_whole = halves full_subgroup ;;

    

(************************************************************************************************************************
Snippet 68 : Finding a polynomial x^4+p*x+q with Galois group A4
************************************************************************************************************************)

let u1 = Ennig.ennig (-50) 50 ;;
let u2 = Cartesian.square u1 ;;
let u3 = Image.image (fun (x,y)->(max(abs x)(abs y),(x,y)) ) u2 ;;
let u4 = Ordered.sort Total_ordering.standard2 u3 ;;
let unchecked_u5 = Image.image snd u4 ;;
let u5 = List.filter (fun (p,q)->List.for_all (fun z->z*z*z*z+p*z+q<>0) 
(Ennig.ennig (-1) 1)) unchecked_u5 ;;

let round x=
  let fl = floor x in 
  if (x -. fl) < 0.5 
  then int_of_float fl 
  else (int_of_float fl)+1 ;;  

let is_a_square n = 
    if n< 0 then false else
    let m =round(sqrt(float_of_int n)) in m * m = n;;  

let check = List.filter is_a_square (Ennig.ennig 0 100) ;;    

let u6 = List.filter (fun (p,q)->is_a_square(-27*p*p*p*p + 256*q*q*q)) u5 ;;

(************************************************************************************************************************
Snippet 67 : Removing indentation in a paragraph in a file  
************************************************************************************************************************)

let ap1 = Absolute_path.of_string "Fads/pan.ml" ;;

let text1 = Io.read_whole_file ap1 ;; 

let (before_text2,text2,after_text2) =
  Lines_in_string.tripartition_associated_to_interval text1 11 60 ;;

let old_lines_in_text2 = Lines_in_string.lines text2 ;;  
let new_lines_in_text2 = Image.image (Cull_string.cobeginning 3) old_lines_in_text2 ;; 

let new_text2 = String.concat "\n" new_lines_in_text2 ;;
let new_text1 = String.concat "\n" [before_text2;new_text2;after_text2] ;;

Io.overwrite_with ap1 new_text1 ;;

(************************************************************************************************************************
Snippet 66 : Intertwining prints for debugging purposes
************************************************************************************************************************)

open Needed_values ;;

let z1 = rf "Fads/pan.ml" ;;
let z2 = Lines_in_string.interval z1 10 58 ;;
let z3 = Lines_in_string.lines z2 ;;
let z4 = List.filter (fun line -> Cull_string.trim_spaces line <> "" ) z3 ;; 
let z5 = Ennig.index_everything z4 ;; 
let z6 = Image.image (
  fun (j,line) ->
    let sj = string_of_int j in 
    line^"\nprint_int "^sj^" ;;"
) z5 ;;
let z7 = "\n\n\n" ^ (String.concat "\n" z6) ^ "\n\n\n" ;;  

(************************************************************************************************************************
Snippet 65 : Problem involving periodicity
************************************************************************************************************************)
let find_periodicity l= 
  let rl = List.rev l in 
  let (a1,after_a1) = Listennou.ht rl in 
  let j = Listennou.find_index a1 after_a1 in 
  let inverted_motif = Listennou.big_head j rl in 
  let motif = List.rev inverted_motif in 
  let p = List.length motif in 
  let m0 = Min.list motif in 
  let i0 = Listennou.find_index m0 motif in 
  let after_m0 = Listennou.big_tail i0 motif 
  and before_m0 = Listennou.big_head (i0-1) motif in
  (p,m0::(after_m0@before_m0)) ;; 


let current_r = 5 ;;
let current_m = Gcd.lcm_for_many (Ennig.ennig 2 current_r) ;;

let pusher old_f n = 
  let lower_bound = max 1 (n-current_r) in  
  let temp1 = Ennig.ennig lower_bound (n-1) in 
  let temp2 = Image.image (fun m->(old_f m)-(current_m/(n-m))) temp1 in 
  let first_trial = Min.list temp2 in 
  if first_trial > 0 then first_trial else 
  let temp3 = Image.image (fun m->(old_f m)+(current_m/(n-m))) temp1 in   
  Max.list temp3 ;;

let ff = Memoized.recursive (fun old_f n->if n<2 then 1 else pusher old_f n) ;;

let z1 = Ennig.doyle ff 1 200 ;;
let (period,motif) = find_periodicity z1 ;;
let last_in_motif = List.nth motif (period-1) ;;
let gg n = let r = n mod period in if r = 0 then last_in_motif else List.nth motif (r-1) ;;
let dg t = Min.list (Ennig.doyle (fun k->(abs(gg(k+t)-gg(k)))*t ) 1 period) ;;
let (max_dg,dg_sols) = Min.minimize_it_with_care dg (Ennig.ennig 1 current_r) ;;
let largest_in_motif = Max.list motif ;;
let ratio = (float_of_int(largest_in_motif-List.hd(motif))) /. (float_of_int max_dg);;


(************************************************************************************************************************
Snippet 64 : Visualize hierarchy of types in a poly-record class
************************************************************************************************************************)

type t3 = Fw_with_archives_t.t = {
   parent : File_watcher_t.t;
   subdirs_for_archived_mlx_files : Dfa_subdirectory_t.t list;
 };;
type t4 = Fw_with_small_details_t.t = {
   parent : Fw_with_archives_t.t;
   small_details_in_files : (Dfn_rootless_t.t * Fw_file_small_details_t.t) list;
 };;
type t5 = Fw_with_dependencies_t.t = {
   parent : Fw_with_small_details_t.t;
   index_for_caching : Fw_instance_index_t.t * Fw_state_index_t.t;
 };;
type t6 = Fw_with_batch_compilation_t.t = {
   parent : Fw_with_dependencies_t.t;
   last_compilation_result_for_module : (Dfa_module_t.t * bool) list;
 };;
type t7 = Fw_with_githubbing_t.t =  {
   parent : Fw_with_batch_compilation_t.t;
   dir_for_backup : Dfa_root_t.t;
   gitpush_after_backup : bool;
   github_url : string;
   encoding_protected_files : (Dfn_rootless_t.t * Dfn_rootless_t.t) list;
 };;

(************************************************************************************************************************
Snippet 63 : Musings on the Szemeredi problem, chapter V
************************************************************************************************************************)
let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let is_not_admissible x= (not(is_admissible x));;
let uncurried_sl  = Memoized.make (fun (x,k)->
  let temp1 = Sz_preliminaries.restricted_power_set (max_width,x) in 
  List.filter (fun z->List.length z=k) temp1 
) ;;  
let sl x k = uncurried_sl (x,k) ;;
let isl n k = uncurried_sl (Ennig.ennig 1 n,k) ;; 
let meas = Sz_precomputed.measure max_width ;;


let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;



let original_minimal_carriers carriers sols =
  let indexed_carriers = Ennig.index_everything carriers in 
  let shadow = (
      fun sol ->
         Option.filter_and_unpack (
          fun (idx,carrier) -> 
             if i_is_included_in carrier sol 
             then Some idx 
            else None 
        ) indexed_carriers 
  )  in     
  let all_shadows = Image.image shadow sols in 
  Ordered_misc.minimal_transversals all_shadows ;;
 
let u_product sheaf1 sheaf2 = 
    let temp1 = Cartesian.product sheaf1 sheaf2 in 
    let temp2 = Image.image (fun (x,y)->i_merge x y) temp1 in 
    let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
    il_sort temp3 ;;

 
let fold_u_product = function 
  [] -> []
  | sheaf :: other_sheaves -> List.fold_left  u_product sheaf other_sheaves ;;   
   

exception Nonunique_set_of_minimal_carriers of int list list list ;;
 
let set_of_minimal_carriers carriers sols =
 let version1 = original_minimal_carriers carriers sols in 
 let m = List.length(List.hd version1) in 
 let version2 = List.filter (fun x->List.length(x)=m) version1 in 
 let visualize = Image.image (fun idx->List.nth carriers (idx-1)) in 
 let version3 = Image.image visualize version2 in 
 if (List.length version3)<>1
 then raise (Nonunique_set_of_minimal_carriers version3)
 else List.hd version3;;
 
let set_of_minimal_carriers_with_extra carriers sols =
 try (Some(set_of_minimal_carriers carriers sols),None) with 
 Nonunique_set_of_minimal_carriers(nonunique) -> (None, Some nonunique)   ;;

let remains_of_obstructions_in_positing_case x=
  Option.filter_and_unpack (fun j->
      let k=(current_width+1)-j in 
      if x>2*k 
      then  Some [x-2*k;x-k]
      else None
  ) (Ennig.ennig 1 current_width) ;;

  

let analize_sheaf1 (left,bound,right) =
    let m = List.hd(List.rev right) in 
    let carriers = Sz_preliminaries.force_subset_in_interval    
      max_width right (1,m) in 
      set_of_minimal_carriers_with_extra carriers (sl left bound);;

let ref_for_missing_sheaves = ref [] ;;

exception Troublesome_aftersheaf of int list * int * int list * (int list list list);;
exception Missing_sheaves of (int list * int *  (int list list)) list ;;
  
let analize_sheaf2 (left,bound,right) =
  let (good_opt,bad_opt) = analize_sheaf1(left,bound,right) in 
  (
    match good_opt with 
     None -> raise(Troublesome_aftersheaf(left,bound,right,Option.unpack bad_opt)) 
    |Some usual -> 
      let _ = (ref_for_missing_sheaves:=[left,bound,usual]) in
      raise(Missing_sheaves [left,bound,usual])
  ) ;;    

let carrier_is_stronger_than_another carrier1 carrier2 =
   List.for_all (fun c1 -> 
    List.exists (fun c2 -> i_is_included_in c2 c1) carrier2) carrier1 ;;

exception Two_carriers_exn of 
  (int list) * int * (int list list) * (int list list) * (int list list list);;

let add_carrier_to_another (x,bound) carrier old_carrier =
   if carrier_is_stronger_than_another carrier old_carrier 
   then [carrier]
   else 
   let carrier2 = u_product carrier old_carrier in 
   let (good_opt,bad_opt) = set_of_minimal_carriers_with_extra carrier2 (sl x bound) in 
   if good_opt<>None
   then [Option.unpack good_opt]
   else raise (Two_carriers_exn(x,bound,carrier,old_carrier,Option.unpack bad_opt));; 

exception Add_carrier_exn of (int list) * int * (int list list) * (int list list list) ;;

let add_carrier (x,bound) carrier old_list = 
   match List.length old_list with 
    0 -> [carrier] 
   | 1 -> add_carrier_to_another (x,bound) carrier (List.hd old_list) 
   | _ -> raise (Add_carrier_exn(x,bound,carrier,old_list));;


let hashtbl_for_sheaves = ((Hashtbl.create 100): 
   ((int list) * int, int list list list) Hashtbl.t) ;;


let add_sheaf (x,bound) carrier =
  let new_val = (
   match Hashtbl.find_opt hashtbl_for_sheaves (x,bound)  with 
    Some (old_val) -> add_carrier (x,bound) carrier  old_val
   | None -> [carrier]
   ) in 
   Hashtbl.replace hashtbl_for_sheaves (x,bound) new_val ;;
   
let consult_sheaves (left,bound,right) =
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
     None -> None
   | Some (sheaves) -> 
     Option.seek (fun sheaf->
       List.for_all (fun z->is_not_admissible (z@right)) sheaf
      ) sheaves
  ;;        

let sheaf_compare new_ll old_ll  =
    List.for_all (fun old_l-> List.exists 
    (fun new_l->i_is_included_in new_l old_l) new_ll ) old_ll  ;;

let sheaf_compare2 new_ll old_lls =
    List.exists (sheaf_compare new_ll) old_lls ;;
       
let sheaf_is_not_already_known_by_sheaves_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
  None -> true
| Some (old_sheaves)  -> not(sheaf_compare2 right old_sheaves)  ;;       

let hashtbl_for_pre_measure = ((Hashtbl.create 100) :
(int list ,int list) Hashtbl.t) ;;

let sheaf_is_not_already_known_by_measures_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_pre_measure left  with 
  None -> if is_admissible left 
          then (List.length left) >= bound
          else true
| Some (old_sol)  -> (List.length old_sol) >= bound  ;;   

let sheaf_is_not_already_known triple = 
   if sheaf_is_not_already_known_by_measures_hashtbl triple 
   then  sheaf_is_not_already_known_by_sheaves_hashtbl triple 
   else false ;; 

exception Borderline_case of ((int list) * int * ((int list) list)) list ;;

let commonest_decomposition (x,bound,carriers) =
    let (m,ry) = Listennou.ht(List.rev x) in 
    let y = List.rev ry in 
   let rem_obstr1 = remains_of_obstructions_in_positing_case m 
   and (pre_rem_obstr3,rem_obstr2) = List.partition (fun z->List.mem m z) carriers in    
   let rem_obstr3 = Image.image (i_outsert m) pre_rem_obstr3 in       
   let temp = (if List.mem [m] carriers then [] else 
    [
     (y,bound-1,il_fold_merge [rem_obstr1;rem_obstr2;rem_obstr3]);
    ])
   @ 
    [(y,bound,rem_obstr2)] in 
  let (cleaned_temp,dirty_temp) = List.partition sheaf_is_not_already_known_by_measures_hashtbl temp in    
  let (temp_good,temp_bad) = List.partition (fun (_,_,obstr2)->obstr2<>[]) cleaned_temp in   
  if temp_bad <> [] 
  then raise(Borderline_case(temp_bad)) 
  else    
  let temp3 = Image.image (
    fun tr3 ->
       let (y3,bound3,obstr3) = tr3 in 
       (tr3,set_of_minimal_carriers_with_extra obstr3 (sl y3 bound3))
  ) temp_good in 
  let (temp3_good,temp3_bad) = List.partition (fun 
    (_,(good_opt,bad_opt)) -> good_opt <> None
  ) temp3 in 
  (
    Image.image (fun ((y,bound,_),(good_opt,bad_opt))->(y,bound,Option.unpack good_opt) ) temp3_good,
    Image.image (fun (tr,(good_opt,bad_opt))->tr ) temp3_bad,
    dirty_temp
  );;   



let enhanced_commonest_decomposition triple =
  let (temp1,temp2,temp3) = commonest_decomposition triple in 
  (List.filter sheaf_is_not_already_known temp1, 
   List.filter sheaf_is_not_already_known temp2,
   temp3);;   

exception Add_sheaf_carefully_exn of (int list * int * int list list) list ;;

let add_sheaf_carefully (x,bound) carrier =
  let (temp1,temp2,_) = enhanced_commonest_decomposition (x,bound,carrier) in 
  let temp3 = temp1 @ temp2 in 
  if temp3 <> []
  then raise(Add_sheaf_carefully_exn(temp3))     
  else add_sheaf (x,bound) carrier ;;

let consult_sheaves_and_double_check (left,bound,right) =
  match consult_sheaves (left,bound,right) with 
    Some obstr -> Some obstr 
   | None ->
     let sols = sl left bound in 
     if List.for_all (fun z->is_not_admissible (z@right)) sols
     then analize_sheaf2(left,bound,right) 
     else None ;;
     


let analize_missing_sheaves ()=
  let triple = List.hd(!ref_for_missing_sheaves) in 
  let (temp1,temp2,temp3) = enhanced_commonest_decomposition triple in 
  let _= (ref_for_missing_sheaves:=temp1@temp2) in 
  (temp1,temp2,temp3);;

let default_string_of_intlist l=  
  "["^(String.concat ";" 
  (Image.image string_of_int l)
  )^"]";;  

  let ennified_string_of_intlist l=
    let a = List.hd(l) and b = List.hd(List.rev l) in 
    if (l=Ennig.ennig a b)
    then  "Ennig.ennig "^(string_of_int a)^" "^(string_of_int b)    
  else default_string_of_intlist l;;

let string_of_intlistlist ll=  
"["^(String.concat ";" 
(Image.image default_string_of_intlist ll)
)^"]";;

let message_for_newcomer (x,bound,carriers) =
 "asc ("^(ennified_string_of_intlist x)^","^(string_of_int bound)^") "^
  (string_of_intlistlist carriers)^" ;;" 
 ;;

let message_for_newcomers l= 
  "\n\n\n"^(String.concat "\n" 
  (Image.image message_for_newcomer l)
  )^"\n\n\n";;

let analize_repeatedly initial_triple =
   let triple1 = enhanced_commonest_decomposition initial_triple in  
   let (bt1,bt2,_) = triple1 in 
   if (bt1,bt2) = ([],[]) 
   then let msg = "\n\n\n"^(message_for_newcomer initial_triple)^"\n\n\n" in 
        let _= (print_string msg;flush stdout) in 
        (initial_triple,triple1)
   else  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (initial_triple,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple1 ;;  
    

let hashtbl_for_solving = ((Hashtbl.create 100) :
(int list * int * int list, int list) Hashtbl.t) ;;



let solve_in_easy_case (left,bound,right) = 
  let p = List.length left in 
  if p < bound 
  then Some None 
  else 
  if p = bound 
  then let unique = left @ right in 
       if is_admissible unique 
       then Some (Some unique)
       else Some None  
  else None ;;

let quick_way_out1 (left,bound,right) =
    if bound<>List.length(left) then false else 
    not(is_admissible(left@right)) ;;  

let pre_measure_arg whole              = (false,whole,0,[]) ;;
let solve_arg   (left,bound,right) = (true,left,bound,right) ;;
let pre_measure_ret m = (m,None) ;;
let solve_ret opt= ([],opt) ;;


let induction_in_solve_case old_f triple = 
  match solve_in_easy_case triple with 
   Some easy_answer -> solve_ret easy_answer 
   | None ->
  (
   match Hashtbl.find_opt hashtbl_for_solving triple with 
   Some old_answer -> solve_ret (Some old_answer) 
   | None ->
    let old_solve = (fun tr -> snd (old_f(solve_arg(tr)))) in 
    let opt_sol = (
      let (left,bound,right) = triple in  
      let (m,ry) = Listennou.ht(List.rev left) in 
      let y = List.rev ry in 
      if is_not_admissible (m::right)
      then old_solve(y,bound,right)
      else     
      let temp1 = [(y,bound-1,m::right);(y,bound,right)] in 
      let my = List.length(fst (old_f (pre_measure_arg(y)))) in 
      let temp2 = List.filter (fun (y2,bound2,_)->bound2<=my) temp1 in 
      let temp3 = List.filter (fun tr->not(quick_way_out1 tr)) temp2 in 
      let temp4 = List.filter (
          fun tr -> (consult_sheaves_and_double_check tr) = None
      )  temp3 in  
      let temp5 = Option.filter_and_unpack old_solve temp4 in 
      if temp5 = []
      then None  
      else Some(List.hd(List.rev temp5))
    ) in 
    let _ = (if opt_sol <>None 
      then Hashtbl.add hashtbl_for_solving triple (Option.unpack opt_sol)) in 
    solve_ret opt_sol
  );;

let induction_in_pre_measure_case old_f whole = 
   if is_admissible whole 
   then pre_measure_ret(whole) 
   else 
   match Hashtbl.find_opt hashtbl_for_pre_measure whole with 
    Some old_answer -> pre_measure_ret(old_answer) 
   |None -> 
   let new_answer = (
    let (m,ry) = Listennou.ht(List.rev whole) in 
   let y = List.rev ry in  
   let sy = fst(old_f(false,y,0,[])) in 
   if is_admissible(sy@[m])
   then  pre_measure_ret(sy@[m])
   else   
   (
     match snd(old_f(true,y,List.length sy,[m])) with 
     None -> pre_measure_ret(sy)
     |Some fitting_one -> pre_measure_ret(fitting_one@[m])
   )) in 
  let _ = Hashtbl.add hashtbl_for_pre_measure whole (fst new_answer) in 
  new_answer;;
   

let rec main_iterator (case,left,bound,right) =
  if case 
  then induction_in_solve_case main_iterator (left,bound,right) 
  else induction_in_pre_measure_case main_iterator left ;;  


let pre_measure whole            = fst(main_iterator(pre_measure_arg(whole))) ;; 
let solve (left,bound,right) = snd(main_iterator(solve_arg(left,bound,right))) ;;  

let clear_hashtables () = (
    Hashtbl.clear hashtbl_for_sheaves  ;
    Hashtbl.clear hashtbl_for_solving  ;
    Hashtbl.clear hashtbl_for_pre_measure  ;
) ;;

exception FF_exn of (( int list * int * int list list) *
((int list * int * int list list) list *
(int list * int * int list list) list *
(int list * int * int list list) list)) ;;

let ff n = 
  try pre_measure (Ennig.ennig 1 n) with 
  Missing_sheaves(l) ->
    let (t1,t2,t3) = List.hd l in 
    let (a,b) = analize_repeatedly(t1,t2,t3) in 
    raise(FF_exn(a,b)) ;;


let ams = analize_missing_sheaves;;
let asc = add_sheaf_carefully ;;

let comp1 = Ennig.doyle ff 1 5 ;;

asc (Ennig.ennig 1 4,3) [[4]] ;; 
ff 6;;
asc (Ennig.ennig 1 1,1) [[1]] ;; 
asc (Ennig.ennig 1 2,1) [[1];[2]] ;; 
asc (Ennig.ennig 1 2,2) [[1]] ;; 
asc (Ennig.ennig 1 3,2) [[1];[2;3]] ;;
asc (Ennig.ennig 1 4,3) [[1;4]] ;; 
asc (Ennig.ennig 1 5,3) [[5];[1;4]] ;;
asc (Ennig.ennig 1 5,4) [[1;4]] ;;
ff 7;;
asc (Ennig.ennig 1 2,2) [[2]] ;; 
asc (Ennig.ennig 1 3,1) [[1];[2];[3]] ;;
asc (Ennig.ennig 1 3,2) [[2];[1;3]] ;;
asc (Ennig.ennig 1 4,2) [[2];[1;3];[1;4];[3;4]] ;;
asc (Ennig.ennig 1 4,3) [[2];[1;3];[3;4]] ;;
asc (Ennig.ennig 1 5,3) [[1;4];[2;5]] ;;
asc (Ennig.ennig 1 5,4) [[2;5]] ;;
asc (Ennig.ennig 1 6,3) [[6];[1;4];[2;5]] ;;
asc (Ennig.ennig 1 6,4) [[2;5];[4;6]] ;;
Ennig.doyle ff 8 10;;
asc (Ennig.ennig 1 9,5) [[9]] ;; 
Ennig.doyle ff 11 13;;
asc (Ennig.ennig 1 12,7) [[12]] ;; 
ff 14;;
asc (Ennig.ennig 1 10,5) [[9];[10]] ;;
asc (Ennig.ennig 1 10,6) [[9]] ;;
asc (Ennig.ennig 1 11,6) [[9];[10;11]] ;;
asc (Ennig.ennig 1 12,7) [[9;12]] ;;
asc (Ennig.ennig 1 13,7) [[13];[9;12]] ;;
asc (Ennig.ennig 1 13,8) [[9;12]] ;;
ff 15;;
asc (Ennig.ennig 1 10,6) [[10]] ;;
asc (Ennig.ennig 1 11,5) [[9];[10];[11]] ;;
asc (Ennig.ennig 1 11,6) [[10];[9;11]] ;;
asc (Ennig.ennig 1 12,6) [[10];[9;11];[9;12];[11;12]] ;;
asc (Ennig.ennig 1 12,7) [[10];[7;10];[9;11];[11;12]] ;;
asc (Ennig.ennig 1 13,7) [[9;12];[10;13]] ;;
asc (Ennig.ennig 1 13,8) [[10;13]] ;;
asc (Ennig.ennig 1 14,7) [[14];[9;12];[10;13]] ;;
asc (Ennig.ennig 1 14,8) [[10;13];[12;14]] ;;
Ennig.doyle ff 16 18;;
asc (Ennig.ennig 1 17,9) [[17]] ;;
Ennig.doyle ff 19 21;;
asc (Ennig.ennig 1 20,11) [[20]] ;;

(*


let l =  [[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13], 7, [[13]; [9; 12]]] ;;
let (bt1,bt2,bt3) = List.hd l ;;
let (a,b) = analize_repeatedly(bt1,bt2,bt3) ;;
let triple0 = enhanced_commonest_decomposition (bt1,bt2,bt3) ;;    

let analize_repeatedly (x,bound,carriers) =
   let triple0 = enhanced_commonest_decomposition (x,bound,carriers) in  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (triple0,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple0 ;;  


asc (Ennig.ennig 1 10,5) [[9];[10]] ;; 
asc (Ennig.ennig 1 5,4) [[2;5]] ;;

let g1 = Hashtbl.find hashtbl_for_sheaves (Ennig.ennig 1 4,3) ;;
asc (Ennig.ennig 1 4, 3)  [[2]; [1; 3]; [3; 4]] ;;

*)

(************************************************************************************************************************
Snippet 62 : Musings on the Szemeredi problem, chapter IV
************************************************************************************************************************)
let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let is_not_admissible x= (not(is_admissible x));;

let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;

let uncurried_sl  = Memoized.make (fun (n,k)->
   let temp1 = Sz_preliminaries.restricted_power_set (max_width,Ennig.ennig 1 n) in 
   List.filter (fun z->List.length z=k) temp1 
) ;;  
let sl n k = uncurried_sl (n,k) ;;

let original_minimal_carriers carriers sols =
  let indexed_carriers = Ennig.index_everything carriers in 
  let shadow = (
      fun sol ->
         Option.filter_and_unpack (
          fun (idx,carrier) -> 
             if i_is_included_in carrier sol 
             then Some idx 
            else None 
        ) indexed_carriers 
  )  in     
  let all_shadows = Image.image shadow sols in 
  Ordered_misc.minimal_transversals all_shadows ;;
 
exception Nonunique_set_of_minimal_carriers ;;
 
let set_of_minimal_carriers carriers sols =
 let version1 = original_minimal_carriers carriers sols in 
 let m = List.length(List.hd version1) in 
 let version2 = List.filter (fun x->List.length(x)=m) version1 in 
 if (List.length version2)<>1
 then raise Nonunique_set_of_minimal_carriers 
 else 
 Image.image (fun idx->List.nth carriers (idx-1)) (List.hd version2)
 ;;
 
let set_of_minimal_carriers_with_extra carriers sols =
 try (Some(set_of_minimal_carriers carriers sols),None) with 
 _ -> (None, Some carriers)   ;;


let tag1 = Ennig.doyle (fun x->[x;2*x-1]) 2 (1+current_width) ;;
let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;

module ConstraintList = struct 

type t = CL of (int * ((int list) list) ) list ;;

exception Constraint_already_present of (int list) * int ;; 

let helper_for_adding (x,j,cstr_for_xj) cl_content=
   let rec tempf = (fun (above_j,to_be_treated) ->
   match to_be_treated with 
     [] -> List.rev ((j,cstr_for_xj)::above_j) 
     |k_pair :: others ->
        let (k,cstr_for_k) = k_pair in 
        (match Total_ordering.for_integers j k with 
         Total_ordering_result_t.Lower->tempf(k_pair::above_j,others)
        |Greater-> 
          List.rev_append ((j,cstr_for_xj)::above_j) to_be_treated
        |Equal -> raise (Constraint_already_present(x,j))
        )
   ) in
   tempf([],cl_content) ;;

let add (x,j,cstr_for_xj) (CL cl_content) = 
    CL (helper_for_adding (x,j,cstr_for_xj) cl_content) ;;
let at_index (CL cl_content) idx = List.assoc_opt idx cl_content ;;    
let empty_one = CL [] ;;

end ;;  


module Oar = struct 

type t = {
  solution : (int list) ;
  constraints : ConstraintList.t ;
} ;;

let add_constraint (x,j,cstr_for_xj) oar ={
   oar with constraints = ConstraintList.add (x,j,cstr_for_xj) oar.constraints ;
} ;;

let extract_constraint oar j = ConstraintList.at_index oar.constraints j ;;
let extract_solution oar = oar.solution ;;

let of_solution sol = {
  solution = sol ;
  constraints = ConstraintList.empty_one ;
} ;;

end ;;

module Boat = struct 

type t = B of ((int list) * Oar.t) list ;;

let empty_one = B [] ;;

exception Solution_already_present of int list ;; 

let helper_for_solution_adding (x,sol_for_x) boat_content=
   let rec tempf = (fun (above_x,to_be_treated) ->
   match to_be_treated with 
     [] -> let oar_for_x = Oar.of_solution sol_for_x in 
            List.rev ((x,oar_for_x)::above_x) 
     |y_pair :: others ->
        let (y,data_for_y) = y_pair in 
        (match Total_ordering.silex_for_intlists x y with 
         Total_ordering_result_t.Lower->tempf(y_pair::above_x,others)
        |Greater-> 
          let oar_for_x = Oar.of_solution sol_for_x in 
          List.rev_append ((x,oar_for_x)::above_x) to_be_treated
        |Equal -> raise (Solution_already_present x)
        )
   ) in
   tempf([],boat_content) ;;

let add_solution (B l) (x,sol_for_x) = 
   B(helper_for_solution_adding (x,sol_for_x) l) ;; 

exception Solution_not_found of int list ;;

let helper_for_constraint_adding (x,j,cstr_for_xj) boat_content=
      let rec tempf = (fun (above_x,to_be_treated) ->
      match to_be_treated with 
        [] -> raise(Solution_not_found x)
        |y_pair :: others -> 
           let (y,data_for_y) = y_pair in 
           (match Total_ordering.silex_for_intlists x y with 
            Total_ordering_result_t.Lower->tempf(y_pair::above_x,others)
           |Greater-> raise(Solution_not_found x)
           |Equal -> 
              let new_data_for_y = Oar.add_constraint (x,j,cstr_for_xj) data_for_y in 
              List.rev_append above_x ((y,new_data_for_y)::others)
           )
      ) in
      tempf([],boat_content) ;;
   
let add_constraint (B l) (x,j,constr_for_xj) = 
      B(helper_for_constraint_adding (x,j,constr_for_xj) l) ;;   

let get_opt (B l) x = List.assoc_opt x l ;;

end ;;  


module ThisBoat = struct 

let main_ref = ref Boat.empty_one ;; 

let add_constraint cstr = (main_ref:= Boat.add_constraint (!main_ref) cstr) ;;
let add_solution sol = (main_ref:= Boat.add_solution (!main_ref) sol) ;;
let get_opt x = Boat.get_opt (!main_ref) x ;;

end ;;

exception Missing_constraints of int list * int list;; 


let induction_step_in_partial_case old_f (x,treated) = (
  if List.length(x)<3 
  then Oar.of_solution(x@treated) 
  else
  let (n,ry) = Listennou.ht(List.rev x) in 
  let y = List.rev ry in 
  if is_not_admissible(n::treated)
  then old_f(false,y,treated)  
  else 
  let answer_for_y = old_f(true,y,[]) in 
  let soly = Oar.extract_solution answer_for_y in 
  let my = List.length soly in 
  match Oar.extract_constraint answer_for_y my with 
   None -> Oar.of_solution(soly @ [n]) 
  |Some (constraints) ->
    if List.for_all (fun z->is_not_admissible(z@[n])) constraints
    then Oar.of_solution(soly)
    else Oar.of_solution(soly@[n]) 
  ) ;;    

let induction_in_full_case old_f x =
    match ThisBoat.get_opt x with 
       Some(old_answer) -> old_answer 
     | None -> 
    let sol_for_x = (
      if List.length(x)<3 
      then x  
      else Oar.extract_solution(old_f(false,x,[]))) in 
    if is_not_admissible sol_for_x 
    then raise (Missing_constraints (x,sol_for_x))
    else   
    let _ = ThisBoat.add_solution (x,sol_for_x) in 
    Oar.of_solution sol_for_x ;;

let rec helper_for_solving (is_full,x,treated) =
   if is_full 
   then induction_in_full_case         helper_for_solving x 
   else induction_step_in_partial_case helper_for_solving (x,treated)  ;; 

let solve x = helper_for_solving (true,x,[]) ;;   

let ff n = solve (Ennig.ennig 1 n) ;;

ff 1 ;;
ff 2 ;;
ThisBoat.add_constraint ([1;2],2,[[1;2]]) ;;
ff 3 ;;
ff 4 ;;
ff 5 ;;
ThisBoat.add_constraint (Ennig.ennig 1 5,4,[[1;2;4;5]]) ;;
ff 6 ;;

let z1 = set_of_minimal_carriers [[5;6];[3;5];[1;4]] (sl 8 4);;
let z1 = set_of_minimal_carriers_with_extra [[5;6];[3;5];[1;4]] (sl 8 4);;

(************************************************************************************************************************
Snippet 61 : Musings on the 1234 problem, chapter II
************************************************************************************************************************)
let original_seed =
   [2; 3; 5; 7; 11; 13; 14; 15; 17; 19; 23; 27; 29; 31; 35; 37; 38] ;;
   
   let seed =
   [2; 3; 5; 6; 7; 10; 11; 13; 14; 15; 17; 19; 22; 23; 26; 27; 29; 30; 31; 33;
      34; 35; 37; 38] ;;
   
   let seed = 
     [2; 3; 5; 6; 7; 9; 10; 11; 13; 14; 15; 17; 19; 21; 22; 23; 25; 26; 27; 29;
      30; 31; 33; 34; 35; 37; 38] ;;
   
   let seed =    
   [2; 3; 5; 6; 7; 9; 10; 11; 13; 14; 15; 17; 18; 19; 21; 22; 23; 25; 26; 27;
      29; 30; 31; 33; 34; 35; 37; 38] ;;
   
   let oi = Total_ordering.for_integers ;;
   let oi2 = Total_ordering.product oi oi ;; 
   
   let reorder pairs =
      let temp1 = Listennou.partition_according_to_fst pairs in 
      let temp2 = Image.image (fun (x,ll)->
          (x,Ordered.safe_set oi2 (List.flatten ll))
         ) temp1 in 
      let temp3 = Ordered.sort Total_ordering.for_integers (Image.image fst temp2) in 
      Image.image (fun x->(x,List.assoc x temp2)) temp3 ;;
   
   let new_elts l=
     let temp1 = Uple.inclusive_list_of_pairs l in 
     let temp2 = Option.filter_and_unpack (
       fun (a,b)->
         if not(Ordered.mem oi (a+b) l) then None else
         let c= a*b  in 
         if Ordered.mem oi c l then None else Some(c,[a,b])
     ) temp1 in 
     reorder temp2 ;;
   
   let generic_push (old_state,old_explanations)=
      let temp1 = new_elts old_state in 
      let temp2 = Image.image fst temp1 in 
      ((Ordered.merge oi old_state temp2,old_explanations@temp1),temp1) ;;
   
   let walker = ref (seed,[]) ;;
   
   let push () = 
      let (new_state,explanations) = generic_push (!walker) in 
      let _ = (walker := new_state) in 
      explanations ;;
   
   let z1 = push () ;;
   let level2 = Image.image fst z1 ;;
   let z2 = List.filter (fun x->x<=38) level2 ;;
   let new_seed = Ordered.merge oi seed z2 ;;
   
   
   

(************************************************************************************************************************
Snippet 60 : Musings on the 1234 problem
************************************************************************************************************************)
open Needed_values ;;

type sensitive_t = {
   unsorted : (int * (int * int) list) list;
   sorted : int list ;
}  ;;  

type increase_t = 
    Inertia of int 
   |Forced of int * ((int*int) list)
   |Redundancy of int * ((int*int) list) 
   |Pass of int * (int list);;


let oi = Total_ordering.for_integers ;;
let oi2 = Total_ordering.product oi oi ;; 

let reorder pairs =
   let temp1 = Listennou.partition_according_to_fst pairs in 
   let temp2 = Image.image (fun (x,ll)->
       (x,Ordered.safe_set oi2 (List.flatten ll))
      ) temp1 in 
   let temp3 = Ordered.sort Total_ordering.for_integers (Image.image fst temp2) in 
   Image.image (fun x->(x,List.assoc x temp2)) temp3 ;;

exception Sore_wound of int list * int * sensitive_t;;

let special_obstructions =
   [
     [2;3;6;8;9]; (* because of [3; 6; 9; 12=2*6] *)
   ]
   @
   (Image.image (fun j->
      [2;j;2*j;2*j+2;3*j] (* because of [j; 2*j; 3*j; 4*j=2*2*j] *)
      
      ) [ 7;11;15]) ;;

let find_initial_obstruction_opt sorted_l =
     let a =List.hd sorted_l and b = List.hd(List.rev sorted_l) in 
     match Option.seek (fun j->Ordered.is_included_in oi [j;2*j;3*j;4*j] sorted_l) 
        (Ennig.ennig a (b/4)) with 
     None -> Option.seek (fun obstr-> Ordered.is_included_in oi obstr sorted_l) special_obstructions
     |Some(j) -> Some [j;2*j;3*j;4*j];;   



module Sensitive = struct 
 
   let check_before_adding x stv = 
      let new_sorted = Ordered.insert oi x stv.sorted in   
      match  find_initial_obstruction_opt new_sorted with 
      None -> (None,Some new_sorted) 
      |Some obstr -> (Some obstr,None) ;;

   let add (x,data_for_x) stv = 
     let (opt_bad,opt_good) = check_before_adding x stv in 
     match opt_bad with 
     Some obstr ->  raise( Sore_wound(obstr,x,stv))
      |None ->
     let new_sorted = Option.unpack opt_good in   
   {
      unsorted = (x,data_for_x) :: stv.unsorted;
      sorted = new_sorted ;
   }   ;;

   let coming_from_last_element stv last_elt =
       let temp1 = Option.filter_and_unpack (fun (x,_)-> 
         let y= last_elt -x in 
         if (x>1)&&(x<=y)&&(List.exists (fun (z,_)->z=y) stv.unsorted) 
         then Some(x*y,[x,y])
         else None   
         ) stv.unsorted in 
         reorder temp1 ;; 
    
   let default_increase stv forbidden_indices =
         let part = Ordered.merge oi stv.sorted (Image.image fst forbidden_indices) in 
         let max_val = (if part=[] then 1 else 1+(List.hd(List.rev part))) in 
         let whole = Ennig.ennig 1 max_val in  
         let possibilities = Ordered.setminus oi whole part in 
         List.hd possibilities ;;   
   
   let empty_one = {unsorted =[] ; sorted =[]} ;;      

   let has_index idx stv= Ordered.mem oi idx stv.sorted ;;

   let incorporate_redundancy (idx,new_decompositions) stv = 
      let new_unsorted = (Image.image (
                  fun old_pair ->
                     let (z,ll) = old_pair in 
                     if z = idx 
                     then (z,ll@new_decompositions)
                     else old_pair   
               ) stv.unsorted ) in 
      {stv with unsorted = new_unsorted ;};;


end ;;   


module Increase = struct 

   type t = increase_t ;;

   

   let string_of_int_pair (x,y)= "("^(string_of_int x)^","^(string_of_int y)^")";;
   let string_of_int_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int l)) ^ "]" ;;
   let string_of_ipair_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int_pair l)) ^ "]" ;;
   
   let message = function 
      Inertia (new_val) -> (string_of_int new_val)^" added by inertia"
      |Forced(new_val,l) -> (string_of_int new_val)^" forced by "^(string_of_ipair_list l)
      |Redundancy(new_val,l) -> "Redundancy : "^(string_of_int new_val)^", with "^(string_of_ipair_list l) 
      |Pass(new_val,l) -> (string_of_int new_val)^" refused because of "^(string_of_int_list l);;
   
   let next (treated,forbidden_indices,to_be_treated)=
      match to_be_treated with 
      [] -> let new_val = Sensitive.default_increase treated forbidden_indices in 
            let (opt_bad,opt_good) = Sensitive.check_before_adding new_val treated in 
            (match opt_bad with 
            Some obstr -> (Pass(new_val,obstr),(treated,forbidden_indices@[new_val,obstr],[])) 
            |None ->   
             (Inertia(new_val),(Sensitive.add (new_val,[]) treated,forbidden_indices,
                     Sensitive.coming_from_last_element treated new_val))) 
      |(new_x,ll) :: others ->
          if Sensitive.has_index new_x treated 
          then let treated2 = Sensitive.incorporate_redundancy (new_x,ll)  treated in 
               (Redundancy(new_x,ll),(treated2,forbidden_indices,others))
          else let addenda = Sensitive.coming_from_last_element treated new_x in 
               (Forced(new_x,ll),(Sensitive.add (new_x,ll) treated,
               forbidden_indices,reorder(others@addenda)));;            

end ;;   

   

let walker = ref (Sensitive.empty_one,[],[]) ;; 

let push () =
    let (incr,next_state) = Increase.next (!walker) in 
    let msg = (Increase.message incr)^"\n" in 
    let _ = (print_string msg;flush stdout) in 
    let _ = (walker:=next_state) in 
    next_state ;;

for j= 1 to 1000 do let _ = push () in () done ;;

let (a,b,c) = (!walker) ;;

let d = List.rev (Option.filter_and_unpack (fun (x,l)->
    if l=[] then Some x else None) a.unsorted);;


(*
walker:=([],[]) ;;
for j= 1 to 10 do let _ = push () in () done ;;
let z1 = fst (!walker) ;;
let z2 = Option.filter_and_unpack 
(fun (x,ll)->if ll=[] then Some x else None) z1;;
let z3 = Ordered.safe_set oi z2 ;;
let z4 = Basic.delta_list z3 ;;
let meas n = let q = (n/3) and r=(n mod 3) in 2*q+r ;;
let z5 = Image.image meas z3 ;;
let z6 = Basic.delta_list z5 ;;

walker:=([],[]) ;;
*)


(************************************************************************************************************************
Snippet 59 : Removes unnecessary blanks at the beginning of lines in an interval
************************************************************************************************************************)
open Needed_values ;;


let ap = Absolute_path.of_string "Filewatching/fw_with_githubbing.ml";;
let old_text = Io.read_whole_file ap ;;

let part1= Lines_in_string.interval old_text 1 171 ;;
let part2= Lines_in_string.interval old_text 172 254;;

let lines1 = Lines_in_string.lines part2 ;;
let lines2 = Image.image (Cull_string.cobeginning 5) lines1 ;;
let new_part2 = String.concat "\n" lines2 ;;
let new_text = part1 ^ "\n" ^ new_part2 ;;

Io.overwrite_with ap new_text ;;

(************************************************************************************************************************
Snippet 58 : Find all modules whose ml file contains a certain substring
************************************************************************************************************************)
open Needed_values ;;

let z1 = Fw_with_githubbing.all_mlx_files (!ucs) ;;
let z2 = List.filter (fun mlx -> (Dfn_full.to_ending mlx)= Dfa_ending.ml ) z1 ;;
let z3 = Explicit.filter (
   fun mlx -> 
    let ap = Dfn_full.to_absolute_path mlx in 
    let text = Io.read_whole_file ap in 
    Substring.is_a_substring_of "Automatic" text
) z2 ;;

(************************************************************************************************************************
Snippet 57 : Musings on the Szemeredi problem, chapter IV
************************************************************************************************************************)
open Needed_values ;;


let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;


let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;

let tag1 = Ennig.doyle (fun x->[x;2*x-1]) 2 (1+current_width) ;;
let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;

let hashtbl_for_main = Hashtbl.create 100 ;;

let main_in_easy_case (n,avoided_elts) =
   let temp1 = Sz_preliminaries.restricted_power_set (max_width,Ennig.ennig 1 n) in 
   let temp2 = List.filter (
     fun y->i_does_not_intersect avoided_elts y
   ) temp1 in 
   Max.maximize_it_with_care List.length temp2 ;;

let translate1 t = Image.image (fun x->x+t) ;;
let translate2 t = Image.image (translate1 t) ;;

let first_break_without_1 avoided_elts =
   [(false,Option.filter_and_unpack 
    (fun x->if x > 1 then Some(x-1) else None) avoided_elts)];;
let first_break_with_1 avoided_elts = 
   if Listennou.extends avoided_elts [1] then None else 
   Some(Image.image ( fun x->
      (true,translate1 (-1) (i_merge avoided_elts x))
    ) tag2) ;;  

let first_break avoided_elts = 
   let part1 = first_break_without_1 avoided_elts in 
   match first_break_with_1 avoided_elts with 
   None -> part1 
   |Some(part2) -> part1 @ part2 ;;

let main_pusher old_f (n,avoided_elts) =
   if n<=15 
   then main_in_easy_case (n,avoided_elts) 
   else 
   let cases = first_break avoided_elts in 
   let temp1 = Image.image (
     fun (head_needed,new_avoided_elts) ->
        let (m2,sols2) = old_f(n-1,new_avoided_elts) in 
        let sols3 = translate2 1 sols2 in 
        if head_needed 
        then (m2+1,Image.image (fun x->1::x) sols3) 
        else (m2,sols3)   
   ) cases in 
   let (final_m,temp2) = Max.maximize_it_with_care fst temp1 in 
   (final_m,il_fold_merge (Image.image snd temp2)) ;;

exception Impatient_exn of int * (int list) ;;

let impatient_main (n,avoided_elts) =
   match Hashtbl.find_opt hashtbl_for_main (n,avoided_elts) with 
   Some res -> res 
   | None -> raise (Impatient_exn(n,avoided_elts)) ;; 


let main pair =
  match Hashtbl.find_opt hashtbl_for_main pair with 
   Some old_answer -> old_answer 
  | None -> 
    let answer = main_pusher impatient_main pair in 
    let _ = (Hashtbl.add hashtbl_for_main pair answer) in 
    answer ;; 

let sons avoided_elts = il_sort (Image.image snd (first_break avoided_elts));;

let iterator (already_treated,to_be_treated) =
   let temp1 = il_fold_merge (Image.image sons to_be_treated) in 
   let new_ones = List.filter (
     fun x->not(il_mem  x already_treated)
   ) temp1 in 
   (il_merge already_treated new_ones,new_ones) ;;

let rec computer pair =
   if snd pair = [] then fst pair else 
   computer(iterator pair) ;; 

let all_helpers = computer ([],[[]]) ;;   

let linear_main n = Image.image (fun y->main (n,y)) all_helpers ;;

let lm n = 
   let _ = linear_main n in 
   let (m,sols)=main (n,[]) in 
    (m,List.hd sols) ;;

let computation = Image.image (fun x->(x,lm x)) (Ennig.ennig 15 50);;


let check = List.filter (fun (n,(m,_))->m <> 
  Sz_precomputed.measure (Sz_max_width_t.MW current_width) n) computation;;

let easy_selector = Memoized.make(fun (n,k) ->
List.filter (fun x->List.length(x)=k) (Sz_preliminaries.restricted_power_set 
(Sz_max_width_t.MW current_width,Ennig.ennig 1 n))
) ;;  


let original_minimal_carriers carriers sols =
 let indexed_carriers = Ennig.index_everything carriers in 
 let shadow = (
     fun sol ->
        Option.filter_and_unpack (
         fun (idx,carrier) -> 
            if i_is_included_in carrier sol 
            then Some idx 
           else None 
       ) indexed_carriers 
 )  in     
 let all_shadows = Image.image shadow sols in 
 Ordered_misc.minimal_transversals all_shadows ;;

exception Nonunique_set_of_minimal_carriers ;;

let set_of_minimal_carriers carriers sols =
let version1 = original_minimal_carriers carriers sols in 
let m = List.length(List.hd version1) in 
let version2 = List.filter (fun x->List.length(x)=m) version1 in 
if (List.length version2)<>1
then raise Nonunique_set_of_minimal_carriers 
else 
Image.image (fun idx->List.nth carriers (idx-1)) (List.hd version2)
;;

let set_of_minimal_carriers_with_extra carriers sols =
try (Some(set_of_minimal_carriers carriers sols),None) with 
_ -> (None, Some carriers)   ;;

let original_carriers n = 
let temp = Ennig.doyle (fun y->let x=current_width+1-y in [n-2*x;n-x]) 1 current_width in 
List.filter (fun l->List.hd(l)>0) temp;;  

let new_carriers_in_hard_case n carriers = 
let (temp1,temp2) = List.partition (fun l->List.mem n l) carriers in 
let temp3 = Image.image (i_outsert n) temp1 in 
let whole1 = temp2@temp3@(original_carriers n) in 
let whole2 = Ordered_misc.minimal_elts_wrt_inclusion whole1 in 
il_sort whole2 ;;

let easy_case (n,k,carriers) = 
(n-1,k,set_of_minimal_carriers carriers (easy_selector (n-1,k)));;

let hard_case (n,k,carriers) = 
let new_carriers = new_carriers_in_hard_case n carriers in 
(n-1,k-1,set_of_minimal_carriers new_carriers (easy_selector (n-1,k-1)));;  

let milton_product carriers1 carriers2 =
let temp1 = Cartesian.product carriers1 carriers2 in 
let temp2 = Image.image (fun (x,y)->i_merge x y) temp1 in 
let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
il_sort temp3 ;;

let fold_milton = function 
[] -> []
| a :: b -> List.fold_left milton_product a b ;;

let meas = Sz_precomputed.measure max_width ;;  
let sample_size = 15 ;;
let current_a = 7 ;;
let base1 = Ennig.ennig 3 (meas current_a) ;;
let base2 = Cartesian.product base1 (Ennig.ennig 1 sample_size) ;;
let base3 = List.flatten(Image.image (
fun (sa,b) -> 
   Ennig.doyle (fun sb->(sa,b,sb)) (meas(current_a+b)-sa+1) (meas b)
) base2);;
let base4 = List.flatten(Image.image (
fun (sa,b,sb) -> 
   Option.filter_and_unpack (
      fun zb -> 
        if List.length(zb) = sb 
        then Some(sa,Image.image (fun t->current_a+t) zb) 
        else None
   ) (Sz_preliminaries.restricted_power_set(max_width,Ennig.ennig 1 b))
) base3);;

let current_sa = 4 ;;
let current_left_component =
 List.filter (fun z->List.length(z)=current_sa) 
 (Sz_preliminaries.restricted_power_set(max_width,Ennig.ennig 1 current_a)) ;; 
let base5 = Option.filter_and_unpack (
fun (sa,zb) -> if sa = current_sa then Some zb else None
) base4 ;;
let base6 = Ordered_misc.minimal_elts_wrt_inclusion(il_sort base5) ;;
let base7 = Image.image (fun z->
let temp1 = Sz_preliminaries.force_subset_in_interval max_width z (1,List.hd(List.rev z)) in 
List.filter (fun l->Max.list(l)<=current_a) temp1
) base6 ;;
let base8 = Image.image (fun l->(l,
set_of_minimal_carriers_with_extra l current_left_component) 
) base7 ;;
let (first_half,other_half) = List.partition (fun (l,(opt1,opt2))->opt2=None) base8 ;;
let base9 = Image.image (fun (l,(opt1,opt2))->Option.unpack opt1) first_half ;;
let base10 = Image.image (fun (l,(opt1,opt2))->Option.unpack opt2) other_half ;;
let res1 = fold_milton base9 ;;

(************************************************************************************************************************
Snippet 56 : Musings on the Szemeredi problem 
************************************************************************************************************************)
open Needed_values ;;
module L3 = struct

   let current_width = 3 ;; 
   
   let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
   let i_merge = Ordered.merge Total_ordering.for_integers ;;
   let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
   let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
   let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
   let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;
   
   let tag1 = Ennig.doyle (fun x->[x;2*x-1]) 2 (1+current_width) ;;
   let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;
   
   let hashtbl_for_main = Hashtbl.create 100 ;;
   
   let main_in_easy_case (n,avoided_elts) =
      let temp1 = Sz_preliminaries.restricted_power_set 
          (Sz_max_width_t.MW current_width,Ennig.ennig 1 n) in 
      let temp2 = List.filter (
        fun y->i_does_not_intersect avoided_elts y
      ) temp1 in 
      Max.maximize_it_with_care List.length temp2 ;;
   
   let translate1 t = Image.image (fun x->x+t) ;;
   let translate2 t = Image.image (translate1 t) ;;
   
   let first_break_without_1 avoided_elts =
      [(false,Option.filter_and_unpack 
       (fun x->if x > 1 then Some(x-1) else None) avoided_elts)];;
   let first_break_with_1 avoided_elts = 
      if Listennou.extends avoided_elts [1] then None else 
      Some(Image.image ( fun x->
         (true,translate1 (-1) (i_merge avoided_elts x))
       ) tag2) ;;  
   
   let first_break avoided_elts = 
      let part1 = first_break_without_1 avoided_elts in 
      match first_break_with_1 avoided_elts with 
      None -> part1 
      |Some(part2) -> part1 @ part2 ;;
   
   let main_pusher old_f (n,avoided_elts) =
      if n<=15 
      then main_in_easy_case (n,avoided_elts) 
      else 
      let cases = first_break avoided_elts in 
      let temp1 = Image.image (
        fun (head_needed,new_avoided_elts) ->
           let (m2,sols2) = old_f(n-1,new_avoided_elts) in 
           let sols3 = translate2 1 sols2 in 
           if head_needed 
           then (m2+1,Image.image (fun x->1::x) sols3) 
           else (m2,sols3)   
      ) cases in 
      let (final_m,temp2) = Max.maximize_it_with_care fst temp1 in 
      (final_m,il_fold_merge (Image.image snd temp2)) ;;
   
   exception Impatient_exn of int * (int list) ;;
   
   let impatient_main (n,avoided_elts) =
      match Hashtbl.find_opt hashtbl_for_main (n,avoided_elts) with 
      Some res -> res 
      | None -> raise (Impatient_exn(n,avoided_elts)) ;; 
   
   
   let main pair =
     match Hashtbl.find_opt hashtbl_for_main pair with 
      Some old_answer -> old_answer 
     | None -> 
       let answer = main_pusher impatient_main pair in 
       let _ = (Hashtbl.add hashtbl_for_main pair answer) in 
       answer ;; 
   
   let sons avoided_elts = il_sort (Image.image snd (first_break avoided_elts));;
   
   let iterator (already_treated,to_be_treated) =
      let temp1 = il_fold_merge (Image.image sons to_be_treated) in 
      let new_ones = List.filter (
        fun x->not(il_mem  x already_treated)
      ) temp1 in 
      (il_merge already_treated new_ones,new_ones) ;;
   
   let rec computer pair =
      if snd pair = [] then fst pair else 
      computer(iterator pair) ;; 
   
   let all_helpers = computer ([],[[]]) ;;   
   
   let linear_main n = Image.image (fun y->main (n,y)) all_helpers ;;
   
   let lm n = 
      let _ = linear_main n in 
      let (m,sols)=main (n,[]) in 
       (m,List.hd sols) ;;
   
   let computation = Image.image (fun x->(x,lm x)) (Ennig.ennig 15 50);;
   
   
   let check = List.filter (fun (n,(m,_))->m <> 
     Sz_precomputed.measure (Sz_max_width_t.MW current_width) n) computation;;
   
   
   end ;;
   
   
   
   module L4 = struct
   
     let current_width = 4 ;; 
     
     let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
     let i_merge = Ordered.merge Total_ordering.for_integers ;;
     let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
     let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
     let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
     let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;
     
     let tag1 = Ennig.doyle (fun x->[x;2*x-1]) 2 (1+current_width) ;;
     let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;
     
     let hashtbl_for_main = Hashtbl.create 100 ;;
     
     let main_in_easy_case (n,avoided_elts) =
        let temp1 = Sz_preliminaries.restricted_power_set 
            (Sz_max_width_t.MW current_width,Ennig.ennig 1 n) in 
        let temp2 = List.filter (
          fun y->i_does_not_intersect avoided_elts y
        ) temp1 in 
        Max.maximize_it_with_care List.length temp2 ;;
     
     let translate1 t = Image.image (fun x->x+t) ;;
     let translate2 t = Image.image (translate1 t) ;;
     
     let first_break_without_1 avoided_elts =
        [(false,Option.filter_and_unpack 
         (fun x->if x > 1 then Some(x-1) else None) avoided_elts)];;
     let first_break_with_1 avoided_elts = 
        if Listennou.extends avoided_elts [1] then None else 
        Some(Image.image ( fun x->
           (true,translate1 (-1) (i_merge avoided_elts x))
         ) tag2) ;;  
     
     let first_break avoided_elts = 
        let part1 = first_break_without_1 avoided_elts in 
        match first_break_with_1 avoided_elts with 
        None -> part1 
        |Some(part2) -> part1 @ part2 ;;
     
     let main_pusher old_f (n,avoided_elts) =
        if n<=15 
        then main_in_easy_case (n,avoided_elts) 
        else 
        let cases = first_break avoided_elts in 
        let temp1 = Image.image (
          fun (head_needed,new_avoided_elts) ->
             let (m2,sols2) = old_f(n-1,new_avoided_elts) in 
             let sols3 = translate2 1 sols2 in 
             if head_needed 
             then (m2+1,Image.image (fun x->1::x) sols3) 
             else (m2,sols3)   
        ) cases in 
        let (final_m,temp2) = Max.maximize_it_with_care fst temp1 in 
        (final_m,il_fold_merge (Image.image snd temp2)) ;;
     
     exception Impatient_exn of int * (int list) ;;
     
     let impatient_main (n,avoided_elts) =
        match Hashtbl.find_opt hashtbl_for_main (n,avoided_elts) with 
        Some res -> res 
        | None -> raise (Impatient_exn(n,avoided_elts)) ;; 
     
     
     let main pair =
       match Hashtbl.find_opt hashtbl_for_main pair with 
        Some old_answer -> old_answer 
       | None -> 
         let answer = main_pusher impatient_main pair in 
         let _ = (Hashtbl.add hashtbl_for_main pair answer) in 
         answer ;; 
     
     let sons avoided_elts = il_sort (Image.image snd (first_break avoided_elts));;
     
     let iterator (already_treated,to_be_treated) =
        let temp1 = il_fold_merge (Image.image sons to_be_treated) in 
        let new_ones = List.filter (
          fun x->not(il_mem  x already_treated)
        ) temp1 in 
        (il_merge already_treated new_ones,new_ones) ;;
     
     let rec computer pair =
        if snd pair = [] then fst pair else 
        computer(iterator pair) ;; 
     
     let all_helpers = computer ([],[[]]) ;;   
     
     let linear_main n = Image.image (fun y->main (n,y)) all_helpers ;;
     
     let lm n = 
        let _ = linear_main n in 
        let (m,sols)=main (n,[]) in 
         (m,List.hd sols) ;;
     
     let computation = Image.image (fun x->(x,lm x)) (Ennig.ennig 15 50);;
     
     let check = List.filter (fun (n,(m,_))->m <> 
       Sz_precomputed.measure (Sz_max_width_t.MW current_width) n) computation;;
     
     end ;;
   
   
   let z1 =  List.filter (fun x->
     let m3 = Sz_precomputed.measure (Sz_max_width_t.MW 3) x 
     and m4 = Sz_precomputed.measure (Sz_max_width_t.MW 4) x in 
     m3<>m4) (Ennig.ennig 1 25);; 
   
   let g1 = L3.main (10,[]) ;;  
   let g2 = L4.main (10,[]) ;; 
   
   let h1 = L3.il_sort (Ordered_misc.minimal_transversals [[1;5;9];[2;6;10]]) ;;
   

(************************************************************************************************************************
Snippet 55 : 
************************************************************************************************************************)
open Needed_values ;;
let mixer (a,b,ll)= Image.image (fun l->a@(Image.image (fun t->t+b) l)) ll;;

let upwards_hat (a,n,b) =  
  let q1 = (n-a)/2 in 
  let central_move = (if (n-a) mod 2 = 0 then -1 else 1) in 
  let new_beginning = (a+2*q1)+central_move in 
  let q2 = (new_beginning-b)/2 in 
  (Ennig.doyle (fun t->a+2*t) 0 q1)@(Ennig.doyle (fun t->new_beginning-2*t) 0 q2) ;;

let downwards_hat (a,n,b) =  
  let q1 = (a-n)/2 in 
  let central_move = (if (a-n) mod 2 = 0 then 1 else -1) in 
  let new_beginning = (a-2*q1)+central_move in 
  let q2 = (b-new_beginning)/2 in 
  (Ennig.doyle (fun t->a-2*t) 0 q1)@(Ennig.doyle (fun t->new_beginning+2*t) 0 q2) ;;

exception Hat_definition_exn of int * int * int ;;

let hat (a,n,b) =
  if (((b-a) mod 2)=0)
    ||((n<a)&&(n>b))||((n>a)&&(n<b))
  then raise(Hat_definition_exn(a,n,b)) else  
  if a<n 
  then upwards_hat (a,n,b)
  else downwards_hat (a,n,b) ;;  

   
let eu_12  old_f n = mixer([1],1,old_f(n-1,1)) ;;
let eu_132 old_f n = if n=3 then [[1;3;2]] else mixer([1;3;2],3,old_f(n-3,1)) ;;
let eu_134 old_f n = if n=4 then [hat(1,4,2)] else [] ;;
let eu_135 old_f n = [hat(1,n,2)] ;;

let eu_21 old_f n = 
  if n=2
  then [[2;1]]
  else mixer([2;1],2,old_f(n-2,1));;

let eu_case1 i1 old_f n = mixer(hat(i1,1,i1-1),i1,old_f(n-i1,1)) ;;
let eu_case4 i1 old_f n = mixer(hat(i1,n,i1+1),0,old_f(i1-1,i1-1)) ;;  

let main_base n =
    (
      [
          [1;2],(fun old_f n_again ->mixer([1],1,old_f(n-1,1))) ;
          [1;3;2],eu_132 ;
          [1;3;4],eu_134 ; 
          [1;3;5],eu_135 ; 
          [2;1],eu_21;
          [2;3],(fun old_f n_again ->if n=3 then [[2;3;1]] else []); 
          [2;4],eu_case4 2; 
        ]  
    )  
    @
    (List.flatten(
      Ennig.doyle (fun x->
        List.filter (fun (l,f)->(List.for_all(fun j->j>0)l)&&(List.hd(List.rev l)<= n)) [
          [x;x-2],eu_case1 x;
          [x;x+2],eu_case4 x; 
        ]
        ) 3 (n-2)
    ))
   @(
     [
      [n-1;n-3],eu_case1 (n-1);
      [n-1;n],(fun old_f n_again ->mixer([n-1;n],0,old_f(n-2,n-2)) ); 
      [n;n-2],(fun old_f n_again ->mixer([n],0,old_f(n-1,n-2)));
      [n;n-1],(fun old_f n_again ->mixer([n],0,old_f(n-1,n-1)));
     ]
   ) ;;

let small_values = [
   (1,1),[[1]];
   (2,1),[[1;2]];
   (2,2),[[2;1]];
   (3,1),[[1;2;3];[1;3;2]];
   (3,2),[[2;1;3];[2;3;1]];
   (3,3),[[3;1;2];[3;2;1]];
   (4,1),[[1;2;3;4];[1;2;4;3];[1;3;2;4];[1;3;4;2]];
   (4,2),[[2;1;3;4];[2;4;3;1]];
   (4,3),[[3;1;2;4];[3;4;2;1]];
   (4,4),[[4;2;1;3];[4;2;3;1];[4;3;1;2];[4;3;2;1]];
]   ;;

exception Main_parameter_exn of int * int ;;

let main = Memoized.recursive (fun old_f (n,i1)->
   match List.assoc_opt (n,i1) small_values with 
   Some(easy_answer) -> easy_answer 
   | None ->
     if (n<5)||(i1<1)||(i1>n) then raise(Main_parameter_exn(n,i1)) else 
     let temp1 = main_base n in 
     let temp2 = Image.image (fun (prefix,f)-> 
        if List.hd(prefix)=i1 
        then f old_f n
        else []  
     ) temp1  in 
     List.flatten temp2 
) ;;

let goal = List.flatten(Ennig.doyle (fun m->(Ennig.doyle (fun j->(m,j)) 1 m)) 1 25);;
exception Haddock of int * int ;;
let computation = Image.image (fun (n,i)-> try main(n,i) with _->raise(Haddock(n,i))) goal ;;

let whole = Memoized.make (fun n->
    List.flatten (Ennig.doyle (fun j->main(n,j)) 1 n)
) ;;

let sizes = 
  let _ = whole 15 in 
  Ennig.doyle (fun n->List.length(whole n)) 1 25;;
let check_sizes = (sizes = [1; 2; 6; 12; 20; 34; 56; 88; 136; 208; 314; 470; 700; 1038; 1534; 2262;
3330; 4896; 7192; 10558; 15492; 22724; 33324; 48860; 71630]) ;;

(************************************************************************************************************************
Snippet 54 : Musing on permutations satisfying |i-j|<1 -> |p(i)-p(j)|<=2, chapter III
************************************************************************************************************************)
open Needed_values ;;
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;

let extensions1 n l = match l with 
    [] -> Ennig.ennig 1 n 
   | a :: others ->
      List.filter (fun x->(x>0)&&(x<=n)&&(not(List.mem x l))) [a-2;a-1;a+1;a+2] ;;

let extensions2 n ll =
  List.flatten (Image.image (fun l->
    let temp1 = extensions1 n l in 
    Image.image (fun a-> a::l) temp1
    ) ll) ;;

let main = Memoized.make(fun n->
    let rec tempf = (fun j->
      if j=0 then [[]] else 
      extensions2 n (tempf (j-1))  
    ) in 
    Image.image List.rev (tempf n)
) ;;    

let selector = Memoized.make(fun (n,beginning)->
  List.filter (
   fun l->Listennou.extends l beginning  
) (main (n+1)));;

let old_sel beg n = List.length (selector (n-1,beg)) ;;

let sel beg = 
    let temp1 = Image.image string_of_int (Ennig.doyle (old_sel beg) 1 18) in 
    let temp2 = String.concat "," temp1 in 
    let temp3 = "\n\n\n["^temp2^"]\n\n\n" in 
    print_string temp3;;

let mixer (a,b,ll)= Image.image (fun l->a@(Image.image (fun t->t+b) l)) ll;;

let upwards_hat (a,n,b) =  
  let q1 = (n-a)/2 in 
  let central_move = (if (n-a) mod 2 = 0 then -1 else 1) in 
  let new_beginning = (a+2*q1)+central_move in 
  let q2 = (new_beginning-b)/2 in 
  (Ennig.doyle (fun t->a+2*t) 0 q1)@(Ennig.doyle (fun t->new_beginning-2*t) 0 q2) ;;

let downwards_hat (a,n,b) =  
  let q1 = (a-n)/2 in 
  let central_move = (if (a-n) mod 2 = 0 then 1 else -1) in 
  let new_beginning = (a-2*q1)+central_move in 
  let q2 = (b-new_beginning)/2 in 
  (Ennig.doyle (fun t->a-2*t) 0 q1)@(Ennig.doyle (fun t->new_beginning+2*t) 0 q2) ;;

exception Hat_definition_exn of int * int * int ;;

let hat (a,n,b) =
  if (((b-a) mod 2)=0)
    ||((n<a)&&(n>b))||((n>a)&&(n<b))
  then raise(Hat_definition_exn(a,n,b)) else  
  if a<n 
  then upwards_hat (a,n,b)
  else downwards_hat (a,n,b) ;;  

   
let eu_12  old_f n = mixer([1],1,old_f(n-1,1)) ;;
let eu_132 old_f n = if n=3 then [[1;3;2]] else mixer([1;3;2],3,old_f(n-3,1)) ;;
let eu_134 old_f n = if n=4 then [hat(1,4,2)] else [] ;;
let eu_135 old_f n = [hat(1,n,2)] ;;

let eu_21 old_f n = 
  if n=2
  then [[2;1]]
  else mixer([2;1],2,old_f(n-2,1));;

let eu_case1 i1 old_f n = 
  if n=i1 
  then mixer([i1],0,old_f(i1-1,i1-2))
  else mixer(hat(i1,1,i1-1),i1,old_f(n-i1,1)) ;;
let eu_case2 i1 old_f n = 
  if n=i1 
  then mixer([i1],0,old_f(i1-1,i1-1))
  else [] ;;
let eu_case3 i1 old_f n = 
  if n=(i1+1) 
  then mixer([i1;(i1+1)],0,old_f(i1-1,i1-1))
  else [] ;;
let eu_case4 i1 old_f n = mixer(hat(i1,n,i1+1),0,old_f(i1-1,i1-1)) ;;  

let main_base n =
    (
      [
          [1;2],eu_12 ;
          [1;3;2],eu_132 ;
          [1;3;4],eu_134 ; 
          [1;3;5],eu_135 ; 
          [2;1],eu_21;
          [2;3],eu_case3 2; 
          [2;4],eu_case4 2; 
        ]  
    )  
    @
    (List.flatten(
      Ennig.doyle (fun x->
        List.filter (fun (l,f)->(List.for_all(fun j->j>0)l)&&(List.hd(List.rev l)<= n)) [
          [x;x-2],eu_case1 x;
          [x;x-1],eu_case2 x;
          [x;x+1],eu_case3 x; 
          [x;x+2],eu_case4 x; 
        ]
        ) 3 n
    )) ;;

let small_values = [
   (1,1),[[1]];
   (2,1),[[1;2]];
   (2,2),[[2;1]];
   (3,1),[[1;2;3];[1;3;2]];
   (3,2),[[2;1;3];[2;3;1]];
   (3,3),[[3;1;2];[3;2;1]];
   (4,1),[[1;2;3;4];[1;2;4;3];[1;3;2;4];[1;3;4;2]];
   (4,2),[[2;1;3;4];[2;4;3;1]];
   (4,3),[[3;1;2;4];[3;4;2;1]];
   (4,4),[[4;2;1;3];[4;2;3;1];[4;3;1;2];[4;3;2;1]];
]   ;;

exception Main_parameter_exn of int * int ;;

let main = Memoized.recursive (fun old_f (n,i1)->
   match List.assoc_opt (n,i1) small_values with 
   Some(easy_answer) -> easy_answer 
   | None ->
     if (n<5)||(i1<1)||(i1>n) then raise(Main_parameter_exn(n,i1)) else 
     let temp1 = main_base n in 
     let temp2 = Image.image (fun (prefix,f)-> 
        if List.hd(prefix)=i1 
        then f old_f n
        else []  
     ) temp1  in 
     List.flatten temp2 
) ;;

let support = List.flatten (Ennig.doyle (fun m->Ennig.doyle(fun j->(m,j)) 1 m) 1 18);;
let check = List.filter (
  fun (n,i1) -> (main (n,i1)) <> selector (n-1,[i1])
) support ;;


let dbg1= main_base 5 ;;
let dbg2 = List.filter (fun (l,f) -> List.hd l = 2) dbg1 ;;
let dbg3 = Image.image (
  fun (l,f)-> f main 5
) dbg2 ;;

eu_case2 3 main 5 ;;


(************************************************************************************************************************
Snippet 53 : Musing on permutations satisfying |i-j|<1 -> |p(i)-p(j)|<=2 
************************************************************************************************************************)
open Needed_values ;;
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;

let extensions1 n l = match l with 
    [] -> Ennig.ennig 1 n 
   | a :: others ->
      List.filter (fun x->(x>0)&&(x<=n)&&(not(List.mem x l))) [a-2;a-1;a+1;a+2] ;;

let extensions2 n ll =
  List.flatten (Image.image (fun l->
    let temp1 = extensions1 n l in 
    Image.image (fun a-> a::l) temp1
    ) ll) ;;

let main = Memoized.make(fun n->
    let rec tempf = (fun j->
      if j=0 then [[]] else 
      extensions2 n (tempf (j-1))  
    ) in 
    Image.image List.rev (tempf n)
) ;;    

let aa = Memoized.make(fun n->
  List.filter (
   fun l->List.hd(l) = 1
  ) (main (n+1))
) ;;

let uu = Memoized.make(fun n->
    List.filter (
     fun l->let rl = List.rev l in
     Listennou.extends rl [n;n-1]
    ) (main n)
) ;;

let vv = Memoized.make(fun n->
  List.filter (
   fun l->let rl = List.rev l in
   Listennou.extends rl [n-1;n]
  ) (main n)
) ;;

let ww = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)=(n-1))&&(gl (n-1)=(n-2))&&(gl n=n)
  ) (main n)
) ;;

let tt = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)<>(n-1))&&(gl n=(n-2))
  ) (main n)
) ;;

let ss = Memoized.make(fun n->
  List.filter (
   fun l->
    let i = Listennou.find_index (n-1) l in 
    if (List.mem i [1;n-1;n])
    then false 
    else (List.nth l i)=n   
  ) (main n)
) ;;

let s_to_w =Memoized.make(fun n -> Ordered.fold_merge ointlist 
  [ss n;tt n;uu n;vv n;ww n] );;

let reversed_s_to_w =Memoized.make(fun n ->
    Ordered.safe_set ointlist (Image.image List.rev (s_to_w n)));;

let double_s_to_w =Memoized.make(fun n ->
      Ordered.intersect 
      ointlist  (s_to_w n) (reversed_s_to_w n) );;
  

let na n = List.length(aa n);;
let nk n = List.length(main n);;
let ns n = List.length(ss n);;
let nt n = List.length(tt n);;
let nu n = List.length(uu n);;
let nv n = List.length(vv n);;
let nw n = List.length(ww n);;

let zz n=(na n,nu n,nv n,nw n,ns n,nt n,nk n) ;;

let uv n = (uu n,vv(n+1)) ;;

let da n = (na (n+3))-(na(n+2)+na(n)+1) ;; 
let ka n = (nk(n)) -(2*na(n-1)) ;;

let selector = Memoized.make(fun (n,t)->
  List.filter (
   fun l->List.hd(l) = t
  ) (main (n+1))
) ;;

let aa = Memoized.make (fun n->selector(n,1)) ;;
let bb = Memoized.make (fun n->selector(n,2)) ;;
let cc = Memoized.make (fun n->selector(n,3)) ;;
let dd = Memoized.make (fun n->selector(n,4)) ;;
let ee = Memoized.make (fun n->selector(n,5)) ;;

let peggy n = Ennig.doyle (fun j->List.length(selector(n,j))) 1 n ;;


let na n = List.length(aa n);;
let nb n = List.length(bb n);;
let nc n = List.length(cc n);;
let nd n = List.length(dd n);;
let ne n = List.length(ee n);;

let zz n=(na n,nb n,nc n,nd n,nk n,ne n);;




(************************************************************************************************************************
Snippet 52 : Rename scanned files
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string (home^"/Downloads/Building_site/");;
let s_ap1 = Absolute_path.to_string ap1 ;;
let u2 = More_unix.beheaded_simple_ls (Directory_name.of_string s_ap1) ;;
let u3 = Image.image (fun s->(int_of_string(Cull_string.interval s 8 14),s)) u2 ;;
let (Set_of_poly_pairs_t.S u4) = Set_of_poly_pairs.safe_set u3 ;;
let u5 = Ennig.index_everything (Image.image fst u4) ;;
let u6 = Image.image (
  fun (k,old_k)->
    let sk = string_of_int k 
     and sok = string_of_int old_k in 
  "mv "^s_ap1^"korres\\ "^sok^".jpg "^s_ap1^"p"^sk^".jpg"
) u5 ;; 
let u7 = Image.image Sys.command u6 ;;

Coherent_pdf.workspace_directory := s_ap1 ;;
Coherent_pdf.implode ("p","") ;;

(************************************************************************************************************************
Snippet 51 : Write repetitive code for PARI-GP
************************************************************************************************************************)
let s_ap = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp" ;;

let ap = Absolute_path.of_string s_ap ;;

let gtext t1 t2 = 
  String.concat "\n"
  ["c_fa="^(string_of_int t1); 
   "c_t1="^(string_of_int t2); 
   "c_for_t2=big_subst(for_t2,[t1,fa],[c_t1,c_fa])";
 "interm=subst(part3_for_t3,t1,c_t1)";
 "c_for_t3=factor(polresultant(interm,c_for_t2,t2))[2,1]";
 "around_t2=make_zero(special_euclid(interm,c_for_t2,t2,c_for_t3,t3),t2)";
 "listput(accu,[[c_fa,c_t1],around_t2])";
 "printf(Str(c_fa,\",\"c_t1,\" done\\n\"))"] ;;

let u1 = Cartesian.product (Ennig.ennig 11 50) (Ennig.ennig 101 140) ;;
let u2 = Image.image (fun (t1,t2)->gtext t1 t2) u1 ;; 
let u3 = String.concat "\n\n\n" u2 ;;

Io.Private.append_string_to_file u3 ap;;

(*

let z1 = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn1.gp" ;;

let z2 = Lines_in_string.interval (rf z1) 60 67 ;;

*)

(************************************************************************************************************************
Snippet 50 : A useful shortcut using Lines_in_string.remove_interval_in_file 
************************************************************************************************************************)
let ri fn x y =
     Lines_in_string.remove_interval_in_file 
      (Absolute_path.of_string fn) x y ;;

(************************************************************************************************************************
Snippet 49 : Test the prepare_fw_with_dependencies.ml file
************************************************************************************************************************)
let the_other_one = 
   Absolute_path.of_string "../Idaho/Filewatching/fw_with_dependencies.ml" ;;
 
(*  #use "Githubbed_archive/prepare_fw_with_dependencies.ml";; 
   
 write_all_to_file the_other_one ;;  *)

(************************************************************************************************************************
Snippet 48 : Typical use of marked comments
************************************************************************************************************************)
open Needed_values ;;

let src1 = Absolute_path.of_string "../Idaho/Filewatching/file_watcher.ml";;

let dest1 = Absolute_path.of_string "../Idaho/Filewatching/fw_modular.ml";;

let act1 () = Mark_comments_for_copying_or_deletion.copy src1 dest1 ;;

let act2 () = Shorten_long_blank_intervals.in_file dest1 ;;


(************************************************************************************************************************
Snippet 47 : Replacements on several files
************************************************************************************************************************)
open Needed_values ;;

let aps_ref = ref [];;

 aps_ref := [
   Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml";
   Absolute_path.of_string "../Idaho/Compilation_management/modify_coma_state.ml";
   Absolute_path.of_string "../Idaho/Ocaml_analysis/read_needed_ocaml_files.ml"
];;

let rep (x,y) = 
   Image.image (
     fun ap -> Replace_inside.replace_inside_file (x,y) ap
   ) (!aps_ref) ;; 

rep ("subdir_at_module","subdir_for_module")   ;;
rep ("principal_ending_at_module","principal_ending_for_module")   ;;
rep ("mli_presence_at_module","mli_presence_for_module")   ;;
rep ("principal_mt_at_module","principal_mt_for_module")   ;;
rep ("mli_mt_at_module","mli_mt_for_module")   ;;
rep ("direct_fathers_at_module","direct_fathers_for_module")   ;;
rep ("ancestors_at_module","ancestors_for_module")   ;;
rep ("needed_libs_at_module","needed_libs_for_module")   ;;
rep ("needed_dirs_at_module","needed_dirs_for_module")   ;;
rep ("product_up_to_date_at_module","last_compilation_result_for_module")   ;;

let fix () =
   let _ = Sys.command (
      "cp Decomposed_filename/dfa_subdirectory.ml "^
      "../Idaho/Decomposed_filename/afd_sybdirectoru.ml") in
   let _ = Sys.command (
         "mv ../Idaho/Decomposed_filename/afd_sybdirectoru.ml "^
         "../Idaho/Decomposed_filename/dfa_subdirectory.ml") in   
   let _ =  Image.image (fun s->
       let ap= Absolute_path.of_string ("../Idaho/"^s) in 
       Replace_inside.replace_inside_file  
       ("Afd_sybdirectoru.","Dfa_subdirectory.") ap
       ) [
      "Decomposed_filename/dfn_rootless.ml";
      "Decomposed_filename/dfn_endingless.ml";
      "Decomposed_filename/dfn_middle.ml";
      "Decomposed_filename/dfn_full.ml";
      "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
      "Compilation_management/coma_constant.ml";
      "Compilation_management/coma_state.ml";
      "Compilation_management/save_coma_state.ml";
      "Compilation_management/modify_coma_state.ml";
      "Compilation_management/create_world_copy.ml";
      "Filewatching/fw_configuration.ml";
      "Filewatching/file_watcher.ml";
      "Filewatching/fw_with_dependencies.ml";
   ] in    
   ruco () ;;

   let fix () =
      let _ = Sys.command (
         "cp Decomposed_filename/dfa_subdirectory.ml "^
         "../Idaho/Decomposed_filename/afd_sybdirectoru.ml") in
      let _ = Sys.command (
            "mv ../Idaho/Decomposed_filename/afd_sybdirectoru.ml "^
            "../Idaho/Decomposed_filename/dfa_subdirectory.ml") in   
      let _ =  Image.image (fun s->
          let ap= Absolute_path.of_string ("../Idaho/"^s) in 
          Replace_inside.replace_inside_file  
          ("Afd_sybdirectoru.","Dfa_subdirectory.") ap
          ) [
         "Decomposed_filename/dfn_rootless.ml";
         "Decomposed_filename/dfn_endingless.ml";
         "Decomposed_filename/dfn_middle.ml";
         "Decomposed_filename/dfn_full.ml";
         "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
         "Compilation_management/coma_constant.ml";
         "Compilation_management/coma_state.ml";
         "Compilation_management/save_coma_state.ml";
         "Compilation_management/modify_coma_state.ml";
         "Compilation_management/create_world_copy.ml";
         "Filewatching/fw_configuration.ml";
         "Filewatching/file_watcher.ml";
         "Filewatching/fw_with_dependencies.ml";
      ] in    
      ruco () ;;   

      let fix () =
         let _ = Sys.command (
            "cp Padioleau/lexer_ml.ml "^
            "../Idaho/Padioleau/rexel_ml.ml") in
         let _ = Sys.command (
               "mv ../Idaho/Padioleau/rexel_ml.ml "^
               "../Idaho/Padioleau/lexer_ml.ml") in   
         let _ =  Image.image (fun s->
             let ap= Absolute_path.of_string ("../Idaho/"^s) in 
             Replace_inside.replace_inside_file  
             ("Rexel_ml.","Lexer_ml.") ap
             ) [
            "Padioleau/parse_ml.ml";
         ] in    
         ruco () ;;   


(*   
let aps_ref = ref [];;

 aps_ref := Image.image (fun s->Absolute_path.of_string ("../Idaho/"^s)) [
   "Decomposed_filename/dfn_rootless.ml";
   "Decomposed_filename/dfn_endingless.ml";
   "Decomposed_filename/dfn_middle.ml";
   "Decomposed_filename/dfn_full.ml";
   "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
   "Compilation_management/coma_constant.ml";
   "Compilation_management/coma_state.ml";
   "Compilation_management/save_coma_state.ml";
   "Compilation_management/modify_coma_state.ml";
   "Compilation_management/create_world_copy.ml";
   "Filewatching/fw_configuration.ml";
   "Filewatching/file_watcher.ml";
   "Filewatching/fw_with_dependencies.ml";
];;

let rep (x,y) = 
   Image.image (
     fun ap -> Replace_inside.replace_inside_file (x,y) ap
   ) (!aps_ref) ;; 

rep ("Afd_sybdirectoru.","Dfa_subdirectory.")   ;;
*)


(************************************************************************************************************************
Snippet 46 : Extract a line interval from a file and treat it
************************************************************************************************************************)
open Needed_values ;;

let z1 = Image.image (
   fun (methname,_,_,_)->"mod_details \""^methname^"\" ([\n   \"\"\n],\"\") ;;"
) [] ;;
let z2=""::(z1@[""]);;
let z3 = String.concat "\n\n" z2 ;;
let z4 () = print_string z3 ;;

let fn = "Filewatching/fw_with_dependencies.ml";;
let fn = "Fads/jug.ml";;
let z5 = Lines_in_string.interval (rf fn) 9 92 ;;
let z6 = Lines_in_string.lines z5;;
let z7 = Image.image (fun line->"   \""^line^"\";") z6;;
let z8 = "\n\n\n"^(String.concat "\n" z7)^"\n\n\n" ;;
let z9  = print_string z8 ;;

let z6 = Image.image (Cull_string.cobeginning 6) z6;;

let g1 = Image.image (fun s->
   let j = Strung.char_finder_from (fun c->c='/') s 1 in 
   Cull_string.beginning j s  ) z6;;
let g2 = Ordered.sort Total_ordering.lex_for_strings g1 ;;
let g3 = String.concat " " g2;;

(************************************************************************************************************************
Snippet 45 : Remove all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd = Dfa_subdirectory.of_line "Van_der_Waerden";;
let z1 = ae () ;;
let z2 = List.filter (
  fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_subdirectory.begins_with s sd
) z1 ;;
let z3 = Image.image (
   fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_module.to_line m
  ) z2 ;;
let act () = fgs z3 ;;  

(************************************************************************************************************************
Snippet 44 : Replacing a long interval in a file with another
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml" ;;
let ap1_text = Io.read_whole_file ap1 ;;
let to_be_replaced = Lines_in_string.interval ap1_text 1 676 ;;

let towards_complement = rf "Fads/pan.ml";;
let replacement = Lines_in_string.interval towards_complement 9 240 ;;

let act7 () = Replace_inside.replace_inside_file (to_be_replaced,replacement) ap1;;

(************************************************************************************************************************
Snippet 43 : Visualize Git tree
************************************************************************************************************************)
open Needed_values ;;

let gc = "git -C "^home^"/Teuliou/OCaml/Idaho_backup ";;

let cmd_for_z0 = gc ^ "ls-tree -r HEAD > ~/Downloads/temp.txt";;
let z0 = Sys.command cmd_for_z0 ;;

let z1 = rf "~/Downloads/temp.txt";;
let z2 = Lines_in_string.lines z1 ;;
let z3 = List.filter (fun line->
   Supstring.contains line "depth_one"
  ) z2;;
let z4 = Image.image (Cull_string.cobeginning 53) z3;;


let cmds1 = Image.image (fun x->gc^"rm --cached "^x) z4 ;;
let cmds2 = Image.image (fun x->
  let cx = String.capitalize_ascii x in
  gc^"add "^cx) z4 ;;
let cmds3 = cmds1 @ cmds2 ;;
let anse1 = Image.image Sys.command cmds3 ;;

(************************************************************************************************************************
Snippet 42 : Miscellaneous tests on compilation management
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae();;
let u2 = Image.image (fun el->Dfa_module.to_line(Dfn_endingless.to_module el)) u1 ;;
let u3 = Max.maximize_it_with_care (fun mn->
   min(List.length(abo mn))(List.length(bel mn))
  ) u2;;

let test1 = rv "Dfa_subdirectory.connectable_to_subpath" "cannectoble_to_sabputh" ;;
let exit_test1 = rv "Dfa_subdirectory.cannectoble_to_sabputh" "connectable_to_subpath" ;;

let test2= ren "dfa_subdirectory" "afd_sybdirectoru" ;;
let exit_test2= ren "afd_sybdirectoru" "dfa_subdirectory";;

let test3 = rensub "Depth_one_testdir/Depth_two_testdir" "Dopth_twe_tistder" ;;
let exit_test3 = rensub "Depth_one_testdir/Dopth_twe_tistder" "Depth_two_testdir" ;;

let create_ml_file_with_text (fname,text) =
   let s_ap = "Depth_one_testdir/Depth_two_testdir/adhoc/"^fname^".ml" in 
   let ap = Absolute_path.create_file_if_absent s_ap in 
   let _ = Io.overwrite_with ap text in ap ;;

let tf1 = create_ml_file_with_text("tf_one","let a= 2;;") ;;
let tf2 = create_ml_file_with_text("tf_two","let b= 3;;") ;;
let tf3 = create_ml_file_with_text("tf_three","let c= 7;;") ;;
let tf4 = create_ml_file_with_text("tf_four","let d= Tf_three.c+4;;") ;;
let tf5 = create_ml_file_with_text("tf_five","let e= Tf_four.d+5;;") ;;

regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_one.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_two.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_three.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_four.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_five.ml";;


Io.overwrite_with tf3 "let c=Tf_one.a+Tf_two.b;;" ;;

reco "1";;

fgs ["tf_one";"tf_two";"tf_three";"tf_four";"tf_five"] ;;

(************************************************************************************************************************
Snippet 41 : Extracting lines from a file and modifying them
************************************************************************************************************************)
open Needed_values ;;

let w2 = Image.image fst (vfm "Fw_with_dependencies") ;;
let z1 = rf "Filewatching/fw_with_dependencies.ml";;
let z2 = Lines_in_string.interval z1 1304 1351 ;;

let w2 = Image.image fst (vfm "Fw_with_small_details") ;;
let z1 = rf "Filewatching/fw_with_small_details.ml";;
let z2 = Lines_in_string.interval z1 309 334 ;;

let w2 = Image.image fst (vfm "Fw_with_archives") ;;
let z1 = rf "Filewatching/fw_with_archives.ml";;
let z2 = Lines_in_string.interval z1 285 317 ;;

let w2 = Image.image fst (vfm "File_watcher") ;;
let z1 = rf "Filewatching/file_watcher.ml";;
let z2 = Lines_in_string.interval z1 526 549 ;;

let w2 = Image.image fst (vfm "Fw_configuration") ;;

let z3 = Lines_in_string.lines z2 ;;
let z4 = Image.image (
  fun line->
    let j1=(try String.index_from line 1 ' ' with _->0)
    and j2=String.index line '=' in 
    Cull_string.trim_spaces (Cull_string.interval line (j1+1) (j2-1)) 
) z3 ;;
let z5 = Image.image (
  fun line->
       if not(String.contains line ' ')
       then line 
       else let j3 =  String.index line ' ' in 
             Cull_string.beginning j3 line
) z4 ;;
let bad_in_z5=List.filter (fun x->not(List.mem x w2)) z5;;

let z4=[];;
let check_z4 = Ordered.sort Total_ordering.lex_for_strings (Image.image snd z4) ;;

let write (fun_name,lbl)=
 let addendum=(
   if lbl = "constructor" then " dummy_fw" else
   if lbl = "zerovariate_producer" then " dummy_arg" else ""

) in 
 "    let "^fun_name^" = extract_"^lbl^" All_printables."^fun_name^addendum^" ;;" ;;

let z5 = "\n\n\n" ^(String.concat "\n" (Image.image write z4)) ^ "\n\n\n";; 
let z6 () = print_string z5 ;;


let write2 (fun_name,lbl)=
 "let "^fun_name^" = Private.Exit."^fun_name^" ;;" ;;

let z7 = "\n\n\n" ^(String.concat "\n" (Image.image write2 z4)) ^ "\n\n\n";; 
let z8 () = print_string z7 ;;

(************************************************************************************************************************
Snippet 40 : Get a list of value names from an interval of lines in a file
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf "Filewatching/fw_with_dependencies.ml";;
let u2 = Lines_in_string.interval u1 33 48 ;;
let u3 = Lines_in_string.lines u2 ;;
(*
let compute_names = Image.image (
  fun line ->
     let temp1 = Cull_string.two_sided_cutting ("    let ","") line in 
     let j1 = Strung.char_finder_from (fun c->List.mem c [' ';'\r';'\t']) temp1 1 in
     Cull_string.interval temp1 1 (j1-1)
) u3;;
*)


(************************************************************************************************************************
Snippet 39 : Using intervals of line indices to extract values from a module
************************************************************************************************************************)
open Needed_values ;;

let u1 = Needed_values.rf "Compilation_management/coma_state.ml";;
let u2 = Lines_in_string.indexed_lines u1 ;; 

let extract_interval ((i,j),_) =
   let temp1 = List.filter (fun (k,_)->(i<=k) && (k<=j)) u2 in 
   let temp2 = Image.image snd temp1 in 
   String.concat "\n" temp2 ;;

let ref_for_colombo = ref ([
   ((961, 985),   "compute_principal_ending");
   ((1024, 1031), "registrations_for_lonely_ending");
   ((2267, 2350), "Simplified_ts_creation")
]:(((int * int) * string) list)) ;;
let ref_for_curcuma = ref ([
  ((5, 616), "Automatic"); ((634, 634), "needed_libs_at_module");
   ((636, 636), "ancestor_at_module"); ((637, 637), "needed_dirs_at_module");
   ((659, 659), "ordered_list_of_modules"); ((671, 671), "root");
   ((792, 801), "find_needed_data"); ((925, 959), "PrivateTwo");
   ((1034, 1055), "complete_id_during_new_module_registration");
   ((1394, 1466), "register_mlx_file_on_monitored_modules");
   ((1842, 1874), "Try_to_register")
]:(((int * int) * string) list)) ;;
let ref_for_replacements = ref([]:((string * string) list));;

ref_for_replacements:=[
  "=registrations_for_lonely_ending ","=Colombo.registrations_for_lonely_ending ";
  "=md_compute_modification_times ","=Colombo.md_compute_modification_times ";
  "=md_associated_modification_time ","=Colombo.md_associated_modification_time ";
  "= md_compute_modification_time ","= Colombo.md_compute_modification_time ";
  "=compute_principal_ending ","=Colombo.compute_principal_ending ";
  "= ocamldebug_printersfile_path ","= Colombo.ocamldebug_printersfile_path ";
] ;;


type spice = Colombo | Curcuma ;;

let associated_ref = function 
   Colombo -> ref_for_colombo 
  |Curcuma -> ref_for_curcuma ;; 

let spice_to_string = function 
  Colombo -> "Colombo" 
 |Curcuma -> "Curcuma" ;; 

let haddock_order = 
    let oi = Total_ordering.for_integers 
    and prod = Total_ordering.product in 
    prod (prod oi oi) Total_ordering.lex_for_strings ;; 

 let add_interval ((i,j),name) spice=
  let raf = associated_ref spice in 
    (raf:= Ordered.insert haddock_order ((i,j),name)
      (!raf)) ;;

let copy_whole spice=
   let temp1 = Image.image (extract_interval) (!(associated_ref spice)) in
   let whole = String.concat "\n\n" temp1 
   and s = spice_to_string spice in 
   let corrected_whole = Replace_inside.replace_several_inside_string
    (!ref_for_replacements) whole in 
   Replace_inside.overwrite_between_markers_inside_file
   (Overwriter.of_string corrected_whole)
   ("(* Beginning of "^s^" *)\n\n","\n\n(* End of "^s^" *)")
   (Absolute_path.of_string "Fads/pan.ml") ;;

let main ((i,j),name) spice=
   let _= add_interval ((i,j),name) spice in copy_whole spice ;;


(*


*)

(*

main ((961,985),"compute_principal_ending") Colombo;;  
main ((1024,1031),"registrations_for_lonely_ending") Colombo;; 
main ((1655,1658),"ocamldebug_printersfile_path") Colombo;;  
main ((2267,2350),"Simplified_ts_creation") Colombo;;     

main ((5,616),"Automatic") Curcuma;; 
main ((654,654),"set_product_up_to_date_at_module") Curcuma;; 
main ((629,629),"subdir_at_module") Curcuma;; 
main ((630,630),"principal_ending_at_module") Curcuma;; 
main ((631,631),"mli_presence_at_module") Curcuma;; 
main ((634,634),"needed_libs_at_module") Curcuma;; 
main ((636,636),"ancestor_at_module") Curcuma;; 
main ((637,637),"needed_dirs_at_module") Curcuma;; 
main ((649,649),"set_needed_libs") Curcuma;; 
main ((651,651),"set_ancestors_at_module") Curcuma;; 
main ((653,653),"set_needed_dirs") Curcuma;; 
main ((655,656),"set_directories") Curcuma;; 
main ((659,659),"ordered_list_of_modules") Curcuma;; 
main ((660,660),"follows_it") Curcuma;; 
main ((661,661),"all_used_subdirs") Curcuma;; 
main ((671,671),"root") Curcuma;; 
main ((680,685),"endingless_at_module") Curcuma;; 
main ((691,697),"check_ending_in_at_module") Curcuma;; 
main ((720,723),"registered_endings_at_module") Curcuma;; 
main ((781,790),"modules_with_their_ancestors") Curcuma;; 
main ((792,801),"find_needed_data") Curcuma;; 
main ((805,813),"needed_dirs_and_libs_in_command") Curcuma;; 
main ((913,917),"compute_subdirectories_list") Curcuma;; 
main ((919,922),"check_registrations") Curcuma;; 
main ((925,959),"PrivateTwo") Curcuma;; 
main ((987,1007),"complete_info") Curcuma;; 
main ((1034,1055),"complete_id_during_new_module_registration") Curcuma;; 
main ((1378,1389),"printer_equipped_types_from_data") Curcuma;; 
main ((1394,1466),"register_mlx_file_on_monitored_modules") Curcuma;; 
main ((1468,1653),"Modern") Curcuma;; 
main ((1661,1797),"Ocaml_target_making") Curcuma;; 
main ((1842,1874),"Try_to_register") Curcuma;; 

*)

(************************************************************************************************************************
Snippet 38 : Remove all snippets containing a given substring (todo : integrate it
into the Manage_diary module directly)
************************************************************************************************************************)
open Needed_values ;;

let ap_for_diary = Absolute_path.of_string "Githubbed_archive/diary_archive.ml";;
let (g1,Manage_diary.Private.D g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;
let g3 = Ennig.index_everything g2;;
let g4 = List.filter (fun (j,(x,y))->Substring.is_a_substring_of "Vdw_" y) g3 ;;
let g5 = Image.image fst g4 ;;
let act1 () = Manage_diary.remove_snippets g5;;

let ap1 = Absolute_path.of_string "Fads/cloth.ml" ;;
let act1 () = Manage_diary.extract_at_index_and_append_to_file 
   84 ap1 ;;


let ap1 = Manage_diary.Private.usual_path ;;
let (prologue,diary1) = Manage_diary.Private.read_and_parse ap1 ;;
let (Manage_diary.Private.D l_diary1) = diary1 ;;
let z1 = List.nth l_diary1 (84-1) ;;



(************************************************************************************************************************
Snippet 37 : Search/replace following some module refactoring
************************************************************************************************************************)
open Needed_values ;;

let aps = ref [] ;;
let list_for_reps = ref [] ;;
aps := (Image.image (fun s->Absolute_path.of_string s) 
  [
    "ordered.ml";
    "Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";
    "Van_der_Waerden/vdw_common.ml";
    "Ordered_Lists/functor_for_sets.ml";
    "Ocaml_analysis/follow_ocaml_values.ml";
  ] );;
list_for_reps := [
  "Total_ordering.t)","Total_ordering_t.t)";
  "Total_ordering.t )","Total_ordering_t.t )";
  "Total_ordering.t\r","Total_ordering_t.t\r";
] ;;


let act1 () = List.iter 
  (Replace_inside.replace_several_inside_file 
     (!list_for_reps)) (!aps);

(************************************************************************************************************************
Snippet 36 : Extracting modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line "Van_der_Waerden/Width_up_to_four";;

let u1 =ae () ;;

let u2 = List.filter (
  fun eless ->
    Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory eless) sd1
) u1;; 

let u3 = Image.image (
   fun eless -> Dfa_module.to_line(Dfn_endingless.to_module eless)
) u2 ;;

(************************************************************************************************************************
Snippet 35 : Remove all "automatic" modules 
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae ();;
let u2 = Image.image (fun eless ->
   Dfa_module.to_line(Dfn_endingless.to_module eless)  
) u1;;
let u3 = List.filter (
  fun x-> Supstring.ends_with x "_automatic"
) u2 ;;

let computed_u3 = ["concrete_object_automatic"; "fw_wrapper_automatic"; "coma_state_automatic";
"fw_nonmodular_wrapper_automatic"; "hex_flattened_end_strategy_automatic"];;

let g1 = vfm "hex_flattened_end_strategy_automatic" ;;
let g2 = Image.image fst g1 ;;

let g3 = Image.image (fun x-> Replace_inside.replace_inside_string ("x",x) "let x = Automatic.x ;;") g2;;
let g4 = String.concat "\n" g3 ;;
let g5 = "\n\n\n" ^ g4 ^ "\n\n\n" ;; 

let h1 = List.flatten (Image.image snd g1) ;;
let h2 = Ordered.sort Total_ordering.standard h1 ;;
let h3 = List.iter (
  fun fn -> Replace_inside.replace_inside_file ("Hex_flattened_end_strategy_automatic.","Hex_flattened_end_strategy.") fn
) h2 ;;


(************************************************************************************************************************
Snippet 34 : Typical use of the Manage_diary module
************************************************************************************************************************)
let act1 () = Manage_diary.fix_indexation ();;

let act2 () = Manage_diary.remove_snippets [ (* put indices here *)];;


let diary_text = Io.read_whole_file ap_for_diary ;;

let (g1,g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;

(************************************************************************************************************************
Snippet  33 : Deduce the lower measure from the usual measure (related to Vdw)
************************************************************************************************************************)
let measure n =
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

let lower_measure n =
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



let compute_lower_measure n = 
  let tempf = (fun t->measure(n+t)-measure(t)) in 
  snd(Min.minimize_it tempf (Ennig.ennig 1 20)) ;;   


(************************************************************************************************************************
Snippet  32 : Relocate all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Van_der_Waerden" ;;

let sd2 = Dfa_subdirectory.of_line 
   "Van_der_Waerden/First_try" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = Image.image ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = Explicit.image (fun mn->relo mn sd2) u3 ;;

(************************************************************************************************************************
Snippet  31 : Delete all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Ocaml_analysis/Standardized_concrete_ocaml_types" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = List.rev_map ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = fgs u3 ;;


(************************************************************************************************************************
Snippet  30 : Code from an abandoned, self-contained module
************************************************************************************************************************)
exception Too_many_arguments of int ;;

let wrap_in_parentheses_if_needed typename =
    if String.contains typename ' '
    then "( "^typename^" )"
    else  typename ;;    

let max_nbr_of_arguments = 7 ;;    

let arguments_in_input argname n=
    if n> max_nbr_of_arguments 
    then raise(Too_many_arguments(n))
    else let temp1 = Ennig.doyle (fun k->
          if k<=n then argname^(string_of_int k) else "_") 1 max_nbr_of_arguments in 
         "(" ^ (String.concat "," temp1) ^ ")" ;;       

let listify is_a_list name =
    if not(is_a_list) 
    then name 
    else (wrap_in_parentheses_if_needed name)^" list" ;;     

let add_appendix_to_last_line appendix lines =
      let (last_line,other_lines) = Listennou.ht (List.rev lines) in 
      List.rev ((last_line^appendix)::other_lines) ;;    

(************************************************************************************************************************
Snippet  29 : Permutations far (wrt Hamming distance) from shift with constants. 
************************************************************************************************************************)
open Needed_values ;;

let hamming_distance perm1 perm2 =
  let temp1 = List.combine perm1 perm2 in 
  List.length(List.filter (fun (x,y)->x<>y) temp1);;

let generic_translate n t  = (Ennig.ennig t n) @ (Ennig.ennig 1 (t-1))  ;;

let all_translates =Memoized.make (fun n -> Ennig.doyle (generic_translate n) 1 n);;

let measure n perm = snd(Min.minimize_it (hamming_distance perm) (all_translates n)) ;;


let iii = Memoized.make Permutation.iii ;;

let ff = Memoized.make(fun n->
   let whole = iii n 
   and meas = Memoized.make (measure n) in 
   let m = snd(Max.maximize_it meas whole) in 
   Explicit.filter (fun perm->meas(perm)=m) whole);;   


let gg n = Chronometer.it ff n;;

let hh n = (measure n (List.hd(ff n)));;

Ennig.doyle (fun x->(x,hh x)) 3 10;;

let hf n = List.hd(ff n) ;;

(************************************************************************************************************************
Snippet  28 : Mass inheritance from a Private submodule 
************************************************************************************************************************)
let z1 = 
  ["conventional_files_with_full_content";
   "conventional_files_with_minimal_content"; "debug_build_subdir";
   "exec_build_subdir"; "full_set_of_needed_dirs"; "git_ignored_subdirectories";
   "minimal_set_of_needed_dirs"; "rootless_path_for_loadingsfile";
   "rootless_path_for_parametersfile"; "rootless_path_for_printersfile";
   "rootless_path_for_targetfile"; "usual_build_subdir"; "utility_files_subdir"] ;;
  
let z2 = Image.image (fun x->" let "^x^" = Private."^x^" ;;") z1;; 
  
let z3 = "\n\n\n" ^ (String.concat "\n" z2) ^ "\n\n\n" ;; 

(************************************************************************************************************************
Snippet  27 : Typical use of the Other_coma_state module 
************************************************************************************************************************)
let act1 () = 
   Other_coma_state.repopulate 
   (Needed_data_summary_t.Everything);;

(* or Other_coma_state.initialize () ;; *)   

let see = Other_coma_state.see_yet_unofficial_changes ();; 
let act2 () = Other_coma_state.officialize_changes ();;




Other_coma_state.Private.ref_for_unofficial_changes :=
(
   Some [ ]
)
;;

Other_coma_state.Private.ref_for_unofficial_changes :=
(
   Some ["Filewatching/file_watcher.ml"; "Filewatching/fw_with_archives_t.ml";
   "Filewatching/fw_with_archives.ml";
   "Filewatching/fw_with_small_details_t.ml";
   "Filewatching/fw_with_small_details.ml";
   "Filewatching/fw_with_dependencies.ml";
   "Filewatching/fw_with_batch_compilation.ml";
   "Filewatching/fw_with_githubbing.ml";
   "Compilation_management/usual_coma_state.ml"; "needed_values.ml";
   "node_project.ml"]
)
;;




(************************************************************************************************************************
Snippet  26 : Testing freezing and unfreezing of world copies
************************************************************************************************************************)
open Needed_values ;;

let remote_dir = Dfa_root.of_line 
   (home^"/Teuliou/OCaml/Forgotten_projects/Html_scraping_project") ;;

(*

To store a "frozen" copy of the project in a separate directory.
You can combine this with a cp -R (which often will not suffice by itself since you 
also need the dependecies from other subdirectories).

*)

let sd= Dfa_subdirectory.of_line "Text_editing/Html_scraping";;

let g1 = Create_world_copy.frozen_copy (!ucs)
    ~destination:remote_dir 
    (Needed_data_summary_t.Selection([],
    [sd])) ;;
   
(*

Much later, you can "unfreeze" the project as follows

*)    

let g2 = Create_world_copy.unfreeze_copy (!ucs) remote_dir ;;

(*

Then, you can cd to the separate dir, launch utop in it, and enjoy.

*)


(************************************************************************************************************************
Snippet  25 : Remove interval of lines in a file 
************************************************************************************************************************)
let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (
   fun (j,line)->
      let i1 = Substring.leftmost_index_of_in "val " line 
      and i2 = Substring.leftmost_index_of_in ":" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+4) (i2-1))
) v2 ;;
let tab = String.make 5 ' ' ;;
let v3 = Image.image (fun (j,line) -> 
  if Supstring.begins_with line tab
  then Cull_string.two_sided_cutting (tab,"") line
  else line   
    ) v2;;

let tab = String.make 7 ' ' ;;
let v4 = Ordered.sort Total_ordering.lex_for_strings v3;;
let v5 = Image.image (fun name -> tab^"let "^name^" = Aantron_encoding."^name^" ;;") v4;;
let old_snippet = String.concat "\n" (Image.image snd v2) ;;
let new_snippet = String.concat "\n"  v5;;
let act () = Replace_inside.replace_inside_file (old_snippet,new_snippet) ap ;; 

let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (fun (j,line)->Cull_string.trim_spaces line) v2 ;;
let v4 = List.filter (Substring.is_the_beginning_of "The value ") v3;;
let v5 = Image.image (
   fun line->
      let i1 = Substring.leftmost_index_of_in "`" line 
      and i2 = Substring.leftmost_index_of_in "'" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+1) (i2-1))
) v4 ;;
let v6 = Ordered.sort Total_ordering.lex_for_strings v5;;
let v7 = Image.image (fun name -> "let "^name^" = Aantron_utility."^name^" ;;") v6;;
let v8 = "\n\n\n" ^ (String.concat "\n" v7) ^ "\n\n\n";;
let v9 = print_string v8 ;;

(************************************************************************************************************************
Snippet  24 : Removing module wrappers in a set of files
************************************************************************************************************************)
let remove_module_wrapper_in_text text =
  let lines = Lines_in_string.indexed_lines text in 
  let (i1,_)= Listennou.force_find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "module "
  ) lines in
  let (i2,_)= Listennou.force_find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "end"
  ) (List.rev lines) in 
  let selected_lines = Option.filter_and_unpack (
    fun (i,line)->if List.mem i [i1;i2] then None else Some line
  ) lines in 
  String.concat "\n" selected_lines ;;

let remove_module_wrapper_in_file ap =
  let old_text = Io.read_whole_file ap in 
  let new_text = remove_module_wrapper_in_text old_text in 
  Io.overwrite_with ap new_text ;;

let the_dir = Directory_name.of_string ((Sys.getcwd())^"/Imported/Aantron/Temp"  ) ;;
let u1 = More_unix.simple_ls the_dir ;;

let act1 () = List.iter remove_module_wrapper_in_file u1 ;;

(************************************************************************************************************************
Snippet  23 : Sorting names in the dictionary order
************************************************************************************************************************)
let z1 = Ordered.sort Total_ordering.lex_for_strings 
[
  "current_state";
  "emit";
  "push_and_emit";
  "pop";
  "emit_end";
  "initial_state";
  "document_state";
  "doctype_state";
  "root_state";
  "after_root_state";

] ;;

let z2 = "\n\n\n" ^ (String.concat "\n" z1) ^ "\n\n\n" ;;

print_string z2;;

(************************************************************************************************************************
Snippet  22 : Remove phpbb links to footnotes 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "\n[b][color=blue]("^sk^")[/color][/b]\n" ;;

let reps = Ennig.doyle (fun j->(write1 j,"")) 1 43  ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations/";;  
let ap1 =   Absolute_path.create_file_if_absent (dir^"/notes_to_dot.txt") ;;

let text1= Io.read_whole_file ap1;;
let lines1 = Lines_in_string.indexed_lines text1;;

let act1 () = Replace_inside.replace_several_inside_file reps ap1;;




(************************************************************************************************************************
Snippet  21 : Typical use of Html_to_phpbb.translate
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf (home^"/Teuliou/html_files/Fenton/divine_origin.html");;

let u2 = Html_to_phpbb.translate u1;;

let ap1 = Absolute_path.of_string 
  (home^"/Teuliou/html_files/Translations/divine_origine_translated.txt") ;;

Io.overwrite_with ap1 u2;;  

(************************************************************************************************************************
Snippet  20 : Interaction between "beginning" and "end" of a large tex file
************************************************************************************************************************)
open Needed_values;;

let beg_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/beginning_of_text.txt");; 
 
let end_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/blet_pius_xii.tex");;   

let cmd_for_texshop = "osascript "^home^"/Teuliou/Bash_scripts/Automation/compile_with_texshop.scpt";;

let loop () =
    let beg_part = Io.read_whole_file beg_ap 
    and end_part = Io.read_whole_file end_ap in 
    let whole = beg_part ^ "\n" ^ end_part in 
    let _ =Io.overwrite_with tex_ap whole in 
    Sys.command cmd_for_texshop;;

let tr k = More_io.transfer_first_lines_of_to k end_ap beg_ap;;

let ll k = let temp = Lines_in_string.interval (Io.read_whole_file end_ap) k k in 
  (temp,Strung.explode temp);;

let rye (a,b) = Replace_inside.replace_inside_file (a,b) end_ap ;; 

let rblap () = Remove_blank_lines_around_percents.in_file end_ap ;;

let rlc pattern = 
   let _ = Lines_in_string.remove_lines_containing_substring_in_file 
   pattern end_ap in rblap ();;

let usual_cleaning () =
   Replace_inside.replace_several_inside_file 
   [
     (".! ",".1 ");
     (".!\n",".1 ");
    ("\012","");
    ("_","");
    ("#","");
    ("\\Vhat","What");
    ("\\Vhen","When");
    ("\\Vhile","While");
    ("\194\162","c");
    ("\226\128\156","\194\171");
    ("\226\128\157","\194\187");
    ("$","\194\167");
    (" & "," \\& ");
    ("&\226\128\153","d'");
    ("\\xii. ","xii. ");
    ("\194\165","V");
    ("\n1}","\n1 ");
    ("\n}","\n1 ");
    ] end_ap ;;


(************************************************************************************************************************
Snippet  19 : Add blank space at the beginning of lines (to make copy&paste easier )
************************************************************************************************************************)
open Needed_values;;

let blanks = String.make 3 ' ';; 

let reform_line x=
  if (x="")||(Supstring.begins_with x blanks) then x else blanks^x;; 

let reform_string s=
  let temp1 = Lines_in_string.lines s in 
  let temp2 = Image.image reform_line temp1 in 
  String.concat "\n" temp2 ;;


let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/PDF_files/Printable/Preparation/greek_in_vl.txt");; 

let old_text = Io.read_whole_file the_ap ;;

let new_text = reform_string old_text ;;

Io.overwrite_with the_ap new_text;; 


(************************************************************************************************************************
Snippet  18 : Delete some HTML footnotes (with their links) and reindex
************************************************************************************************************************)
open Needed_values;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;

let u1 = Enumerate_html_links_to_footnotes.main old_text ;;

let see = Image.image (fun ((i_start,i_end),link_idx)->
    Cull_string.interval old_text i_start i_end) u1 ;; 

let bad_indices = [1;3;4;5;10;11;18;20] ;;

let u2 = List.filter (fun ((i_start,i_end),link_idx)-> 
    (List.mem link_idx bad_indices) ) u1;;
let u3 = Image.image fst u2 ;;
let u4 = Ennig.index_everything u3 ;; 
let u5 = Image.image (
  fun (k,(i_start,i_end))->((i_start,i_end),k)
) u4;;
let u6 = Image.image (fun ((i_start,i_end),link_idx)-> 
    ((i_start,i_end),List.assoc_opt (i_start,i_end) u5) 
) u1;;
let write_link opt = match opt with 
  None -> ""
  |Some(k) -> let sk=string_of_int k in 
              "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>";;

let u7 = Image.image ( fun (pair,opt)->(pair,write_link opt) ) u6;;

let new_text = Strung.replace_ranges_in u7 old_text ;;

Io.overwrite_with ap1 new_text ;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;
let v1 = Enumerate_html_footnotes.main old_text ;;
let see = Image.image (fun ((i_start,i_end),_)->
    Cull_string.interval old_text i_start i_end) v1 ;;   
let good_indices = List.filter (fun k->not(List.mem k bad_indices )) (Ennig.ennig 1 (List.length v1));;
let reindexation = Image.image (fun (i,j)->(j,i)) (Ennig.index_everything good_indices) ;;
let v2 = Image.image (
  fun ((footnote_idx,html_content),(i_start,i_end))->
    ((footnote_idx,html_content),(i_start,i_end),List.assoc_opt footnote_idx reindexation)
) v1;;
let write_reindexed_version ((i_start,i_end),(footnote_idx,html_content),opt_idx)=
   let new_text = (match opt_idx with 
      None -> ""
      |Some(k)->let sk=string_of_int k in 
      "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a> "^html_content^"</div>"
   ) in 
   ((i_start,i_end),new_text);;
let v3 = Image.image write_reindexed_version v2;;
let new_text = Strung.replace_ranges_in v3 old_text ;;
Io.overwrite_with ap1 new_text ;;





(************************************************************************************************************************
Snippet  17 : Remove contiguous lines in a file
************************************************************************************************************************)
open Needed_values ;;

let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 

let old_text = Io.read_whole_file the_ap ;;

let to_be_deleted = Lines_in_string.interval old_text 3387 4948 ;;

Replace_inside.replace_inside_file (to_be_deleted,"") the_ap ;; 


(************************************************************************************************************************
Snippet  16 : Put fillable footnotes in an html draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>"^
  "\n\n\n"^
  "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a>   \n\n "^
  "</div>" ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Fortescue";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/pra_filled.html") ;;

let memo = String.concat "\n\n" (Ennig.doyle write1 121 170) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  15 : Aggregate pages
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Adrifor";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.extract_page_range "main" (5,5) ;;

Coherent_pdf.implode ("p","") ;;

Coherent_pdf.merge ["part1";"part2";"part3"] "whole";;

(************************************************************************************************************************
Snippet  14 : Cleaning up and fixing a chaotic mass download
************************************************************************************************************************)
let downloads_s_dir = home ^ "/Downloads";; 

let u1 = More_unix.quick_beheaded_complete_ls downloads_s_dir  ;;
let u2 = List.filter (Substring.is_the_beginning_of "iau") u1;;

let p_value s =
     let j1 = Substring.leftmost_index_of_in_from "-" s 5 in 
     let j2 = Substring.leftmost_index_of_in_from "-" s (j1+1) in
     int_of_string(Cull_string.interval s (j1+1) (j2-1));; 

let min_pageNumber = 9 and max_pageNumber = 70 ;; 

let pre_u3 = Image.image (fun s->(s,p_value s)) u2 ;;
let (bad_ones1,u3) = List.partition (fun (s,p)->(p<min_pageNumber) || (p>max_pageNumber)) pre_u3 ;;
let cmds1 = Image.image (fun (s,_)->"rm "^downloads_s_dir^"/"^s) bad_ones1;;
let act1 () = Image.image Sys.command cmds1 ;;

let reached_page_numbers = Ordered.sort Total_ordering.for_integers (Image.image snd u3) ;; 

let u4 = Ennig.doyle (
   fun p->(p,Option.filter_and_unpack (fun (s,q)->if q=p then Some s else None) u3)
) min_pageNumber max_pageNumber;;

let u5 = List.filter (fun (p,representatives) -> List.length(representatives)>1) u4 ;;
let bad_ones2 =List.flatten 
  (Image.image (fun (p,representatives) -> List.tl(representatives)) u5);;
let cmds2 = Image.image (fun s->"rm "^downloads_s_dir^"/"^s) bad_ones2;;
let act2 () = Image.image Sys.command cmds2 ;;

let bad_ones3 = Option.filter_and_unpack 
  (fun (p,representatives) -> 
     if List.length(representatives)=0 then Some p else None) u4 ;;

    
let cmds3 = Image.image (fun (p,l)->
    let fn = List.hd l in 
    let sk = string_of_int(p-6) in 
    "mv "^downloads_s_dir^"/"^fn^" "^downloads_s_dir^"/p"^sk^".pdf") u4;;
let act3 () = Image.image Sys.command cmds3 ;;

let workdir = home^"/Downloads/";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.implode ("p","") ;;



(************************************************************************************************************************
Snippet  13 : Update footnote format in old phpbb text
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/OCRed_texts/barenton_on_loisy.txt");;

let main_text = Io.read_whole_file ap ;; 

let opening_tag= "[color=blue]";;
let closing_tag = "[/color]";;

let u1 = Substring.occurrences_of_in opening_tag main_text ;;
let u2 = Substring.occurrences_of_in closing_tag main_text ;;

let u3 = List.combine u1 u2;;
let opening_length = String.length opening_tag ;;
let closing_length = String.length closing_tag ;;
let u4 = Image.image (fun (old_a,old_b)->
    let a = old_a+opening_length and b=old_b-1 in 
    ((a,b),Cull_string.interval main_text a b)
    ) u3;;
exception RA of string ;; 
    
let rhine_analysis s=
  let j1 = Substring.leftmost_index_of_in "(" s in 
  let j2 = Substring.leftmost_index_of_in ")" s in 
  if (j1<0)||(j2<0) then raise(RA(s)) else    
  let idx = int_of_string(Cull_string.interval s (j1+1) (j2-1)) in 
  (idx,Cull_string.cobeginning j2 s);;

let u5 = Image.image (
   fun ((a,b),text) -> (((a,b),text),rhine_analysis text)
)  u4 ;; 

let (redundant_u6,redundant_u7)=List.partition (fun (_,(idx,content))->content="") u5;;

let u6 = Image.image (fun (((a,b),text),(idx,content))->
      ((a-opening_length,b+closing_length),idx) ) redundant_u6;;

let u7 = Image.image (fun (((a,b),text),(idx,content))->
        ((a-opening_length,b+closing_length),idx,content) ) redundant_u7;;      

let check1 = ( (List.length u6) = (List.length u7) ) ;;        

let u8 = Image.image (
   fun ((a,b),idx,content) ->
      let s_idx=string_of_int idx in 
      ((a,b),"[size=90][b][color=blue]("^s_idx^")[/color][/b]"^content^"[/size]")
) u7;;
let corrected_text = Strung.replace_ranges_in u8 main_text;;

Io.overwrite_with ap corrected_text ;;



(************************************************************************************************************************
Snippet  12 : Combinatorial musings
************************************************************************************************************************)
exception Hard_computation of string * int ;;

let translate_all t (n,sols)=
  let increment = (if t="1" then 1 else 0) in  
  (n+increment,Image.image (fun u->t^u) sols) ;;

let synthesize_after_translating (n1,sols1) (n2,sols2) =
    if n1 < n2 then (n2,sols2) else 
    if n2 < n1 then (n1,sols1) else (n1,sols1@sols2);; 

let synthesize res1 res2 = 
    synthesize_after_translating 
     (translate_all "0" res1) (translate_all "1" res2);; 


let main_hashtbl = ((Hashtbl.create 50): (string * int, int * (string list)) Hashtbl.t);; 

let am x y = Hashtbl.add main_hashtbl x y;;

let eval_at_one pattern =
     if pattern="" then (1,["1"]) else 
     (if (String.get pattern 0)='F' then  (1,["1"]) else (0,["0"]) );;

let enforce_conditions pattern = 
    let m = String.length pattern in 
    let temp2 = Ennig.doyle (fun j->
        if (j<5)&&(j<>2) then "N" else 
        if j>m then "F" else Cull_string.interval pattern j j) 1 (max 4 m) in 
    String.concat "" temp2;;     

(*

enforce_conditions "A";;
enforce_conditions "ANPE";;

*)

let prepare_computation pattern=
   if pattern="" then ("",Some"NFNN") else 
   let tail = Cull_string.cobeginning 1 pattern in 
   (
    if (String.get pattern 0)='F'
    then (tail,Some(enforce_conditions tail))  
    else (tail,None)    
   )      ;;

let left_n_decomposition pattern =
     let j1 = Strung.char_finder_from (fun c->c<>'N') pattern 1 in 
     if j1=0 
     then (String.length pattern,"") 
     else (j1-1,Cull_string.cobeginning (j1-1) pattern);; 

(*

left_n_decomposition "ABC";;
left_n_decomposition "NNABC";;

*)


let eval_quickly pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one pattern else
    match Hashtbl.find_opt main_hashtbl (pattern,n) with 
     Some(l,sol)->(l,sol)
     |None -> raise(Hard_computation(pattern,n)) ;;

let eval_using_left_n_decomposition old_pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one old_pattern else
    let old_length = String.length old_pattern in 
    let pattern = (if old_length > n 
                   then Cull_string.beginning n old_pattern
                   else old_pattern) in 
    let (number_of_ns,core) = left_n_decomposition pattern in 
    let (size_of_sols,old_sols) = eval_quickly core (n-number_of_ns) in 
    let new_sols =(
       if number_of_ns=0 
      then old_sols 
      else  let offset = String.make number_of_ns '0' in 
      Image.image (fun t->offset^t) old_sols
      ) in 
    (size_of_sols,new_sols);; 


let eval_slowly pattern n =
       try eval_using_left_n_decomposition pattern n with _->
         let (passive_case,opt_active_case) = prepare_computation pattern in 
         let case0 =  translate_all "0" (eval_using_left_n_decomposition passive_case (n-1)) in 
         match opt_active_case with 
         None -> case0
        |Some(active_case) ->
          let case1 =  translate_all "1" (eval_using_left_n_decomposition active_case (n-1)) in 
          synthesize_after_translating case0 case1;;     

let consider pattern n=
   let res = eval_slowly pattern n in 
   let _= (am (pattern,n) res) in res ;;

let ff n = eval_slowly "" n;;

let bf n = Image.image ff (Ennig.ennig 1 n);;



consider "" 2 ;;
consider "" 3 ;;
consider "FN" 2;;
consider "" 4;;
for k=3 to 30 do let _ = consider "FNN" k in ();let _=consider "" (k+2) in () done ;;


let res1 = Ennig.doyle (fun x->fst(ff x)) 1 30;;




(************************************************************************************************************************
Snippet  11 : Massive conversion of audios into videos using ffmepgs
************************************************************************************************************************)
let base1 =
  [
  
  "001_Ier_D_de_l_Avent_01_12_2013_27min33.mp";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp";
  "017_Epiphanie_06_01_1996_38min27.mp"; "018_Epiphanie_07_01_90_29min34.mp";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp";
  "035_Paques_Moulins_04_04_1996_44min07.mp";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp";
  "041_Ascension_24_05_1990_25min18.mp";
  "042_Ascension_Moulins_09_05_1991_27min55.mp";
  "043_Ascension_2011_Vendee_20min32.mp";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp";
  "096_Assomption_Moulins_15_08_1991_29min15.mp"; "097_Assomption_2012.mp";
  "098_Assomption_2013_Vendee_21min34.mp";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp";
  "118_Presentation_des_ouvrages_de_labbe_Zins_video_001_52min.mp";
  "119_Presentation_des_ouvrages_de_labbe_Zins_video_002_1h.mp";
  "120_Presentation_des_ouvrages_de_labbe_Zins_video_003_55min.mp";
  "121_Presentation_des_ouvrages_de_labbe_Zins_video_004_42min.mp"
  
  ];;

let n1 = List.length base1 ;;

let main_list = String.concat "\n" base1 ;; 




let write1 x =  
    let idx = string_of_int(int_of_string(String.sub x 0 3)) in 
    "wget -c http://larchange.org/audio/"^x^"3\n"^
    "ffmpeg -loop 1 -i STP.jpg -i "^x^"3 -acodec copy -vcodec libx265 -shortest "^x^"4\n"^
    "rm "^x^"3\n"^
    "echo $'\\n\\n\\n\\n\\n\\n Step "^idx^" of 121 finished\\n\\n\\n\\n\\n\\n'" ;; 

let home = Sys.getenv "HOME" ;;    
let ap_for_main_script = Absolute_path.of_string 
    (home^"/Teuliou/Bash_scripts/Convert_audio_to_video/audiotovideo.sh");;    

let main_script = String.concat "\n\n\n" (Image.image write1 base1) ;;      

let fill_main_script () =
   Io.overwrite_with ap_for_main_script main_script ;;

fill_main_script () ;;


let base2 = Ennig.index_everything base1;;
let (pre_part1,remains1) = List.partition (fun (j,x)->j<=40) base2;;
let (pre_part2,pre_part3) = List.partition (fun (j,x)->j<=80) remains1;;
let part1 = Image.image (fun (_,s)->s^"4") pre_part1;;
let part2 = Image.image (fun (_,s)->s^"4") pre_part2;;
let part3 = Image.image (fun (_,s)->s^"4") pre_part3;;

let main_dir = (Sys.getenv "HOME")^"/Teuliou/Bash_scripts/Convert_audio_to_video/Sermons";;
let cmds1 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_1_a_40/"
) part1 ;;
let cmds2 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_41_a_80/"
) part2 ;;
let cmds3 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_81_a_117/"
) part3 ;;
let cmds = cmds1 @ cmds2 @ cmds3 ;;





(************************************************************************************************************************
Snippet  10 : Removing misinterpreted characters from a freshly OCR-ed doc
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let dirname = "Lossky";;
let num_of_pages = 196 ;;    

let partial_texts = Ennig.doyle (fun k->
    let sk = string_of_int k in 
    let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
    let prelude="% Beginning of page "^sk^"\n"
    and postlude="\n% End of page "^sk in 
    prelude^(rf fn)^postlude)  1 num_of_pages ;;
      
let full_ap = Absolute_path.create_file_if_absent  
(home^"/Downloads/"^dirname^"/full.txt");;  
let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/pre_vladimir_lossky.txt");; 
 

let full_text = "\n"^(String.concat "\n" partial_texts)^"\n" ;;


let adjusted_text = Replace_inside.replace_several_inside_string 
   ["&","\\&";
    "$","\\$";
    "#","\\#";
    "_","\\_";
    "\194\162","c";
    "\194\176","o";
    "\226\130\172","E"] full_text ;;   
 
Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string adjusted_text) ("% BEGINNING MARKER","%END MARKER") tex_ap;;


let text1 = Io.read_whole_file tex_ap ;;
let u1 = Substring.occurrences_of_in "% End of page 7\n" text1 ;;
let i1 = List.hd u1 ;;
let u2 = Cull_string.interval text1 (i1-20) i1;;

Replace_inside.replace_several_inside_file
   ["\n\012\n","\n"] tex_ap ;;

(************************************************************************************************************************
Snippet  9 : Typical use of the Trim_text_between_tags module
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Translations/act_of_body_translated.txt");;

Trim_text_between_tags.in_file [("[i]","[/i]")] ap;;


(************************************************************************************************************************
Snippet  8 : Put fillable footnotes in a phpbb draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "[b][color=blue]("^sk^")[/color][/b]\n\n"^
  "[size=90][b][color=blue]("^sk^")[/color][/b]   [i]   [/i]   [/size]";;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/temp.txt") ;;

let memo = String.concat "\n\n" (Ennig.doyle write1 1 5) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  7 : Finding extremal vertices in a polytope
************************************************************************************************************************)
open Needed_values ;;


let small_n=1;;

let u1 = Cartesian.fifth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a4,a5)->
      let a6 = a3+a4-a5
      and a7 = a2+a4-a5
      and a8 = a1-a4+a5 in
      if List.for_all (fun x->(x>=0)) [a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;


let u3 = List.tl u2 ;; 

let shadow (a1,a2,a3,a4,a5,a6,a7,a8) =
    let l = [a1;a2;a3;a4;a5;a6;a7;a8] in 
    Set_of_polys.safe_set(List.filter (fun j->List.nth l (j-1)=0) [1; 2; 3; 4; 5; 6; 7; 8]) ;;

let supporting_rel uple uple2 =
    if uple=uple2 then false else 
    Set_of_polys.is_included_in (shadow uple) (shadow uple2);;  
     
let supporters uple = List.filter (supporting_rel uple ) u3;;

let u4 = Option.filter_and_unpack (
   fun uple -> let r= supporters uple in 
   if r<>[]
    then Some(uple,r)
  else None
) u3;;

let u5 = List.filter (fun uple->(supporters uple)=[]) u3;;

[[0, 0, 0, 1, 1, 0, 0, 0],
   [0, 0, 1, 0, 0, 1, 0, 0], [0, 0, 1, 1, 1, 1, 0, 0],
   [0, 1, 0, 0, 0, 0, 1, 0], [0, 1, 0, 1, 1, 0, 1, 0],
   [0, 1, 1, 0, 0, 1, 1, 0], [0, 1, 1, 0, 1, 0, 0, 1],
   [0, 1, 1, 1, 1, 1, 1, 0], [1, 0, 0, 0, 0, 0, 0, 1],
   [1, 0, 0, 1, 0, 1, 1, 0], [1, 0, 0, 1, 1, 0, 0, 1],
   [1, 0, 1, 0, 0, 1, 0, 1], [1, 0, 1, 1, 0, 2, 1, 0],
   [1, 0, 1, 1, 1, 1, 0, 1], [1, 1, 0, 0, 0, 0, 1, 1],
   [1, 1, 0, 1, 0, 1, 2, 0], [1, 1, 0, 1, 1, 0, 1, 1],
   [1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 1, 0, 1, 0, 0, 2],
   [1, 1, 1, 1, 0, 2, 2, 0], [1, 1, 1, 1, 1, 1, 1, 1]]

(*

let small_n=2;;

let u1 = Cartesian.fourth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a5)->
      let a4 = small_n-(a1+a2+a3) 
      and a6 = small_n-(a1+a2+a5)
      and a7 = small_n-(a1+a3+a5) 
      and a8 = (2*a1+a2+a3+a5)-small_n in
      if List.for_all (fun x->(x>=0)&&(x<=small_n)) [a4;a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;

let u3 = List.filter (
   fun (a1,a2,a3,a4,a5,a6,a7,a8) -> 
    List.exists (fun (x,y)->x<>y) [a1,a8;a2,a7;a3,a6;a4,a5]
) u2;;

*)

(************************************************************************************************************************
Snippet  6 : Abandoned code snippet to remove paragraph containing footnotes.
It is much simpler to add html paragraph tags only when the region of text
does not contain footnotes (see the Htmlize module and snippet 4)
************************************************************************************************************************)
exception Unbalanced_html_paragraph_tags of int * int ;;
exception Nested_html_paragraphs of (int * int) * (int * int) ;;

let footnote_marker = ref " nowfeetneto ";;


let html_par_opening_tag = "<p>";;
let html_par_closing_tag = "</p>";;

let op_tag_length = (String.length html_par_opening_tag)-1 ;;
let cl_tag_length = (String.length html_par_closing_tag)-1 ;;

let detect_nested_paragraphs l=
   let temp1 = Listennou.universal_delta_list l in 
   match Option.seek (fun 
     (((i1,j1),(i2,j2)),((i3,j3),(i4,j4)))->i3<j2
   ) temp1 with 
   None -> ()
   |Some(((i1,j1),(i2,j2)),((i3,j3),(i4,j4))) ->
       raise(Nested_html_paragraphs((i2,j2),(i3,j3)));;

let locate_all_paragraphs_in_html txt=
  (* paragraphs are assumed to be non-nested *)
  let temp1 = Substring.occurrences_of_in html_par_opening_tag txt 
  and temp2 = Substring.occurrences_of_in html_par_closing_tag txt in 
  let o1 = List.length temp1 and c1 = List.length temp2 in 
  if o1<>c1 
  then raise(Unbalanced_html_paragraph_tags(o1,c1))
  else   
  let temp3 = Image.image (fun i->(i,i+op_tag_length)) temp1
  and temp4 = Image.image (fun j->(j,j+cl_tag_length)) temp2 in 
  let temp5 = List.combine temp3 temp4 in 
  let _ = detect_nested_paragraphs temp5 in 
  temp5 ;; 

let remove_paragraphs_containing_footnotes txt l= ();;
        



(************************************************************************************************************************
Snippet  5 : Mass deletion of modules 
************************************************************************************************************************)
open Needed_values;;

let u1 = ae ();;

let u2 = List.filter (fun
   (Dfn_endingless_t.J(r,s,m)) -> Dfa_subdirectory.begins_with s 
   (Dfa_subdirectory_t.SD "Text_editing")
) u1 ;;

let u3 = Image.image (fun
(Dfn_endingless_t.J(r,s,m)) -> match m with 
  (Dfa_module_t.M m0) -> m0
) u2;;

let u4 = List.filter (
  fun m0->not(List.mem m0 ["control_pdf_size";"read_russian"])
) u3;;

let u5 = Image.image (
  fun m0->(m0, bel m0)
) u4;;

let u6 = List.filter (
  fun (m0,b0)->List.for_all (fun m1->(List.mem m1 u4)) b0
) u5;;

let u7=List.rev(Image.image fst u6);;

(************************************************************************************************************************
Snippet  4 : Code to OCR-size PDF's into .txt (and later html)
************************************************************************************************************************)
open Needed_values ;;


let write1 k =
    let sk = string_of_int k in 
    "pdftoppm main.pdf p"^sk^" -png -f "^sk^" -singlefile\n"^
    "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Pius_XII";;
let num_of_pages = 326 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Ennig.doyle write1 1 num_of_pages))^"\n\n\n" ;;   
    
Io.overwrite_with ap1 text1;;


let partial_texts = Ennig.doyle (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  "%\n% Page "^sk^" \n%\n"^(rf fn))  7 num_of_pages ;;


let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;

let (page1,page2,ranges_for_lfm,ranges_for_fm) =
   Option.unpack(!(Htmlize.Private.error_handling_ref ));;

(* Re-indexed version *)

let main_list = 
  [51;149;187;189;201;203;231;249;257;261;263;265;269;271;297] ;;

let write1 j =
 let k =List.nth main_list (j-1) in 
 let sj = string_of_int j 
 and sk = string_of_int k in 
 "pdftoppm main.pdf p"^sk^" -png -f "^sj^" -singlefile\n"^
 "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Blet_again";;
let num_of_pages = 15 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Ennig.doyle write1 1 num_of_pages))^"\n\n\n" ;;   
 
Io.overwrite_with ap1 text1;;

let partial_texts = Ennig.doyle (fun j->
let k =List.nth main_list (j-1) in   
let sk = string_of_int k in 
let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
let announcer = "%\n% Page "^sk^" \n%\n" in 
(announcer,announcer^(rf fn)))  1 num_of_pages ;;

let end_ap = Absolute_path.of_string 
 (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let act () = Replace_inside.replace_several_inside_file 
 partial_texts end_ap;;  


(************************************************************************************************************************
Snippet  3 : Typical use of the Read_russian module
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME";;
let txt1 = rf (home^"/Downloads/temp.txt");;

let z1 = Read_russian.read txt1;;
let z2= Read_russian.prepare_dictation txt1;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/LaTeX/Moullapl/archipelago.tex");;

let act () = 
Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string z2)
  ("\\begin{document}","\\end{document}") ap1;;

(************************************************************************************************************************
Snippet  2 : Convert footnotes between phpBB and HTML
************************************************************************************************************************)
let peggy j =
   let sj=string_of_int j in 
   "<span id=\""^"ln"^sj^"\"><a href=\"#n"^sj^"\">("^sj^")</a></span>";;
 
 let u1 = Ennig.doyle peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 
 
 let peggy j =
   let sj=string_of_int j in 
   "<div id=\""^"n"^sj^"\"><a href=\"#ln"^sj^"\">("^sj^")</a> <i> </i>  </div>";;
 
 let u1 = Ennig.doyle peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 

(************************************************************************************************************************
Snippet  1 : Typical use of the Coherent_pdf module on a freshly scanned doc
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Building_Site";;

Coherent_pdf.workspace_directory := workdir ;;

let cmd1 = "cp "^workdir^"/moncunill.pdf "^workdir^"/whole.pdf";;

let act1 () = Sys.command cmd1;;

let act2 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:1 ~range_end:1 ~deflated_one:"whole" 
~total_length:28;;

let act3 () = Coherent_pdf.append_on_the_right "whole" "velaza" ;;

let act5 () = Coherent_pdf.transfer_range_to_rightmost
~range_start:91 ~range_end:91 ~deflated_one:"whole" 
~total_length:592 ~receiving_one:"p90" ;;

let act6 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p90" ~receiving_one: "whole"
     ~page_number:89 ~initial_total_length:591;;

let act7 () = Coherent_pdf.delete_file "p90";;     

let act8 k = Coherent_pdf.replace_page_number_in_by 
~page_number:k ~receiving_one:"whole"  ~inserted_one:("p"^(string_of_int k)) 
~total_length:133;;

Image.image act8 [36;40;42;44;68;70;72;90;100] ;;

let act9 () = Coherent_pdf.extract_page_range "whole" (10,10);;

let act10 () = Coherent_pdf.extract_odd_pages "russia" ;;

let act11 () = Coherent_pdf.intertwine 
~odd_pages:"odd" ~even_pages:"even" 
~num_odd:81  ~num_even:81 ~final_name:"118_to_279" ;;

let phoebe n=
  let q =(n/4) in 
  let r= n - 4*q in 
  (4*q)+List.assoc r [0,(-3);1,2;2,3;3,4;];; 
    
let act12 () =   
   Explicit.image (
     fun k-> 
      let j = phoebe k in 
      Coherent_pdf.rename 
      ("p"^(string_of_int k)) ("q"^(string_of_int j))
   ) (Ennig.ennig 1 260) ;;

let act13 ()= Coherent_pdf.implode ("q","") ;; 

let workdir = home^"/Downloads";;

Coherent_pdf.workspace_directory := workdir ;;

let act1 () = Coherent_pdf.extract_page_range "mariage" (24,24);;

let act2 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "xx" ~receiving_one: "mariage"
     ~page_number:22 ~initial_total_length:732;;

let act3 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:25 ~range_end:25 ~deflated_one:"mariage" 
~total_length:733;;

let act4 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p272" ~receiving_one: "mariage"
     ~page_number:315 ~initial_total_length:732;;

