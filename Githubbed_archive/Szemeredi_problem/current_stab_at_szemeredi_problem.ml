(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)

open Needed_values ;;


open Sz_types ;; 

let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;

let concretize (n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 

let test_for_admissibility_up_to_max_with max_width z =
  if max_width<1 then true else 
  Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;

let test_for_admissiblity width breadth z =
   (test_for_admissibility_up_to_max_with (width-1) z)
   &&
   (List.for_all (fun t->
    not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;

let remove_one_element (n,scrappers) k=
  let new_scrappers = i_insert k scrappers in 
  if k <> n then (n,new_scrappers) else 
  let new_z =  concretize (n,new_scrappers) in 
  let new_max = List.hd(List.rev new_z) in 
  (new_max,List.filter (fun t->t<new_max) new_scrappers) ;;


(*

remove_one_element (10,[3;7;8;9]) 10 ;;

*)



module Constraint = struct

let extra_constraints_from_boundary_increment width breadth n =
    let mainstream = Int_range.scale (fun j->
        let k = width-j in [n-2*k;n-k]
     ) 1 (width-1) in
     let lower_end = n-2*width in 
     if (lower_end>=1) && (lower_end<=breadth) 
     then [lower_end;lower_end+width]::mainstream 
     else mainstream ;;   

let satisfied_by_individual l_constr l =
  List.for_all (fun constr->not(i_is_included_in constr l)) l_constr

let satisfied_by_all_in_list l_constr ll=
  List.for_all (satisfied_by_individual l_constr) ll ;;

let merge_constraints l_constr1 l_constr2 =
    Ordered_misc.minimal_elts_wrt_inclusion (il_merge l_constr1 l_constr2) ;;

end ;;  


module Point = struct 
    
  let width (P(w,b,n,s)) = w ;;
  let breadth (P(w,b,n,s)) = b ;;
  let size (P(w,b,n,s)) = n ;;
  let scrappers (P(w,b,n,s)) = s ;;
  let unveil (P(w,b,n,s)) = (w,b,n,s) ;;
  
  
end ;;  
  
module Accumulator_with_optional_anticipator = struct 

let low_hashtbl = Hashtbl.create 50 ;;
let low_anticipator = ref [] ;; 

let get_from_low_hashtbl ~with_anticipation pt =
    if not(with_anticipation)
    then  Hashtbl.find_opt low_hashtbl pt 
    else
        match List.assoc_opt pt (!low_anticipator) with 
        Some anticiped_answer -> Some anticiped_answer 
        | None -> Hashtbl.find_opt low_hashtbl pt  ;;

let add_to_low_hashtbl ~with_anticipation pt vaal=
  if not(with_anticipation)
  then   Hashtbl.replace low_hashtbl pt vaal
  else low_anticipator := (pt,vaal) :: (!low_anticipator)  ;;
  

end ;;   


module Rubber_definition = struct 

let check_constraints constraints = function 
Constraining (rcl,constraints2) -> List.for_all (fun weak->
   List.exists (fun strong->i_is_included_in strong weak) constraints2
  ) constraints 
| Merger (_,_) -> false
| Short_name (ll) -> Constraint.satisfied_by_all_in_list constraints ll ;;


end ;;   

module Rubber_core_list = struct 

let ref_for_definitions = ref [] ;; 
let ref_for_ambient_spaces = ref [] ;;
let ref_for_representatives = ref [] ;; 
let ref_for_constraint_data = ref [] ;;

let add_definition rcl rcl_defn =
  ref_for_definitions :=   
     (rcl_defn,rcl) :: (!ref_for_definitions) ;;    

exception Find_associated_definition_exn of rubber_core_list ;;

let find_associated_definition rcl =
    match Option.seek (
      fun pair -> snd(pair) = rcl
    )(!ref_for_definitions) with 
    Some (rcl_defn,_) -> rcl_defn
    |None -> raise(Find_associated_definition_exn rcl);;

exception Find_from_definition_exn of rubber_definition ;;

let find_from_definition rcl_defn =
  match List.assoc_opt rcl_defn (!ref_for_definitions) with 
  Some (rcl) -> rcl
  |None -> raise(Find_from_definition_exn rcl_defn);;


exception Find_ambient_space_exn of rubber_core_list ;;

let find_ambient_space rcl =
        match List.assoc_opt rcl (!ref_for_ambient_spaces) with 
        Some n -> n
        |None -> raise(Find_ambient_space_exn rcl);;

exception Find_representative_exn of rubber_core_list ;;

let find_representative rcl =
    match List.assoc_opt rcl (!ref_for_representatives) with 
     Some n -> n
    |None -> raise(Find_representative_exn rcl);;

exception Impose_constraint_exn of rubber_core_list * ((int list) list);;

let impose_constraints rcl constraints = 
    if Rubber_definition.check_constraints constraints (find_associated_definition rcl)
    then Some rcl
    else  
    match List.assoc_opt (rcl,constraints) (!ref_for_constraint_data) with 
      Some old_answer -> old_answer 
    | None -> raise(Impose_constraint_exn(rcl,constraints)) ;; 

let common_length rcl = List.length (find_representative rcl) ;; 

end ;; 
 
module Rubber_list = struct 

  exception List_too_large of int list list ;;   

  let of_list ll = 
     if List.length(ll)<=500 
     then Short_list ll 
     else let defn = Short_name ll in 
          try Rubber(Rubber_core_list.find_from_definition defn,[]) with 
          _ -> raise(List_too_large ll) ;;

  let unveil = function 
   (Short_list small_list) -> (Some small_list, None) 
  |Rubber(rcl,common) -> (None,Some(rcl,common)) ;; 

  let common_length = function 
  (Short_list small_list) -> List.length (List.hd small_list) 
 |Rubber(rcl,common) -> (Rubber_core_list.common_length rcl) + (List.length(common)) ;; 

  let remaining_part_of_constraint rcl common new_constraint = 
    let n = Rubber_core_list.find_ambient_space rcl in 
    let (below,above) = List.partition (fun t->t<=n) new_constraint in 
    if not(i_is_included_in above common)
    then None 
    else Some below ;;

    
  let apply_new_constraints new_constraints = function 
  (Short_list small_list) -> 
    let new_small_list = List.filter (fun z->
      List.for_all (fun new_constraint ->
      not(i_is_included_in new_constraint z))  new_constraints
  )  small_list in 
    of_list new_small_list 
   |Rubber(rcl,common) -> 
      let cleaned_constraints = Option.filter_and_unpack (
        remaining_part_of_constraint rcl common
      ) new_constraints in 
      match Rubber_core_list.impose_constraints rcl cleaned_constraints with 
       None -> (of_list [])
       | Some new_rcl -> Rubber(new_rcl,common) ;;
  
  
  

   exception Remove_fixed_part of rubber_core_list * (int list) ;;

   let remove_fixed_part_on_all extra = function 
   (Short_list small_list) -> 
    let new_small_list = Image.image (fun z->i_setminus z extra) small_list in 
    of_list new_small_list 
   |Rubber(rcl,common) -> 
      let n =  Rubber_core_list.find_ambient_space rcl in 
      if List.exists (fun t->t<=n) extra 
      then raise(Remove_fixed_part(rcl,extra))
      else       
      Rubber(rcl,i_setminus common extra) ;;

         


  end ;; 



module Selector_for_hook = struct 

  let of_hook (width,breadth,n) = function   
      Passive_repeat -> Passive_repeat_selector(width,breadth)
    | Boundary_increment -> Boundary_increment_selector(width,breadth,n) 
    | Fork -> Fork_selector  
    | Jump -> Jump_selector ;;
  
    let apply_passive_repeat width b rl =
      Rubber_list.apply_new_constraints [[b;b+width;b+2*width]] rl ;; 
     
     
    let apply_boundary_increment width breadth n rl = 
      let new_constraints = Constraint.extra_constraints_from_boundary_increment width breadth n in 
     match rl with 
    (Short_list small_list) -> 
      let new_small_list = Option.filter_and_unpack (fun z->
        if List.for_all (fun new_constraint ->
        not(i_is_included_in new_constraint z))  new_constraints 
        then Some(z@[n])
        else None
    )  small_list in 
      Rubber_list.of_list new_small_list 
     |Rubber(rcl,common) -> 
        let cleaned_constraints = Option.filter_and_unpack (
          Rubber_list.remaining_part_of_constraint rcl common
        ) new_constraints in 
        match Rubber_core_list.impose_constraints rcl cleaned_constraints with 
         None -> (Rubber_list.of_list [])
         | Some new_rcl -> Rubber(new_rcl,i_insert n common) ;;
  
    let apply_fork ll =
      let (_,temp1) = Max.maximize_it_with_care Rubber_list.common_length ll in  
      let temp2 = Image.image Rubber_list.unveil temp1 in 
      let (temp3,temp4) = List.partition (fun (opt1,opt2)->opt2=None) temp2 in 
      let small_lists = Image.image (fun (opt1,opt2)->Option.unpack opt1) temp3 
      and increased_lists = Image.image (fun (opt1,opt2)->Option.unpack opt2) temp4 in 
      let full_list = il_fold_merge small_lists in 
      if increased_lists = [] 
      then Rubber_list.of_list full_list 
      else   
      let defn =  Merger (full_list,increased_lists) in 
      Rubber(Rubber_core_list.find_from_definition defn,[])  ;; 
  
    let eval selector l =  
          match selector with 
          Passive_repeat_selector(width,b) -> 
            apply_passive_repeat width b (List.hd l)
          | Boundary_increment_selector(width,breadth,n) ->
            apply_boundary_increment width breadth n (List.hd l)
           | Fork_selector ->     
            apply_fork l 
           | Jump_selector -> List.hd l;;
  
  end ;;  
  
  

module Hungarian = struct 

module Breadth_reduction = struct 

let test_for_automatic_treatment width scrappers b = 
  List.exists(fun t->List.mem t scrappers) 
  [b;b+width;b+2*width] ;;

let rec effective_breadth (width,scrappers,b) =
   if (test_for_automatic_treatment width scrappers b)&&(b>0) 
   then effective_breadth (width,scrappers,b-1)
   else b ;;

end ;;  

let normalized_adjust adj =
    if adj = [] 
    then Leave_unchanged 
    else Adjust adj ;;


let short_adjust old_result adjustment =
      match adjustment with 
      Leave_unchanged -> old_result 
      |Adjust extra -> Rubber_list.remove_fixed_part_on_all extra old_result;;
  
let adjust result_opt adjustment=
    match result_opt with 
    None -> None 
    | Some old_result ->  
    Some(short_adjust old_result adjustment) ;;   

let merge_adjustments adj1 adj2 =
  (
    match adj1 with 
      Leave_unchanged -> adj2 
     |Adjust content1 -> 
      (
      match adj2 with 
      Leave_unchanged -> adj1 
     |Adjust content2 -> Adjust(i_merge content1 content2) 
      
  )
  ) ;;

let first_step_in_decomposition (width,breadth,scrappers) =
     let bound = breadth+2*width in 
     let (below,above) = (if width<>1
      then (scrappers,[])
      else List.partition (fun t->t<=bound) scrappers) in 
     ((width,Breadth_reduction.effective_breadth (width,scrappers,breadth),below),
       normalized_adjust above) ;;     

let pusher_for_decomposition (triple,adj)= 
      let (triple2,adj2) = first_step_in_decomposition triple in 
      (triple2,merge_adjustments adj adj2) ;;

let rec iterator_for_decomposition  pair =
   let next_pair = pusher_for_decomposition pair in 
   if next_pair = pair 
   then pair 
   else iterator_for_decomposition next_pair ;;

let decompose triple =  iterator_for_decomposition (triple,Leave_unchanged) ;;  

(* Example : decompose (1,18,[8;11;14;17;20]);; *)

end ;;  



module Parametrized = struct 

let eval_uniform_subrange usr n =
  List.filter (
     fun k->
      if i_mem k usr.Sz_types.usr_negative_exceptions then false else  
      if i_mem k usr.Sz_types.usr_positive_exceptions then true  else 
      i_mem (k mod usr.Sz_types.usr_modulus)
      usr.Sz_types.usr_usual
  ) (Int_range.range 1 n) ;; 

let eval_subrange sr n =
   match List.assoc_opt n sr.Sz_types.ps_exceptions with 
   Some answer -> answer 
   | None ->
    eval_uniform_subrange sr.Sz_types.ps_usual n  ;;

let eval_ps_list psl n =
  match List.assoc_opt n psl.Sz_types.pl_exceptions with 
  Some answer -> Short_list(answer) 
  | None ->
   Short_list(Image.image (fun sr->eval_subrange sr n) psl.Sz_types.pl_usual) ;;    

let eval_level_two (Quick l) scrappers n =
  let z = concretize (n,scrappers) in 
  if (not(i_is_included_in l z))  
  then Short_list([z]) 
  else 
  let temp1 = List.rev_map (fun t->i_setminus z [t]) l in 
  Short_list(il_sort temp1) ;;     

end ;;   


module Parametrized_Example = struct 

  let uniform_subrange pe ne mdl usu = {
    Sz_types.usr_positive_exceptions = pe ;
    usr_negative_exceptions = ne ; 
    usr_modulus = mdl;
    usr_usual = usu ;
  };; 
  
  let subrange (sr_exns,pe,ne,mdl,usu) = {
    Sz_types.ps_exceptions = sr_exns ;
    ps_usual = uniform_subrange pe ne mdl usu ;
  };; 
  
  let ps_list psl_exns psl_usu = {
    Sz_types.pl_exceptions = psl_exns ;
    pl_usual = Image.image subrange psl_usu ;
  };; 

  let example1 = Quick [1;2;3] ;;

  let example2 (* for (1,2,[]) *) = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
     ]
     [
      ([],[],[3],1,[0]);
      ([],[],[2],1,[0]);
     ] ;;  
     
  let example3 (* for (1,3,[]) *) = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
     ]
     [
      ([],[],[3],1,[0]);
     ] ;;    
     
  let example4 (* for (1,4,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
     ]
     [
      ([],[],[3;6],1,[0]);
      ([],[],[3;5],1,[0]);
      ([],[],[3;4],1,[0]);
      ([],[],[2;5],1,[0]);
      ([],[],[2;4],1,[0]);
      ([],[],[1;4],1,[0]);
     ] ;;   
     
   let example5 (* for (1,5,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
       6,[[1;2;4;5];[1;2;4;6];[1;2;5;6];
          [1;3;4;6];[1;3;5;6];[2;3;5;6]]
     ]
     [
      ([],[],[3;6],1,[0]);
      ([],[],[3;5],1,[0]);
      ([],[],[2;5],1,[0]);
     ] ;;      

    let example6 (* for (1,6,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
       6,[[1;2;4;5];[1;2;4;6];[1;2;5;6];
          [1;3;4;6];[1;3;5;6];[2;3;5;6]];
       7,[[1;2;4;5;7];[1;2;4;6;7];[1;3;4;6;7]]   
     ]
     [
      ([],[],[3;6],1,[0]);
     ] ;;    

     let example9 (* for (1,2,[7]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
     ]
     [
      ([],[],[3;5],1,[0]);
      ([],[],[2;5],1,[0]);
     ] ;;  
     

  end ;;   
  


let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;



let nonhungarian_enhance_getter old_getter pt =
    let (width,breadth,n,scrappers) = Point.unveil pt in  
    let z = concretize (n,scrappers) in 
    if ((width,breadth)=(1,0))||(test_for_admissiblity width breadth z) 
    then Some (Short_list[z]) 
    else 
    match Hashtbl.find_opt rose_hashtbl (width,breadth) with 
    Some summary -> Some (Parametrized.eval_level_two summary scrappers n)
    | None ->  
    (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
     Some summary -> Some (Parametrized.eval_ps_list summary n)
     | None -> old_getter pt) ;;   

let hungarian_enhance_getter old_getter pt = 
  let (width,breadth,n,scrappers) = Point.unveil pt in  
  let ((width2,breadth2,scrappers2),adj) = 
    Hungarian.decompose (width,breadth,scrappers) in 
  let pt2 = P(width2,breadth2,n,scrappers2) in   
  let res_opt = nonhungarian_enhance_getter old_getter pt2 in  
    Hungarian.adjust res_opt adj;;

let low_getter = Accumulator_with_optional_anticipator.get_from_low_hashtbl 
  ~with_anticipation:false ;;    
let access = hungarian_enhance_getter low_getter ;;   

let descendants_for_tool pt tool = 
       let (width,breadth,n,scrappers) = Point.unveil pt in  
       match tool with 
       Passive_repeat -> [P(width,breadth-1,n,scrappers)]     
      | Boundary_increment ->
       let (m,new_scrappers) = remove_one_element (n,scrappers) n in  
       [P(width,breadth,m,new_scrappers)]
      | Fork ->     
          Int_range.scale (fun k->
             let (m,scr) = remove_one_element  (n,scrappers)  (breadth+k*width) in 
             P(width,breadth-1,m,scr)
           ) 0 2 
      | Jump -> [P(width-1,n-2*(width-1),n,scrappers)] ;;


let try_tool_quickly old_getter pt tool = 
   let (width,breadth,n,scrappers) = Point.unveil pt in  
   let nh_enhanced_getter = nonhungarian_enhance_getter old_getter in 
   let descendants = descendants_for_tool pt tool 
   and selector = Selector_for_hook.of_hook (width,breadth,n) tool in 
   let hungarian_descendants = Image.image (
      fun pt1  ->
        let (w,b,m,s) = Point.unveil pt1 in  
        let ((w2,b2,s2),adj) =  Hungarian.decompose (w,b,s) in 
        (P(w2,b2,m,s2),adj)
    ) descendants in 
   let temp1 = Image.image (fun (pt3,adj)->
      (pt3,Hungarian.adjust (nh_enhanced_getter pt3) adj)
  ) hungarian_descendants in   
  let (failures,successes) = List.partition (
          fun (_,opt) -> opt = None
  ) temp1 in 
  let missing_data = Image.image fst failures in 
  if missing_data <> [] then (missing_data,Short_list []) else 
  let args = Image.image (fun (_,opt)->Option.unpack opt) successes in 
  ([],Selector_for_hook.eval selector args) ;;  


exception Compute_from_below_exn of point ;;  

let compute_from_below old_getter pt tool =
   let (missing_data,result) = 
     try_tool_quickly old_getter pt tool in 
   if result = Short_list [] 
   then raise(Compute_from_below_exn(pt)) 
   else  result ;; 

let low_add pt tool =
   let finder = Accumulator_with_optional_anticipator.get_from_low_hashtbl ~with_anticipation:false in  
   let res = compute_from_below finder pt tool in  
   let _ = Accumulator_with_optional_anticipator.add_to_low_hashtbl  ~with_anticipation:false pt res in 
   res ;;

let med_add (width,breadth,scrappers) summary = 
  Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;

let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;;  
 

let find_remote_stumbling_block_or_immediate_working_tool 
       old_getter pt = 
   let hg_enhanced_getter = hungarian_enhance_getter old_getter in     
   match hg_enhanced_getter pt with 
    Some old_answer -> ([],None) 
    | None ->
   let (width,breadth,n,scrappers) = Point.unveil pt in     
   if breadth=0 
   then let (missing_data0,result_opt0) = 
        try_tool_quickly old_getter pt Jump in 
        if result_opt0<>(Short_list [])
        then ([],Some Jump)
        else (missing_data0,None)    
   else      
   let (missing_data1,result_opt1) = 
    try_tool_quickly old_getter pt Passive_repeat in 
   if result_opt1<>(Short_list []) then ([], Some Passive_repeat) else  
   if missing_data1<>[] then (missing_data1,None) else  
   let (missing_data2,result_opt2) = 
    try_tool_quickly old_getter pt Boundary_increment in 
   if result_opt2<>(Short_list []) then ([], Some Boundary_increment) else  
   if missing_data2<>[] then (missing_data2,None) else  
   let (missing_data3,result_opt3) = 
    try_tool_quickly old_getter pt Fork in 
   if result_opt3<>(Short_list []) then ([], Some Fork) else  
    (missing_data3,None) ;;
    


  
exception Pusher_exn ;;

let rec pusher_for_recursive_computation to_be_treated= 
   let carrier_get = Accumulator_with_optional_anticipator.get_from_low_hashtbl 
   ~with_anticipation:true in 
    match to_be_treated with 
    [] -> raise(Pusher_exn)
    | pt :: others -> 
       let (missing_data,opt_res) =
      find_remote_stumbling_block_or_immediate_working_tool 
      carrier_get pt in 
      match opt_res with 
       Some tool ->
           (*
           let _ = Accumulator_with_optional_anticipator.add_to_low_hashtbl
           ~with_anticipation:true pt tool in 
           *)
           others
       | None -> 
         if missing_data = [] 
         then others 
         else missing_data @ (pt::others) 
      ;;      
         
let rec born_to_fail_for_recursive_computation walker=
  born_to_fail_for_recursive_computation
  (pusher_for_recursive_computation walker)  ;;     

let  needed_subcomputations_for_several_computations uples = 
  let _ = ( Accumulator_with_optional_anticipator.low_anticipator:=[]) in  
  try born_to_fail_for_recursive_computation uples with 
  Pusher_exn -> !( Accumulator_with_optional_anticipator.low_anticipator) ;; 

let needed_subcomputations_for_single_computation pt = 
  needed_subcomputations_for_several_computations [pt] ;; 


let compute_recursively width breadth (n,scrappers) = 
  let uple = P(width,breadth,n,scrappers) in 
  let needed_carrier = needed_subcomputations_for_single_computation uple in 
  let answer = List.assoc_opt uple needed_carrier in 
  (answer,needed_carrier) 
;;  




let feel_new_line (width,breadth,scrappers) =
  let temp1 = Int_range.scale 
    (fun n->P(width,breadth,n,scrappers)) 1  (width+2*breadth) in 
  let temp2 = needed_subcomputations_for_several_computations temp1 in 
  let temp3 = Image.image (fun (P(w,b,n,s),_)->(w,b,s)) temp2 in 
  Ordered.sort t_order temp3 ;; 

let exhaust_new_line (width,breadth,scrappers) = 
  let carrier_get = Accumulator_with_optional_anticipator.get_from_low_hashtbl 
  ~with_anticipation:true in 
    let temp1 = Int_range.scale 
      (fun n->P(width,breadth,n,scrappers)) 1  30 in 
    let carrier = needed_subcomputations_for_several_computations temp1 in 
    let temp2 = Image.image (fun pt-> 
      let mutilated_carrier = List.filter (
        fun p->fst(p)<>pt
      ) carrier in 
      let _ = ( Accumulator_with_optional_anticipator.low_anticipator:=mutilated_carrier) in 
      let (_,hook_opt) = find_remote_stumbling_block_or_immediate_working_tool carrier_get pt in 
      (Point.size pt,hook_opt)
    ) temp1 in 
    let selector = (fun l->Option.filter_and_unpack  (fun (n,pair_opt)->match pair_opt with 
      None -> None |Some pair ->Some(n,pair)) l) in 
    let temp3 = selector temp2 in 
    let temp4 = Int_range.scale (fun n-> 
       let pt2 = P(width,breadth,n,scrappers) in 
       let _ = ( Accumulator_with_optional_anticipator.low_anticipator:=carrier) in 
      (n, hungarian_enhance_getter carrier_get pt2 ))  1 30  in 
    let temp5 = selector temp4 in 
    (temp3,temp5) ;;   




rose_add (1,1) Parametrized_Example.example1 ;; 


med_add (1,2,[]) Parametrized_Example.example2 ;; 
med_add (1,3,[]) Parametrized_Example.example3 ;; 


med_add (1,4,[])  Parametrized_Example.example4 ;; 
med_add (1,5,[])  Parametrized_Example.example5 ;; 
med_add (1,6,[])  Parametrized_Example.example6 ;; 

(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;
let g1 = needed_subcomputations_for_single_computation (P(4,0,8,[])) ;;

*)