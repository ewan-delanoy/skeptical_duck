(*

#use"lib/Szemeredi/sz4_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz4 is short for "fourth stab at Szemeredi problem".

*)

type width = Sz4_types.width = W of int ;; 

type finite_int_set = Sz4_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz4_types.constraint_t = C of int list;; 

type fan = Sz4_types.fan = F of int list list ;; 

type point = Sz4_types.point = {
    base_set : finite_int_set;
    max_width: width;
    added_constraints: constraint_t list
} ;;

type mold = Sz4_types.mold = {
    solutions : (int list) list;
    mandatory_elements : int list;
} ;;

type explanation = Sz4_types.explanation = 
   Free
  |Extension 
  |Filled_complement of int list 
  |Decomposition of finite_int_set * finite_int_set * (int list) 
  |Breaking_point of int * int * int 
  |Width_one_expl ;; 


let i_order = Total_ordering.for_integers ;;
let i_does_not_intersect = Ordered.does_not_intersect i_order ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersect = Ordered.intersect i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_length_preserving_sort = Ordered.length_preserving_sort i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_insert = Ordered.insert il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_mem = Ordered.mem il_order ;;
let il_min= Ordered.min il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;




let order_for_triples = ((fun (W w1,scr1,b1) (W w2,scr2,b2) ->
  let trial1 = i_order w1 w2 in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else 
  let trial2 = i_order (List.length scr2) (List.length scr1) in 
  if trial2<>Total_ordering_result_t.Equal then trial2 else
  let trial3 = Total_ordering.silex_for_intlists scr1 scr2 in 
  if trial3<>Total_ordering_result_t.Equal then trial3 else
    Total_ordering.for_integers b1 b2
): (width * int list * int) Total_ordering_t.t);;

module Constraint = struct 


let cleanup_list unchecked_list base =
 let temp1 = List.filter_map (fun (C cstr)-> 
   if i_is_included_in cstr base 
   then Some(cstr)
   else None
  ) unchecked_list in 
 let temp2 = il_sort(Ordered_misc.minimal_elts_wrt_inclusion temp1) in 
 Image.image (fun cstr -> (C cstr) )   temp2 ;;

let is_an_arithmetic_progression (C l) =
     if List.length(l)<>3 then false else 
     ((List.nth l 1)-(List.nth l 0))=((List.nth l 2)-(List.nth l 1));;


let is_weaker_than (C cstr_weak) (C cstr_strong) = i_is_included_in cstr_strong cstr_weak ;; 

let select_in_list  l_cstr candidates =  
   List.filter (fun candidate->
    List.for_all( fun (C cstr) ->not(i_is_included_in cstr candidate)) l_cstr ) 
     candidates;;


let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;



end ;;  

module Highest_constraint = struct

   module Private = struct

  let rec for_exact_positive_width (W w) 
     domain to_be_treated =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if (i_is_included_in [p-2*w;p-w] domain) 
       then Some (C[p-2*w;p-w;p])
       else for_exact_positive_width (W w) domain others ;;    
       
  let for_exact_width (W w) domain =
     if w<1 then None else for_exact_positive_width (W w) domain (List.rev domain) ;;

  
  let rec below_maximal_width (W w) domain =
        match for_exact_width (W w)  domain with 
        Some (cstr) -> Some(cstr)
        |None ->
           if w<2 then None else 
           below_maximal_width (W (w-1)) domain ;;       
  
   let effective_max_width base_set proposed_width =
     match below_maximal_width proposed_width  base_set with 
     None -> W 0
     |Some(cstr) -> Constraint.width cstr ;; 

   let compute_max_width_without_help base_set =
       let a=List.hd base_set and b=List.hd(List.rev base_set) in 
       effective_max_width base_set (W((b-a)/2)) ;;  

  end ;;

  let below_maximal_width = Private.below_maximal_width ;;

  let effective_max_width ?(proposed_width_opt=None) base_set =
     match proposed_width_opt with 
     None -> Private.compute_max_width_without_help base_set 
     |Some(proposed_width) ->  Private.effective_max_width base_set proposed_width ;; 
  
  

  end ;;

module Finite_int_set = struct 

  exception Translation_goes_negative of int * finite_int_set ;; 

  exception Empty_set_has_no_diameter ;;

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let empty_set = FIS(0,[]) ;;

  let of_usual_int_list domain =
       if domain = [] then empty_set else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  let translation_goes_negative d = function 
    [] -> false
    | m :: _ -> m+d<0 ;;      

  let remove_element fis k=
    let (FIS(n,scrappers)) = fis in 
    if (k>n)||(k<1) then fis else 
    let new_scrappers = i_insert k scrappers in 
    if k <> n then FIS(n,new_scrappers) else 
    if scrappers = Int_range.range 1 (n-1)
    then empty_set
    else   
    let new_z =  to_usual_int_list (FIS(n-1,scrappers)) in 
    let new_max = List.hd(List.rev new_z) in 
    FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;         

  
  let diameter fis = 
   let (FIS(n,_scr)) = fis in 
   let l = to_usual_int_list fis in 
   if l = [] then raise Empty_set_has_no_diameter else 
   let m = List.hd(to_usual_int_list fis) in 
   n-(m-1);;   
  
   let is_connected fis = 
      let (FIS(_n,scr)) = fis in 
      match scr with 
      [] -> true 
      |min_scr :: others ->
         if min_scr<>1 then false else 
         (
            match (List.rev others) with 
            [] -> true 
            |max_scr :: _ -> 
              scr = Int_range.range min_scr max_scr 
         )   ;;
         
  end ;;

  let constraint_can_apply (FIS(n,scrappers)) (C l) =
     List.for_all(fun t->(1<=t)&&(t<=n)&&(not(List.mem t scrappers))) l ;; 

  let constructor n scrappers =
      let domain = i_setminus (Int_range.range 1 n) scrappers in 
      Private.of_usual_int_list domain ;; 

  let decompose_wrt_translation fis_domain = 
    let domain = Private.to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,Private.of_usual_int_list core_domain) ;; 

   let diameter = Private.diameter ;; 

  let effective_max_width ?(proposed_width_opt=None) fis= 
       Highest_constraint.effective_max_width 
       ~proposed_width_opt (Private.to_usual_int_list fis)  ;; 

  let empty_set = Private.empty_set ;;

  let interval i j = FIS(j,Int_range.range 1 (i-1)) ;;
  let is_connected = Private.is_connected ;;
  let max (FIS(n,_)) = n ;; 

  let name (FIS(n,l)) =
     "FIS("^(string_of_int n)^",["^
     (String.concat ";" (Image.image string_of_int l))
     ^"])";;

  let of_usual_int_list = Private.of_usual_int_list ;; 

  let order = ((fun (FIS(n1,scr1)) (FIS(n2,scr2)) ->
    let trial1 = i_order n1 n2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    let trial2 = i_order (List.length scr2) (List.length scr1) in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else
      Total_ordering.silex_for_intlists scr1 scr2
  ): finite_int_set Total_ordering_t.t);;

  let remove fis vertices_to_be_removed = 
    let old_list = Private.to_usual_int_list fis in 
    let vertices_in_order = i_sort vertices_to_be_removed in 
    let new_list = i_setminus old_list vertices_in_order in 
    Private.of_usual_int_list new_list ;; 

    
  (*
  
  remove (FIS(10,[3;7;8;9])) [10] ;;
  remove (FIS(3,[])) [3] ;;
  remove (FIS(1,[])) [1] ;;
  remove (FIS(1,[])) [4] ;;

  *)

  let size (FIS(n,scr)) = n-(List.length scr);; 

  let to_usual_int_list = Private.to_usual_int_list ;; 

  let translate d fis = 
    let domain = Private.to_usual_int_list fis in 
    if Private.translation_goes_negative d domain 
    then raise(Translation_goes_negative(d,fis))   
    else  
    let translated_domain =  Image.image (fun t->t+d) domain in 
    Private.of_usual_int_list translated_domain;; 

end ;;    



module Point = struct

  (*

   UPDATE : the excluded_full_constraints field has been removed.

  The record { 
    base_set:b, 
    max_width:w, 
    excluded_full_constraints: efc,
    added_constraints: apc
  } 
  represents the hypergraph (b,H) 
  where H=(Sz(b,w) \ E) u A= (Sz(b,w) u A) \ E 
  where 
  - Sz(b,w) is the set of all arithmetic progressions 
  inside b with width <=w (in particular, this set is 
  empty when w = 0)
  - A is a set of added constraints.
  - E is a set of excluded arithmetic progressions inside
  b  

  *)

  exception Excessive_forcing of point * int list ;; 
  exception Exclusion_not_implemented of point * constraint_t ;;

  module Private = struct
  
  
  let constructor 
     ?(max_width_opt=None) base 
     ~added_constraints = 
      let checked_max_width = Finite_int_set.effective_max_width 
         base ~proposed_width_opt:max_width_opt in 
      let base_list = Finite_int_set.to_usual_int_list base in 
      let checked_added_constraints = Constraint.cleanup_list added_constraints base_list in 
      {
        base_set = base;
        max_width = checked_max_width;
        added_constraints = checked_added_constraints;
     } ;; 
  

  
   
  let remove pt vertices_to_be_removed =
    let new_base = Finite_int_set.remove pt.base_set vertices_to_be_removed 
    and selector = List.filter (
        fun (C l) -> i_does_not_intersect l vertices_to_be_removed
    ) in 
    let new_added_pcs = selector pt.added_constraints in  
    constructor new_base 
     ~max_width_opt:(Some pt.max_width) 
       ~added_constraints:new_added_pcs;; 

  let left_complementary_pairs domain (W max_w) j = 
    let v_max = List.hd(List.rev domain) in 
    let upper_bound = min max_w ((v_max-j)/2) in 
    let range = Int_range.range 1 upper_bound in 
    List.filter_map (
         fun w->
            let u = j+w and v=j+2*w in 
           if i_is_included_in [u;v] domain 
           then Some([u;v]) 
           else None  
     ) range ;; 

  let middle_complementary_pairs domain (W max_w) j = 
    let v_min = List.hd domain 
    and v_max = List.hd(List.rev domain) in 
    let upper_bound = min max_w (min (j-v_min) (v_max-j)) in 
    let range = Int_range.range 1 upper_bound in 
    List.filter_map (
         fun w->
            let u = j-w and v=j+w in 
           if i_is_included_in [u;v] domain 
           then Some([u;v]) 
           else None  
     ) range ;; 

  let right_complementary_pairs domain (W max_w) j = 
    let v_min = List.hd domain  in 
    let upper_bound = min max_w ((j-v_min)/2) in 
    let range = Int_range.range 1 upper_bound in 
    List.filter_map (
         fun w->
            let u = j-2*w and v=j-w in 
           if i_is_included_in [u;v] domain 
           then Some([u;v]) 
           else None  
     ) range ;; 

  let complementary_pairs domain w j =
    il_sort(
    (left_complementary_pairs domain w j)@
    (middle_complementary_pairs domain w j)@
    (right_complementary_pairs domain w j) 
    );; 

  let other_complements pt j =
    il_sort(List.filter_map (
         fun (C l)->
           if i_mem j l 
           then Some(i_outsert j l) 
           else None  
     ) pt.added_constraints) ;; 

  
  let translate t pt =   
     let new_base = Finite_int_set.translate t pt.base_set in 
     let new_partial = Image.image (fun (C l) ->
        C(Image.image ((+) t) l)
     ) pt.added_constraints
     in  
     {
        base_set = new_base;
        max_width = pt.max_width;
        added_constraints = new_partial;
     } ;; 
   
   let subset_is_admissible pt subset =
      ((Highest_constraint.below_maximal_width 
       pt.max_width subset) 
       =None) && 
       (List.for_all (fun (C l)->
         not(i_is_included_in l subset)
       ) pt.added_constraints);;

    let complements pt j = 
     let domain = Finite_int_set.to_usual_int_list pt.base_set 
     and w = pt.max_width in 
     il_merge
     (complementary_pairs domain w j)
     (other_complements pt j) ;; 

   exception Excessive_forcing_reached ;; 

   let force_one_vertex pt vertex_to_be_forced =
    let new_base = Finite_int_set.remove pt.base_set [vertex_to_be_forced] in 
    let new_added_pcs = complements pt vertex_to_be_forced in  
    if List.mem [] new_added_pcs 
    then raise(Excessive_forcing_reached)
    else 
    let (automatic,non_automatic) = 
      List.partition (fun l->(List.length l)=1 ) new_added_pcs in 
    let mandatory_elements = Image.image List.hd automatic 
    and retained_added_pcs = 
       List.filter_map(fun ( C l)-> 
         if not(i_mem vertex_to_be_forced l) then Some l else None) 
       pt.added_constraints in 
    let final_added_pcs = Image.image(fun l-> C l) 
       (il_merge retained_added_pcs non_automatic) in 
    let draft = constructor new_base 
     ~max_width_opt:(Some pt.max_width) 
       ~added_constraints:final_added_pcs in 
    if mandatory_elements = []
    then draft
    else remove draft mandatory_elements;;

  let restrict pt l = 
     let base = Finite_int_set.to_usual_int_list pt.base_set in 
     remove pt (i_setminus base l) ;; 

   let force pt vertices_to_be_forced = 
     try List.fold_left force_one_vertex pt vertices_to_be_forced with 
     Excessive_forcing_reached ->   
       raise(Excessive_forcing(pt,vertices_to_be_forced)) ;; 
 
   let decide pt decisions = 
     let (paired_forcings,paired_removals) = List.partition snd decisions in 
     let forcings = Image.image fst paired_forcings 
     and removals = Image.image fst paired_removals in 
     force (remove pt removals) forcings ;;

  let order_on_points_containing_1 pt1 pt2 =
   let (W w1) = pt1.max_width 
   and (W w2) = pt2.max_width in 
   let trial1 = Total_ordering.for_integers w1 w2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   let (FIS(n1,scr1)) = pt1.base_set 
   and (FIS(n2,scr2)) = pt2.base_set in 
   let trial2 = Total_ordering.for_integers n1 n2 in 
   if trial2 <> Total_ordering_result_t.Equal then trial2 else 
   let trial3 = Total_ordering.silex_for_intlists scr2 scr1 in 
   if trial3 <> Total_ordering_result_t.Equal then trial3 else  
   let ill_order = Total_ordering.silex_compare il_order in  
   let apc1 = Image.image (fun (C l)->l) pt1.added_constraints 
   and apc2 = Image.image (fun (C l)->l) pt2.added_constraints in 
   ill_order apc1 apc2 ;;    
   
  let decompose_wrt_translation pt =
     let (d,_) = Finite_int_set.decompose_wrt_translation pt.base_set in 
     (d,translate (-d) pt) ;;  

  let order = ((fun pt1 pt2 ->
    let (d1,translated_pt1) = decompose_wrt_translation pt1 
    and (d2,translated_pt2) = decompose_wrt_translation pt2 in 
    let trial1 = order_on_points_containing_1 translated_pt1 translated_pt2 in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else
    Total_ordering.for_integers d1 d2  
  ) : point Total_ordering_t.t) ;; 

  let three_term_arithmetic_progression_opt (C l) =
    if List.length(l)<>3 then None else 
    let nt = (fun k ->List.nth l (k-1)) in 
    let d1 =nt(2)-nt(1) and d2 =nt(3)-nt(2) in 
    if (d1<>d2)||(d1<=0) then None else 
    Some(nt(1),d1) ;;      
    
  let exclude pt cstr = 
   let old_ac = pt.added_constraints in 
    if List.mem cstr pt.added_constraints
    then {pt with 
     added_constraints = List.filter(fun t->t<>cstr) old_ac}
    else 
    match three_term_arithmetic_progression_opt cstr with 
    None -> pt 
    |(Some(start,ratio)) ->
        let (C l) =cstr 
        and base = Finite_int_set.to_usual_int_list pt.base_set 
        and (W w) = pt.max_width in 
        if not(i_is_included_in l base)||(ratio>w) then pt else
        if ratio <> w then raise(Exclusion_not_implemented(pt,cstr)) else
        let n = Finite_int_set.max pt.base_set in 
        let upper_constraints = List.filter_map (
          fun i->
            let l2 = [i;i+w;i+2*w] in 
            if not(i_is_included_in l2 base)||(i=start)
            then None   
            else Some (C l2)
        )(Int_range.range (List.hd base) n) in 
        {
          pt with
        max_width = (W(w-1));
        added_constraints = upper_constraints;
        } ;; 
    


  end ;;

  
  let complements = Private.complements ;; 

  let constraint_can_apply pt cstr =
     if List.mem cstr pt.added_constraints 
     then true 
     else 
     if not(Constraint.is_an_arithmetic_progression cstr)
     then false 
     else 
     Finite_int_set.constraint_can_apply pt.base_set cstr ;; 
  

  let constructor = Private.constructor ;; 
  
  let decide_on_the_left pt l = 
    let indexed_l = Int_range.index_everything l in 
    let pairs = List.filter_map(fun (v,tag)->
     if tag<0 then None else 
     Some(v,tag<>0)
    ) indexed_l in 
    Private.decide pt pairs ;;
 
  let decide_on_the_right pt l = 
    let n = Finite_int_set.max pt.base_set in 
    let indexed_l = Int_range.index_everything l in 
    let pairs = List.filter_map(fun (v,tag)->
     if tag<0 then None else 
     Some(n+1-v,tag<>0)
    ) indexed_l in 
    Private.decide pt pairs ;;

  let decompose_wrt_translation = Private.decompose_wrt_translation ;;  

  let exclude = Private.exclude ;;

  let force = Private.force ;; 
 
  let highest_constraint_opt pt =
    if pt.added_constraints <> []
    then Some(List.hd(List.rev(pt.added_constraints)))
    else 
    let (W w) = pt.max_width in 
    if w < 1 then None else  
    let domain = Finite_int_set.to_usual_int_list pt.base_set in 
    Highest_constraint.below_maximal_width 
       pt.max_width domain ;;
 

  let is_free pt = ((highest_constraint_opt pt) =None );;

  let usual ?max_width n scrappers = 
    let max_width_opt = 
      Option.map (fun w->W w) max_width in   
    constructor (FIS(n,scrappers)) 
     ~max_width_opt
       ~added_constraints:[];;
  let order = Private.order ;;
  let remove = Private.remove ;;

  let restrict = Private.restrict ;;  

  let size pt = Finite_int_set.size pt.base_set ;; 

  let subset_is_admissible = Private.subset_is_admissible ;;

  let translate = Private.translate ;;     

  end ;; 


module Mold = struct 

  exception Incorrect_extra_solutions of int *((int list) list) ;;

  module Private = struct

  let add_carefully extra_sols old_sols = 
    let m = List.length(List.hd old_sols) in 
    let bad_extra_sols = List.filter (
       fun z-> (List.length z)<> m 
    ) extra_sols in 
    if bad_extra_sols <> []
    then raise(Incorrect_extra_solutions(m,bad_extra_sols))
    else il_merge (il_sort extra_sols) old_sols ;; 
  
  end ;;

  let add_solutions mold extra_sols = 
      {
         mold with 
         solutions = Private.add_carefully extra_sols mold.solutions
      } ;;

  let in_decomposition_case mold1 mold2 full_sol extra_solutions =
  {
             solutions = Private.add_carefully extra_solutions [full_sol];
             mandatory_elements = 
             i_merge
             (mold1.mandatory_elements)
             (mold2.mandatory_elements)
  } ;; 

  let in_extension_case extended_sols beheaded_mold n =
  {
             solutions = extended_sols;
             mandatory_elements = 
             (beheaded_mold.mandatory_elements)@[n]
  } ;; 

  let in_fork_case molds extra_solutions =
    let last_mold = List.hd(List.rev molds) in 
    
   { 
     solutions  = Private.add_carefully extra_solutions last_mold.solutions; 
     mandatory_elements = i_fold_intersect (Image.image
      (fun mold -> mold.mandatory_elements) molds
   ); 
   } ;;

 let in_free_case pt =
    let base = Finite_int_set.to_usual_int_list pt.base_set in 
   { 
   solutions  = [base]; 
   mandatory_elements = base; 
   } ;;      

 let in_stagnation_case beheaded_mold =
  {
         solutions = beheaded_mold.solutions;
         mandatory_elements = []
  };;

 let sum mold1 mold2 =
     {
       solutions = il_merge mold1.solutions mold2.solutions ;
       mandatory_elements = i_intersect 
            mold1.mandatory_elements mold2.mandatory_elements   
     } ;; 

 let solution_size mold = List.length(List.hd mold.solutions) ;;   

 let successful_append forced_mold n =
     {
       solutions=Image.image(fun sol->sol@[n]) forced_mold.solutions;
       mandatory_elements=(forced_mold.mandatory_elements)@[n]
     } ;;


 let translate d mold = 
    let tr = Image.image ((+) d) in 
    { 
        solutions  = Image.image tr mold.solutions; 
        mandatory_elements = tr mold.mandatory_elements; 
    } ;;

end ;;

  
module Small_size_set = struct 


exception Size_too_big of point ;; 

module Private = struct 

let max_size = 18 ;; 

let memoized_all_realizations =Memoized.make(fun pt ->
      let base = Finite_int_set.to_usual_int_list pt.base_set in 
      let temp1 = il_sort(List_again.power_set base) in 
      List.filter (Point.subset_is_admissible pt) temp1)  ;; 

let all_realizations pt =
    if Point.size(pt) > max_size 
    then raise(Size_too_big(pt))
    else memoized_all_realizations pt ;;

let all_solutions pt offset =
      let realizations = all_realizations pt  in 
      let m = List.length(List.hd(List.rev realizations)) in 
      List.filter (fun y->List.length(y)=m-offset) realizations ;;  

let measure = Memoized.make(fun pt ->
   List.length(List.hd(List.rev (all_realizations pt)))
);;

let rec helper_for_solution_chooser (sols,base) =
     if List.length sols = 1 then List.hd sols else 
     match base with 
      [] -> failwith("Error in Small_size_set.helper_for_solution_chooser")
     |v::others ->
       let possibly_empty = List.filter (fun sol ->not(i_mem v sol)) sols in 
       let not_empty = (if possibly_empty = [] then sols else possibly_empty) in 
       helper_for_solution_chooser (not_empty,others) ;;

let canonical_solution = Memoized.make(fun pt->
   let base = List.rev(Finite_int_set.to_usual_int_list pt.base_set) 
   and sols = all_solutions pt 0 in 
   helper_for_solution_chooser (sols,base)
) ;; 

let eval_on_pt_with_1 pt = 
     let base = List.rev(Finite_int_set.to_usual_int_list pt.base_set) 
     and sols = all_solutions pt 0 in 
     {
       solutions = [helper_for_solution_chooser (sols,base)]; 
       mandatory_elements = i_fold_intersect sols
     } ;; 


let eval pt =
    let (d,pt_with_1) = 
      Point.decompose_wrt_translation pt in 
    Mold.translate d (eval_on_pt_with_1 pt_with_1);;

end ;;

let all_realizations = Private.all_realizations ;; 
    
let all_solutions = Private.all_solutions ;;  

let eval = Private.eval ;;  

let max_size = Private.max_size ;;


end ;;  

module Width_one = struct 
  
module Private = struct   
let on_interval i j = 
   let r = (fun t->(t-i+1) mod 3) 
   and whole = Int_range.range i j in 
   let global_state = r j in 
   let forced_remainders = 
      List.filter (fun t->t<=global_state) [1;2] in 
   {
     solutions = [List.filter(fun t->r(t)<>0) whole];
     mandatory_elements = List.filter(fun 
     t->List.mem (r(t)) forced_remainders) whole;
   } ;;

let on_arbitrary_set l =
   let components = Arithmetic_list.decompose_into_connected_components l in 
   let temp1 = Image.image (fun (i,j) -> on_interval i j) components in 
    
   {
     solutions = [
      List.flatten (Image.image (fun mold->
         List.hd(mold.solutions)) temp1)
     ];
     mandatory_elements = 
      List.flatten 
      (Image.image (fun mold->mold.mandatory_elements) temp1);
   } ;;

end ;;   

let eval fis = Private.on_arbitrary_set 
  (Finite_int_set.to_usual_int_list fis) ;;

end ;;   

module Util = struct 
   
let current_bound = 200 ;;

let width2_subset n = List.filter (fun k->List.mem(k mod 3)[1;2]) 
  (Int_range.range 1 n) ;;
let width3_subset n = List.filter (fun k->List.mem(k mod 8)[1;2;4;5]) 
  (Int_range.range 1 n) ;;

end ;;   


module Precomputed = struct 

module Private = struct 

let test_width_two_usual pt =
   let n=Finite_int_set.max pt.base_set in 
   pt=Point.usual ~max_width:2 n [] ;;

let width_two_usual n = 
    let expl = (
        if n<=2 then Free else 
        if n<=4 then Width_one_expl else    
        if (n mod 3)=0 
        then Filled_complement [n-2;n-1]
        else Extension
    ) in
    ({solutions =[Util.width2_subset n];
  mandatory_elements = 
    (
     if n=4 then [1;4] else 
     if n=5 then [1;4;5] else   
     if n mod 3 = 0 then [] else List.filter (fun x->
     (x>0)&&((x mod 3)>0) ) [n-1; n])},
    expl) ;;  

end ;;   


let eval_opt (pt:point) = 
   if (pt.max_width = W 1)&&(pt.added_constraints=[])
   then Some(Width_one.eval pt.base_set,Width_one_expl)  
   else 
   let n=Finite_int_set.max pt.base_set in    
   if Private.test_width_two_usual pt 
   then Some(Private.width_two_usual n)
   else None ;;  

end ;;   



module One_more_small_step = struct 

module Private = struct 

let impatient_ref = ref ([]: (point * mold) list) ;; 
let explanations_ref = ref ([]: (point * explanation) list) ;; 




let check_extension_case pt n beheaded_mold_opt = 
   match beheaded_mold_opt with 
    None -> None 
   |Some beheaded_mold -> 
   let extended_sols = List.filter_map (
       fun sol -> let extended_sol = sol @ [n] in 
         if Point.subset_is_admissible pt extended_sol 
         then Some extended_sol
         else None 
     ) beheaded_mold.solutions in 
     if extended_sols <> []
     then Some(Mold.in_extension_case extended_sols beheaded_mold n)
     else None ;; 

let check_filled_complement_case pt n beheaded_mold_opt = 
  match beheaded_mold_opt with 
    None -> None 
   |Some beheaded_mold ->
    (let complements = Point.complements pt n in 
     match List.find_opt (
        fun c-> i_is_included_in c 
        beheaded_mold.mandatory_elements
     ) complements with 
     (Some complement) ->
       Some(complement,Mold.in_stagnation_case beheaded_mold)
     | None -> None );;  


let lower_level_eval_on_pt_with_1_opt pt_with_1 = 
   if Point.is_free pt_with_1 
   then Some(Mold.in_free_case pt_with_1) 
   else 
   match Precomputed.eval_opt pt_with_1 with 
   (Some old_answer) -> Some (fst old_answer) 
   | None -> List.assoc_opt pt_with_1 (!impatient_ref) ;;

let lower_level_eval_opt pt =
    let (d,pt_with_1) = 
      Point.decompose_wrt_translation pt in 
    Option.map(Mold.translate d)
     (lower_level_eval_on_pt_with_1_opt pt_with_1);;

let add_explanation pt expl = 
     (explanations_ref := (pt,expl) :: (!explanations_ref));;

let prefilled_decomposers = [
   (FIS(7,[]),[1;2;4;5]);
   (FIS(8,[]),[1;2;4;5])
] ;;

let check_prefilled_decomposition pt_with_1 n (fis3,sol3)= 
   let n3 = Finite_int_set.max fis3 in 
   if n < n3 then None else
   let fis2 = Finite_int_set.translate (n-n3) fis3 
   and sol2 = Image.image (fun t->t+n-n3) sol3 in 
   let dom2 = Finite_int_set.to_usual_int_list fis2 
   and dom = Finite_int_set.to_usual_int_list pt_with_1.base_set in 
   let dom1 = i_setminus dom dom2 in 
   let fis1 = Finite_int_set.of_usual_int_list dom1 in 
   let pt1 = Point.remove pt_with_1 dom2 
   and pt2 = Point.restrict pt_with_1 dom2 in 
   let opt1 = lower_level_eval_opt pt1 
   and opt2 = lower_level_eval_opt pt2 in 
   if (opt1=None)||(opt2=None) then None else 
   let mold1 = Option.get opt1 and mold2 = Option.get opt2 in 
   if not(List.mem sol2 mold2.solutions) then None else 
   match List.find_map (
     fun sol1 -> let sol = i_merge sol1 sol2 in 
     if Point.subset_is_admissible pt_with_1 sol 
     then Some sol 
     else None
   ) mold1.solutions with 
   None -> None 
  |(Some sol)->
    let mold = Mold.in_decomposition_case mold1 mold2 sol [] in 
    Some(Decomposition(fis1,fis2,sol),mold)  ;;

let check_prefilled_decomposition_case pt_with_1 n = 
   List.find_map (
      check_prefilled_decomposition pt_with_1 n
   )  prefilled_decomposers ;;




let expand_pt_with_1_without_remembering_opt pt_with_1 =
   let opt0 = lower_level_eval_on_pt_with_1_opt pt_with_1 in 
   if opt0 <> None 
   then opt0
   else   
   let n = Finite_int_set.max (pt_with_1.base_set) in 
   let beheaded_pt = Point.remove pt_with_1 [n] in 
   let beheaded_mold_opt = lower_level_eval_on_pt_with_1_opt beheaded_pt in 
   let opt1 = check_extension_case pt_with_1 n beheaded_mold_opt in 
   if opt1 <> None
   then let _ = add_explanation pt_with_1 Extension in 
        opt1
   else 
   let opt2 = check_filled_complement_case pt_with_1 n beheaded_mold_opt in 
   if opt2 <> None
   then let (complement,mold) = Option.get opt2 in 
        let _ = add_explanation pt_with_1 (Filled_complement(complement)) in 
        Some mold
   else 
   let opt3 = check_prefilled_decomposition_case pt_with_1 n  in 
   if opt3 <> None
   then let (expl,mold) = Option.get opt3 in 
        let _ = add_explanation pt_with_1 expl in 
        Some mold
   else    
   None ;;

let eval_on_pt_with_1_and_remember_opt pt_with_1 = 
   let opt0 = lower_level_eval_on_pt_with_1_opt pt_with_1 in 
   if opt0 <> None 
   then opt0
   else 
    match expand_pt_with_1_without_remembering_opt pt_with_1 with 
    None -> None 
   |Some new_answer ->
        let _ = (
            impatient_ref := (pt_with_1,new_answer) :: 
          (!impatient_ref)) in 
        Some new_answer

   ;;      


let eval_and_remember_opt pt =
    let (d,pt_with_1) = 
      Point.decompose_wrt_translation pt in 
    Option.map(Mold.translate d)
     (eval_on_pt_with_1_and_remember_opt pt_with_1);;



let eval_without_remembering_opt pt =
    let (d,pt_with_1) = 
      Point.decompose_wrt_translation pt in 
    Option.map(Mold.translate d)
     (expand_pt_with_1_without_remembering_opt pt_with_1);;     

let eval_opt ?(without_remembering=false) pt = 
   if without_remembering 
   then eval_without_remembering_opt pt
   else eval_and_remember_opt pt ;;

let unsafe_add pt mold expl = 
      (impatient_ref := (pt,mold) ::(!impatient_ref);
       explanations_ref := (pt,expl) ::(!explanations_ref);
      ) ;;

let explanation_on_pt_with_1_opt pt_with_1 = 
   match Precomputed.eval_opt pt_with_1 with 
   (Some old_answer) -> Some (snd old_answer) 
   | None -> 
    if Point.is_free pt_with_1 then Some Free else  
   List.assoc_opt pt_with_1 (!explanations_ref) ;; 

let explanation_opt pt =  
   let (_,pt_with_1) = 
      Point.decompose_wrt_translation pt in 
   explanation_on_pt_with_1_opt  pt_with_1 ;;   

let explained_eval pt = 
   let opt = eval_opt ~without_remembering:false pt in 
   (Option.get opt,Option.get (explanation_opt pt)) ;;  

end ;;

let eval_opt = Private.eval_opt ;; 

let explained_eval = Private.explained_eval ;;

let explanation_opt = Private.explanation_opt ;;

let unsafe_add = Private.unsafe_add ;;

end ;;

module Deduce = struct 

exception Incorrect_constraint_in_fork_exn 
   of constraint_t  * point ;;

exception Incomplete_fork_exn of point ;;

module Private = struct 

exception Nonadmissible_whole_in_decomposition of (point * (int list));;
exception Nonadmissible_part_in_decomposition of (point * (int list));;
exception Badly_sized_part_in_decomposition of (point * (int list));;
exception Unknown_part_in_decomposition of point ;;



let check_part_in_decomposition pt fis sol = 
    let domain = Finite_int_set.to_usual_int_list fis in 
    let smaller_pt = Point.restrict pt domain in 
    match One_more_small_step.eval_opt smaller_pt with 
    None -> raise(Unknown_part_in_decomposition(smaller_pt))
    |Some mold ->
      let m = Mold.solution_size mold in 
      if not(Point.subset_is_admissible smaller_pt sol)
      then raise(Nonadmissible_part_in_decomposition(smaller_pt,sol))
      else 
      if List.length(sol)<>m
      then raise(Badly_sized_part_in_decomposition(smaller_pt,sol)) 
      else Mold.add_solutions mold [sol] ;; 

let deduce_using_decomposition 
   ?(extra_solutions=[]) pt (fis1,fis2,sol) = 
   let part1 = Finite_int_set.to_usual_int_list fis1 
   and part2 = Finite_int_set.to_usual_int_list fis2 in 
   let sol1 = i_intersect part1 sol 
   and sol2 = i_intersect part2 sol in 
   let mold1 = check_part_in_decomposition pt fis1 sol1 
   and mold2 = check_part_in_decomposition pt fis2 sol2 in 
   if Point.subset_is_admissible pt sol 
   then let mold = Mold.in_decomposition_case mold1 mold2 sol extra_solutions in 
        let _ = One_more_small_step.unsafe_add pt mold (Decomposition(fis1,fis2,sol)) in 
        mold
   else raise(Nonadmissible_whole_in_decomposition(pt,sol));;



end ;;

let using_decomposition = Private.deduce_using_decomposition ;;

let using_fork ?(extra_solutions=[]) pt cstr = 
    let (C l) = cstr in 
    if not(Point.constraint_can_apply pt cstr)
    then raise(Incorrect_constraint_in_fork_exn(cstr,pt))
    else
    let temp1 = Image.image (fun t->
       One_more_small_step.eval_opt(Point.remove pt [t])
    ) l in 
    let get_in_l = (fun k->List.nth l (k-1)) in 
    let m = List_again.find_index_of_in None temp1 in 
    if m > 0 
    then let p = get_in_l m in
         raise(Incomplete_fork_exn(Point.remove pt [p]))
    else 
    let molds = Image.image Option.get temp1 in 
    let mold = Mold.in_fork_case molds extra_solutions in 
    let _ = One_more_small_step.unsafe_add pt mold 
       (Breaking_point(get_in_l 1,get_in_l 2,get_in_l 3)) in 
    mold ;;

end ;;


module Painstaking = struct 

exception Initial_data_already_accessible ;;

module Private = struct 

let painstaking_ref = ref ([]: (point * mold) list) ;; 
let new_discoveries_ref = ref ([]: (point * mold) list) ;; 


let finalize_decomposition_computation 
        beheaded_pt forced_pt data n =
     let beheaded_mold = Option.get(List.assoc beheaded_pt data) 
     and pre_forced_mold = Option.get(List.assoc forced_pt data) in 
     let forced_mold = Mold.successful_append pre_forced_mold n in 
     let m0 = (Mold.solution_size beheaded_mold)
     and m1 = (Mold.solution_size forced_mold) in 
     if m1 > m0 (* implying m1 = m0 + 1 *)
     then forced_mold 
     else if m1 = m0
     then Mold.sum forced_mold beheaded_mold 
     else (* unknown case so far, but theoretically possible *)
          beheaded_mold ;;   

         
let lower_level_eval_on_pt_with_1_opt pt_with_1 = 
   if Point.is_free pt_with_1 
   then Some(Mold.in_free_case pt_with_1) 
   else 
   match List.assoc_opt pt_with_1 (!painstaking_ref) with 
  (Some old_answer) -> Some old_answer
  | None ->
    (match List.assoc_opt pt_with_1 (!new_discoveries_ref) with 
  (Some old_answer) -> Some old_answer
  | None ->
    if (Point.size pt_with_1)<= Small_size_set.max_size 
    then Some (Small_size_set.eval pt_with_1)
    else      
   (match One_more_small_step.eval_opt pt_with_1 with  
    (Some lower_level_answer) -> 
       let _ = (new_discoveries_ref := (pt_with_1,lower_level_answer) :: 
        (!new_discoveries_ref)) in
        Some lower_level_answer 
    |None -> None ));;   

(*
If we have reached the end of the list, we return the last
computed value (which corresponds to the initially asked value),
otherwise we store the result in new_discoveries_ref 
and keep on computing

*)

let rec eval_on_nonempty_list next_pt other_untreated_pts = 
   match lower_level_eval_on_pt_with_1_opt next_pt with 
  (Some next_mold) -> 
      (
        match other_untreated_pts with  
        second_next_pt :: other_pts ->
          eval_on_nonempty_list second_next_pt other_pts 
        | [] -> next_mold   
      )  
  | None ->
     let n = Finite_int_set.max (next_pt.base_set) in 
     let beheaded_pt = Point.remove next_pt [n] in 
     let forced_pt = Point.force next_pt [n] in
     let temp1 = Image.image (
          fun pt -> (pt,lower_level_eval_on_pt_with_1_opt pt)
     ) [beheaded_pt;forced_pt] in 
     let (known,unknown) = List.partition (
         fun (_pt,answer_opt) -> answer_opt <> None
     ) temp1 in 
     if unknown <> []
     then let untreated_pts = Image.image fst unknown in 
          eval_on_nonempty_list (List.hd untreated_pts) 
                     ((List.tl untreated_pts)@
                         (next_pt::other_untreated_pts))
     else  
     let next_mold = finalize_decomposition_computation 
        beheaded_pt forced_pt known n in 
     (
        match other_untreated_pts with  
        second_next_pt :: other_pts ->
          let _ = (new_discoveries_ref := (next_pt,next_mold) :: 
        (!new_discoveries_ref)) in
          eval_on_nonempty_list second_next_pt other_pts 
        | [] -> next_mold   
      )     
     ;;

let outer_eval pt_with_1 = 
   let  _ = (new_discoveries_ref:=[]) in 
   match lower_level_eval_on_pt_with_1_opt pt_with_1 with 
  (Some old_answer) -> old_answer
  | None ->
     let n = Finite_int_set.max (pt_with_1.base_set) in 
     let beheaded_pt = Point.remove pt_with_1 [n] in 
     let forced_pt = Point.force pt_with_1 [n] in
     let temp1 = Image.image (
          fun pt -> (pt,lower_level_eval_on_pt_with_1_opt pt)
     ) [beheaded_pt;forced_pt] in 
     let (known,unknown) = List.partition (
         fun (_pt,answer_opt) -> answer_opt <> None
     ) temp1 in 
     if unknown <> []
     then let untreated_pts = Image.image fst unknown in 
          eval_on_nonempty_list (List.hd untreated_pts) 
                     ((List.tl untreated_pts)@[pt_with_1])
     else  
     finalize_decomposition_computation 
        beheaded_pt forced_pt known n ;;



let eval_on_pt_with_1 pt_with_1 =
  match List.assoc_opt pt_with_1 (!painstaking_ref) with 
  (Some old_answer) -> old_answer
  | None ->
   (
      let new_answer = outer_eval pt_with_1 in 
      let _ = (
            if not(Point.is_free pt_with_1) then  
            painstaking_ref := (pt_with_1,new_answer) :: 
          (!painstaking_ref)) in 
      new_answer
   ) ;;      


let eval pt =
    let (d,pt_with_1) = 
      Point.decompose_wrt_translation pt in 
    Mold.translate d
     (eval_on_pt_with_1 pt_with_1);;

end ;;

let eval = Private.eval ;; 

end ;;

module BuiltOnEval = struct 

exception Find_sticky_vertex_exn ;;
exception Lazy_mode_failed of point ;;

module Private = struct 

let lazy_mode = ref false ;;

let eval pt = 
   match One_more_small_step.eval_opt pt with 
   (Some answer) -> answer 
   |None -> 
      if !lazy_mode 
      then raise(Lazy_mode_failed(pt))   
      else Painstaking.eval pt ;;

let measure = Memoized.make(fun pt->
    Mold.solution_size(eval pt)
);;

let check_for_sticky_vertex pt m_pt v= 
    (measure(Point.remove pt [v])=m_pt) ;;

let find_sticky_vertex pt m_pt = 
    let base = Finite_int_set.to_usual_int_list (pt.base_set) in 
    match List.find_opt (check_for_sticky_vertex pt m_pt) (List.rev base) with 
     None -> raise Find_sticky_vertex_exn 
     |Some v -> v ;; 

let check_for_distinguished_part pt m_pt part = 
     let pt1 = Point.remove pt part 
     and pt2 = Point.restrict pt part in 
     (measure pt1)+(measure pt2) = m_pt ;;

let distinguished_parts = Memoized.recursive(fun old_f pt -> 
    if Point.is_free pt 
    then let base = Finite_int_set.to_usual_int_list (pt.base_set) in
         let unordered_ps = List_again.power_set base in 
         il_sort unordered_ps
    else 
    let m_pt = measure pt in 
    let v = find_sticky_vertex pt m_pt in 
    let smaller_pt = Point.remove pt [v] in 
    let smaller_dps = old_f smaller_pt in 
    let enlarged_smaller_dps = Image.image (i_insert v) smaller_dps in 
    let temp1 = List.filter (check_for_distinguished_part pt m_pt) smaller_dps 
    and temp2 = List.filter (check_for_distinguished_part pt m_pt) enlarged_smaller_dps in 
    il_merge temp1 temp2
) ;; 

let hashtbl_for_canonical_solutions = 
    (Hashtbl.create 100: (point, int list) Hashtbl.t ) ;;

let pre_helper_for_solution_chooser_in_hard_case old_f (pt,beheaded_pt,m,obtained_so_far,v,others) =
   if measure(beheaded_pt) = m 
   then old_f (beheaded_pt,m,obtained_so_far,others)
   else 
   let forced_pt = Point.force pt [v] in
   old_f (forced_pt,m-1,v::obtained_so_far,others) ;;
       

let pre_helper_for_solution_chooser old_f (pt,m,obtained_so_far,to_be_treated) =
     match to_be_treated with 
      [] -> obtained_so_far
     |v::others -> 
      let beheaded_pt = Point.remove pt [v] in  
      (
       match Hashtbl.find_opt hashtbl_for_canonical_solutions beheaded_pt with 
       (Some beheaded_csol) ->
         let csol = beheaded_csol @ (v::obtained_so_far) in 
         if Point.subset_is_admissible pt csol 
         then csol     
         else 
         if (List.length beheaded_csol) = m    
         then let csol2 = beheaded_csol @ (obtained_so_far) in
              if Point.subset_is_admissible pt csol2 
              then csol2     
              else  
              pre_helper_for_solution_chooser_in_hard_case old_f (pt,beheaded_pt,m,obtained_so_far,v,others) 
         else 
         pre_helper_for_solution_chooser_in_hard_case old_f (pt,beheaded_pt,m,obtained_so_far,v,others)
      | None -> 
         pre_helper_for_solution_chooser_in_hard_case old_f (pt,beheaded_pt,m,obtained_so_far,v,others)
      );;
       
let rec helper_for_solution_chooser (pt,m,obtained_so_far,to_be_treated) =
    pre_helper_for_solution_chooser helper_for_solution_chooser 
      (pt,m,obtained_so_far,to_be_treated) ;;


let canonical_solution pt = 
   match Hashtbl.find_opt hashtbl_for_canonical_solutions pt with 
   (Some old_answer) -> old_answer 
   |None ->
   let rev_base = List.rev(Finite_int_set.to_usual_int_list pt.base_set) 
   and m = measure pt  in 
   let answer = helper_for_solution_chooser (pt,m,[],rev_base) in 
   let _ = Hashtbl.add hashtbl_for_canonical_solutions pt answer in 
   answer ;; 

module Choose_preferred_decomposition = struct 

let biconnected_measure (fis1,fis2) = 
   match 
   (
      Finite_int_set.is_connected fis1,
      Finite_int_set.is_connected fis2
   )
   with    
   (true,true) -> 1 
   |(false,true) -> 2
   |(true,false) -> 3 
   |(false,false) -> 4 ;;  


let adhoc_order = ((fun (p1,p2) (q1,q2) ->
    let trial1 =i_order 
       (biconnected_measure (p1,p2))
       (biconnected_measure (q1,q2)) in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    let trial2 = i_order (Finite_int_set.size p2) (Finite_int_set.size q2) in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else  
    let trial3 = i_order (Finite_int_set.diameter p2) (Finite_int_set.diameter q2) in 
    if trial3<>Total_ordering_result_t.Equal then trial2 else    
    let lp1 = Finite_int_set.to_usual_int_list p1 
    and lq1 = Finite_int_set.to_usual_int_list q1 in 
    il_order lp1 lq1
): (finite_int_set * finite_int_set) Total_ordering_t.t) ;;   

let adhoc_min = Ordered.min adhoc_order ;;

let pairs_from_distinguished_parts pt parts= Image.image (
  fun p -> 
   let fis = pt.base_set in 
   let n = Finite_int_set.max fis in 
   let whole = Finite_int_set.to_usual_int_list fis in 
   let q = i_setminus whole p in 
   let fp = Finite_int_set.of_usual_int_list p 
   and fq = Finite_int_set.of_usual_int_list q in 
   if Finite_int_set.max(fp) = n
   then (fq,fp)
   else (fp,fq)    
) parts;;

let choose pt parts = 
  adhoc_min(pairs_from_distinguished_parts pt parts);;

end ;;  

let extension_case_opt pt = 
   let m = measure pt in 
   let n = Finite_int_set.max (pt.base_set) in 
   let beheaded_pt = Point.remove pt [n] in 
   if measure(beheaded_pt) = (m-1) 
   then Some Extension 
   else None ;;   

let filled_complement_opt pt = 
   let m = measure pt in 
   let n = Finite_int_set.max (pt.base_set) in 
   let tempf = (fun i->
      measure(Point.remove pt [i]) = m - 1
   ) in 
   let complements = Point.complements pt n in 
   match List.find_opt (
        fun c-> List.for_all tempf c
   ) complements with 
   None -> None 
   |(Some c) -> 
     Some(Filled_complement(c));;   

let decomposition_opt pt =
   let decs = distinguished_parts pt in 
   let real_decs = List.rev(List.tl(List.rev(List.tl decs))) in 
   if real_decs = []
   then None 
   else 
   let (fis1,fis2) = Choose_preferred_decomposition.choose pt real_decs in 
   Some(Decomposition(fis1,fis2,canonical_solution pt))  ;; 
   
let rec compute_breaking_constraint (pt, size) = 
    let cstr = Option.get (Point.highest_constraint_opt pt) in 
    let pt_before = Point.exclude pt cstr in 
    let sol_before = eval pt_before in 
    let size_before = Mold.solution_size sol_before in 
    if size_before<>size  
    then cstr
    else compute_breaking_constraint (pt_before, size) ;;    
let breaking_point_case pt =   
   let m = measure pt in 
   let (C cstr) = compute_breaking_constraint (pt, m) in 
   let nt = (fun k->List.nth cstr (k-1)) in 
   Breaking_point(nt 1,nt 2,nt 3) ;;

let analize pt = 
   if Point.is_free pt then Free else
   let opt1 = extension_case_opt pt in
   if opt1<>None then Option.get opt1 else 
   let opt2 = filled_complement_opt pt in
   if opt2<>None then Option.get opt2 else 
   let opt3 = decomposition_opt pt in
   if opt3<>None then Option.get opt3 else   
   breaking_point_case pt ;;  

let adapt_to_subset pt fis = 
   let l = Finite_int_set.to_usual_int_list fis in 
   let pt2 = Point.restrict pt l in 
   let (_,pt3) = Point.decompose_wrt_translation pt2 in 
   pt3 ;;

let direct_parents pt = match analize pt with 
   Free | Width_one_expl -> []
  |Extension 
  |Filled_complement(_)-> let n = Finite_int_set.max (pt.base_set) in 
                 [Point.remove pt [n]]
  |Decomposition(fis1,fis2,_sol) -> 
      Image.image (adapt_to_subset pt) [fis1;fis2]
  |Breaking_point(i,j,k) ->
      Image.image (fun t->Point.remove pt [t]) [i;j;k];; 

let important_parents pt = List.filter(
   fun pt -> One_more_small_step.eval_opt
   ~without_remembering:true pt=None
)(direct_parents pt) ;;

let rec helper_for_ancestors_computation (treated,to_be_treated) =
   match to_be_treated with 
   [] -> treated 
   |pt :: others ->
       helper_for_ancestors_computation (pt::treated,
       (important_parents pt)@others)

let all_ancestors pt =
   Ordered.sort Point.order
    (helper_for_ancestors_computation ([],
       (important_parents pt))) ;;

let set_lazy_mode lm = (lazy_mode:=lm) ;;

end ;;

let all_ancestors = Private.all_ancestors ;;
let analize = Private.analize ;;
let canonical_solution = Private.canonical_solution ;; 
let set_lazy_mode = Private.set_lazy_mode ;;

end ;;




module Initialization = struct 

module Private = struct 
let segment 
   ?imposed_max_width ?(number_of_extra_obstructions=0) n= 
   let default_width = ((n-1)/2) in 
   let final_width = (
     match imposed_max_width with
      None -> default_width
     |Some(width) -> min width default_width 
   ) in 
   let outer_width = (
     match imposed_max_width with
      None -> default_width
     |Some(width) -> width 
   ) in 
   let extra_obstructions = List.filter_map (
      fun k->
         let m = k+2*(outer_width+1) in 
         if m<=n 
         then Some(C[k;k+(outer_width+1);m])
         else None
   ) (Int_range.range 1 number_of_extra_obstructions) in 
  {
   base_set = FIS (n, []);
   max_width = (W final_width);
   added_constraints = extra_obstructions
  } ;; 

let p2 n o = segment n 
    ~imposed_max_width:2 ~number_of_extra_obstructions:o;;

let d = Deduce.using_decomposition;;
let e = One_more_small_step.eval_opt ~without_remembering:false ;;
let f ?(extra_solutions=[]) pt cstr = 
    Deduce.using_fork ~extra_solutions pt (C cstr);;

let ud 
   ?(extra_solutions=[]) pt tr = 
   let _ = d ~extra_solutions pt tr in () ;;

let ue pt = let _ = e pt in () ;;

let uf ?(extra_solutions=[]) pt cstr = 
    let _ = f ~extra_solutions pt cstr in ();;


let fi = Finite_int_set.interval ;;
let fu = Finite_int_set.of_usual_int_list ;;

let cs = BuiltOnEval.canonical_solution ;;

let u3 n = List.filter (fun k->List.mem(k mod 8)[1;2;4;5]) 
  (Int_range.range 1 n) ;;

let ecs pt = let temp = e pt in (temp,cs  pt) ;;

 

end ;;


open Private ;;

let width_two k = 
   One_more_small_step.explained_eval(Point.usual ~max_width:2 k []);;
    
let recompute_width_two_line () = 
 let _ = BuiltOnEval.set_lazy_mode true in 
 let _ = for k=1 to Util.current_bound do let _ =width_two k in () done in 
 BuiltOnEval.set_lazy_mode false ;;

let check_width_two_line f = 
 let _ = recompute_width_two_line () in 
 let temp1 = Int_range.scale (fun k->
    (k, width_two k, f k)  
 ) 1 Util.current_bound in 
 let temp2 = List.filter_map (
    fun (k,(x1,_x2),(y1,_y2)) ->
      if x1<>y1 then Some(k,x1,y1) else None
 ) temp1 
 and temp3 = List.filter_map (
     fun (k,(_x1,x2),(_y1,y2)) ->
      if x2<>y2 then Some(k,x2,y2) else None
 ) temp1 in 
 (temp2,temp3);; 


let initialize () = 
 let current_bound = Util.current_bound in   
 let _ = BuiltOnEval.set_lazy_mode true in 
 let _ = for k=3 to current_bound do let _ =ecs(p2 k 0) in () done in 
 let _ = for k=3 to 6 do let _ =ecs(p2 k 1) in () done in 
 let _ = d (Point.remove(p2 7 1) [4]) (fi 1 3,fi 5 7,[1;2;5;6]) in 
 let _ = f (p2 7 1) [1;4;7] in
 let _ = cs(p2 7 1) in 
 let _ = for k=8 to current_bound do let _ =ecs(p2 k 1) in () done in 
 let _ = d (Point.remove(p2 8 2) [4;5]) (fi 1 3,fi 6 8,[1;2;6;7]) in 
 let _ = d (Point.remove(p2 8 2) [5;7]) (fi 1 3,fu [4;6;8],[1;3;4;6]) in
 let _ = f (Point.remove(p2 8 2) [5]) [1;4;7] in 
 let _ = d (Point.remove(p2 8 2) [2;4]) (fu [1;3;5],fi 6 8,[1;3;6;7]) in
 let _ = d (Point.remove(p2 8 2) [2;7]) (fu [1;3;5],fu [4;6;8],[1;3;4;6]) in
 let _ = f (Point.remove(p2 8 2) [2]) [1;4;7] in 
 let _ = f (p2 8 2) [2;5;8] in
 let _ = cs(p2 8 2) in 
 let _ = for k=9 to current_bound do let _ =ecs(p2 k 2) in () done in 
 let _=
 (for j=3 to current_bound-6 do
   for k=(j+6) to current_bound do let _ =ecs(p2 k j) in () done
 done)  in
 BuiltOnEval.set_lazy_mode false ;;


(* 

BuiltOnEval.set_lazy_mode true ;;


for k=3 to current_bound do let _ =ecs(p2 k 0) in () done ;;

for k=3 to 6 do let _ =ecs(p2 k 1) in () done ;;

d (Point.remove(p2 7 1) [4]) (fi 1 3,fi 5 7,[1;2;5;6]) ;;
f (p2 7 1) [1;4;7] ;;
cs(p2 7 1);;

for k=8 to current_bound do let _ =ecs(p2 k 1) in () done ;;

d (Point.remove(p2 8 2) [4;5]) (fi 1 3,fi 6 8,[1;2;6;7]);;
d (Point.remove(p2 8 2) [5;7]) (fi 1 3,fu [4;6;8],[1;3;4;6]) ;;
f (Point.remove(p2 8 2) [5]) [1;4;7] ;;

d (Point.remove(p2 8 2) [2;4]) (fu [1;3;5],fi 6 8,[1;3;6;7]) ;;
d (Point.remove(p2 8 2) [2;7]) (fu [1;3;5],fu [4;6;8],[1;3;4;6]) ;;
f (Point.remove(p2 8 2) [2]) [1;4;7] ;;

f (p2 8 2) [2;5;8] ;;
cs(p2 8 2) ;;

for k=9 to current_bound do let _ =ecs(p2 k 2) in () done ;;

for j=3 to current_bound-6 do
   for k=(j+6) to current_bound do let _ =ecs(p2 k j) in () done
done ;;   


BuiltOnEval.set_lazy_mode false ;;

*)

end ;;






