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
    excluded_full_constraints: constraint_t list;
    added_partial_constraints: constraint_t list
} ;;

type mold = Sz4_types.mold = {
    solutions : (int list) list;
    mandatory_elements : int list;
} ;;

type decomposition_hook = 
   Sz4_types.decomposition_hook = DH of (int list) * (int list) ;; 



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

  let rec for_exact_positive_width (W w) excluded_constraints 
     domain to_be_treated =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if (i_is_included_in [p-2*w;p-w] domain)
           &&(not(List.mem (C[p-2*w;p-w;p]) excluded_constraints)) 
       then Some (C[p-2*w;p-w;p])
       else for_exact_positive_width (W w) excluded_constraints domain others ;;    
       
  let for_exact_width (W w) excluded_constraints domain =
     if w<1 then None else for_exact_positive_width (W w) excluded_constraints domain (List.rev domain) ;;

  let rec all_for_exact_positive_width (W w) excluded_constraints 
     domain (treated,to_be_treated) =
    match to_be_treated with 
    [] -> treated 
    |p::others ->
       if p<=2*w then treated else 
       if (i_is_included_in [p-2*w;p-w] domain)
           &&(not(List.mem (C[p-2*w;p-w;p]) excluded_constraints)) 
       then all_for_exact_positive_width (W w) excluded_constraints domain (C[p-2*w;p-w;p]::treated,others)
       else all_for_exact_positive_width (W w) excluded_constraints domain (treated,others) ;;    
       
  let all_for_exact_width (W w) excluded_constraints domain =
     if w<1 then [] else all_for_exact_positive_width (W w) excluded_constraints domain ([],List.rev domain) ;;


  let rec below_maximal_width (W w) excluded_constraints domain =
        match for_exact_width (W w) excluded_constraints domain with 
        Some (cstr) -> Some(cstr)
        |None ->
           if w<2 then None else 
           below_maximal_width (W (w-1)) excluded_constraints domain ;;       
  
  end ;;

  let below_maximal_width = Private.below_maximal_width ;;

  let effective_max_width base_set excluded_constraints proposed_width =
     match Private.below_maximal_width proposed_width excluded_constraints base_set with 
     None -> W 0
     |Some(cstr) -> Constraint.width cstr ;; 
  
  

  end ;;

module Finite_int_set = struct 

  exception Translation_goes_negative of int * finite_int_set ;; 

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

  let rec helper_for_oe_decomposer (half1,half2,l) = 
    match l with 
    [] -> (List.rev half1,List.rev half2)
    |a1 :: others1 ->
      (
        match others1 with 
        [] -> (List.rev (a1::half1),List.rev half2)
        |a2 :: others2 ->
           helper_for_oe_decomposer (a1::half1,a2::half2,others2)
      ) ;;

  let oe_decomposer_for_usual_lists l = 
     helper_for_oe_decomposer ([],[],l) ;;

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

  let effective_max_width fis excluded_constraints proposed_width = 
       Highest_constraint.effective_max_width 
       (Private.to_usual_int_list fis) excluded_constraints proposed_width ;; 

  let empty_set = Private.empty_set ;;

  let max (FIS(n,_)) = n ;; 

  let name (FIS(n,l)) =
     "FIS("^(string_of_int n)^",["^
     (String.concat ";" (Image.image string_of_int l))
     ^"])";;

  let oddeven_decomposition fis = 
    Private.oe_decomposer_for_usual_lists(
        Private.to_usual_int_list fis
    );;

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

  The record { 
    base_set:b, 
    max_width:w, 
    excluded_full_constraints: efc,
    added_partial_constraints: apc
  } 
  represents the hypergraph (b,H) 
  where H=(Sz(b,w) \ E) u A= (Sz(b,w) u A) \ E 
  where 
  - Sz(b,w) is the set of all arithmetic progressions 
  inside b with width <=w (in particular, this set is 
  empty when w = 0)
  - A is a set of added constraints of cardinality <= 2.
  - E is a set of excluded arithmetic progressions inside
  b  

  *)

  module Private = struct
  
  let check_excluded_constraint base_set checked_max_width 
       ~checked_added_constraints excl_constraint=
     if Constraint.width(excl_constraint)>checked_max_width 
     then false 
     else if List.exists (Constraint.is_weaker_than excl_constraint) checked_added_constraints 
     then false
     else Finite_int_set.constraint_can_apply base_set excl_constraint ;; 
  
  let constructor base 
     ~max_width:unchecked_max_width 
     ~excluded_full_constraints ~added_partial_constraints = 
      let checked_max_width = Finite_int_set.effective_max_width base excluded_full_constraints unchecked_max_width in 
      let base_list = Finite_int_set.to_usual_int_list base in 
      let checked_added_constraints = Constraint.cleanup_list added_partial_constraints base_list in 
      let checked_excluded_constraints = Constraint.cleanup_list(
         List.filter (check_excluded_constraint base checked_max_width ~checked_added_constraints)
         ( excluded_full_constraints)
      ) base_list in 
      {
        base_set = base;
        max_width = checked_max_width;
        excluded_full_constraints = checked_excluded_constraints;
        added_partial_constraints = checked_added_constraints;
     } ;; 
  

  let exclude_full_arithmetic_progression pt constraint_to_be_excluded = 
    let new_excluded_fcs = constraint_to_be_excluded :: pt.excluded_full_constraints in 
    constructor pt.base_set 
     ~max_width:pt.max_width 
      ~excluded_full_constraints:new_excluded_fcs
       ~added_partial_constraints:(pt.added_partial_constraints);;
   
  let exclude_partial_arithmetic_progression pt constraint_to_be_excluded = 
    let new_added_pcs = 
    List.filter (fun c -> c <> constraint_to_be_excluded) 
       pt.added_partial_constraints in 
    constructor pt.base_set 
     ~max_width:pt.max_width 
      ~excluded_full_constraints:pt.excluded_full_constraints
       ~added_partial_constraints:new_added_pcs;;
   
  let remove pt vertices_to_be_removed =
    let new_base = Finite_int_set.remove pt.base_set vertices_to_be_removed 
    and selector = List.filter (
        fun (C l) -> i_does_not_intersect l vertices_to_be_removed
    ) in 
    let new_excluded_pcs = selector pt.excluded_full_constraints 
    and new_added_pcs = selector pt.added_partial_constraints in  
    constructor new_base 
     ~max_width:pt.max_width 
      ~excluded_full_constraints:new_excluded_pcs
       ~added_partial_constraints:new_added_pcs;; 

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
     ) pt.added_partial_constraints) ;; 

  
  let translate t pt =   
     let new_base = Finite_int_set.translate t pt.base_set in 
     let new_full = Image.image (fun (C l) ->
        C(Image.image ((+) t) l)
     ) pt.excluded_full_constraints
     and new_partial = Image.image (fun (C l) ->
        C(Image.image ((+) t) l)
     ) pt.added_partial_constraints
     in  
     {
        base_set = new_base;
        max_width = pt.max_width;
        excluded_full_constraints = new_full;
        added_partial_constraints = new_partial;
     } ;; 
   
   let subset_is_admissible pt subset =
      ((Highest_constraint.below_maximal_width 
       pt.max_width pt.excluded_full_constraints subset) 
       =None) && 
       (List.for_all (fun (C l)->
         not(i_is_included_in l subset)
       ) pt.added_partial_constraints);;

    let complements pt j = 
     let domain = Finite_int_set.to_usual_int_list pt.base_set 
     and w = pt.max_width in 
     il_merge
     (complementary_pairs domain w j)
     (other_complements pt j) ;; 

   exception Excessive_forcing_reached ;; 

   let force_one_vertex pt vertex_to_be_forced =
    let new_base = Finite_int_set.remove pt.base_set [vertex_to_be_forced] in 
    let new_excluded_pcs = List.filter (
        fun (C l) -> not(i_mem vertex_to_be_forced l)
    )  pt.excluded_full_constraints 
    and new_added_pcs = complements pt vertex_to_be_forced in  
    if List.mem [] new_added_pcs 
    then raise(Excessive_forcing_reached)
    else 
    let (automatic,non_automatic) = 
      List.partition (fun l->(List.length l)=1 ) new_added_pcs in 
    let mandatory_elements = Image.image List.hd automatic 
    and retained_added_pcs = 
       List.filter_map(fun ( C l)-> 
         if not(i_mem vertex_to_be_forced l) then Some l else None) 
       pt.added_partial_constraints in 
    let final_added_pcs = Image.image(fun l-> C l) 
       (il_merge retained_added_pcs non_automatic) in 
    let draft = constructor new_base 
     ~max_width:pt.max_width 
      ~excluded_full_constraints:new_excluded_pcs
       ~added_partial_constraints:final_added_pcs in 
    if mandatory_elements = []
    then draft
    else remove draft mandatory_elements;;

  let restrict pt l = 
     let base = Finite_int_set.to_usual_int_list pt.base_set in 
     remove pt (i_setminus base l) ;; 

  end ;;

  exception Excessive_forcing of point * int list ;; 
  
  let complements = Private.complements ;; 

  let constraint_can_apply pt cstr =
     if List.mem cstr pt.added_partial_constraints 
     then true 
     else 
     if not(Constraint.is_an_arithmetic_progression cstr)
     then false 
     else 
     if List.mem cstr pt.excluded_full_constraints 
     then false 
     else Finite_int_set.constraint_can_apply pt.base_set cstr ;; 
  

  let constructor = Private.constructor ;; 
  
  let decompose_wrt_translation pt =
     let (d,_) = Finite_int_set.decompose_wrt_translation pt.base_set in 
     (d,Private.translate (-d) pt) ;;  

  let exclude pt constraint_to_be_excluded = 
    if Private.check_excluded_constraint 
        pt.base_set pt.max_width 
        ~checked_added_constraints:[] 
        constraint_to_be_excluded
    then Private.exclude_full_arithmetic_progression pt constraint_to_be_excluded
    else Private.exclude_partial_arithmetic_progression pt constraint_to_be_excluded ;;
 
  let force pt vertices_to_be_forced = 
     try List.fold_left Private.force_one_vertex pt vertices_to_be_forced with 
     Private.Excessive_forcing_reached ->   
       raise(Excessive_forcing(pt,vertices_to_be_forced)) ;; 
 
  let highest_constraint_opt pt =
    if pt.added_partial_constraints <> []
    then Some(List.hd(List.rev(pt.added_partial_constraints)))
    else 
    let (W w) = pt.max_width in 
    if w < 1 then None else  
    let domain = Finite_int_set.to_usual_int_list pt.base_set in 
    Highest_constraint.below_maximal_width 
       pt.max_width pt.excluded_full_constraints domain ;;

  let impose pt (C l) = force (exclude pt (C l)) l ;;  

  let is_free pt = ((highest_constraint_opt pt) =None );;

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

let eval pt = 
     let base = List.rev(Finite_int_set.to_usual_int_list pt.base_set) 
     and sols = all_solutions pt 0 in 
     {
       solutions = [helper_for_solution_chooser (sols,base)]; 
       mandatory_elements = i_fold_intersect sols
     } ;; 




end ;;

let all_realizations = Private.all_realizations ;; 
    
let all_solutions = Private.all_solutions ;;  

let eval = Private.eval ;;  

let max_size = Private.max_size ;;


end ;;  







module One_more_small_step = struct 

module Private = struct 

let impatient_ref = ref ([]: (point * mold) list) ;; 
let explanations_ref = ref ([]: (point * string) list) ;; 


let june_left_decompositions_ref = 
  ref ([
    (Int_range.range 1 8),[1;2;4;5]  
  ]) ;;

let june_right_decompositions_ref = 
  ref ([
    (Int_range.range 1 8),[4;5;7;8]  
  ]) ;;  

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
     (Some _complement) ->
       Some(Mold.in_stagnation_case beheaded_mold)
     | None -> None );;  

let lower_level_eval_on_pretranslated_opt pt_with_1 = 
   if Point.is_free pt_with_1 
   then Some(Mold.in_free_case pt_with_1) 
   else List.assoc_opt pt_with_1 (!impatient_ref) ;;

let lower_level_eval_opt pt =
    let (d,pretranslated_pt) = 
      Point.decompose_wrt_translation pt in 
    Option.map(Mold.translate d)
     (lower_level_eval_on_pretranslated_opt pretranslated_pt);;

let check_individual_june_left_decomposition pt (left_domain,left_sol)=
   let whole = Finite_int_set.to_usual_int_list pt.base_set in 
   if not(i_is_included_in left_domain whole)
   then None 
   else 
   let left_pt = Point.restrict pt left_domain in 
   let left_opt = lower_level_eval_opt left_pt in 
   if left_opt = None then None else  
   let left_mold = Option.get left_opt in  
   if (List.length(left_sol)<>Mold.solution_size left_mold)
        ||
        (not(Point.subset_is_admissible left_pt left_sol))
   then None 
   else 
   let right_pt = Point.remove pt left_domain in 
   let right_opt = lower_level_eval_opt right_pt in 
   if (right_opt = None)||(Point.size right_pt = 0) then None else
   let right_mold = Option.get right_opt in  
   List.find_map (
         fun right_sol ->
           let full_sol = i_merge left_sol right_sol in 
           if Point.subset_is_admissible pt full_sol 
           then Some(Mold.in_decomposition_case 
              left_mold right_mold full_sol [])
           else None
      ) right_mold.solutions ;;


let prepare_june_right_decomposition pt (pre_right_domain,pre_right_sol) = 
    let whole = Finite_int_set.to_usual_int_list pt.base_set in 
    let m1 = List.hd (List.rev(pre_right_domain))
    and m2 = List.hd (List.rev(whole)) in 
    let offset = m2-m1 in 
    if offset < 0 then None else
    let right_domain =  Image.image (fun x->x+offset) pre_right_domain 
    and right_sol =  Image.image (fun x->x+offset) pre_right_sol in 
    Some(whole,right_domain,right_sol) ;; 
 

let check_individual_june_right_decomposition pt (pre_right_domain,pre_right_sol)=
   let opt1 = prepare_june_right_decomposition pt (pre_right_domain,pre_right_sol) in 
   if opt1 = None then None else 
   let (whole,right_domain,right_sol) = Option.get opt1 in 
   if not(i_is_included_in right_domain whole)
   then None 
   else 
   let right_pt = Point.restrict pt right_domain in 
   let right_opt = lower_level_eval_opt right_pt in 
   if right_opt = None then None else  
   let right_mold = Option.get right_opt in  
   if (List.length(right_sol)<>Mold.solution_size right_mold)
        ||
        (not(Point.subset_is_admissible right_pt right_sol))
   then None 
   else 
   let left_pt = Point.remove pt right_domain in 
   let left_opt = lower_level_eval_opt left_pt in 
   if (left_opt = None)||(Point.size left_pt = 0) then None else
   let left_mold = Option.get left_opt in  
   List.find_map (
         fun left_sol ->
           let full_sol = i_merge left_sol right_sol in 
           if Point.subset_is_admissible pt full_sol 
           then Some(Mold.in_decomposition_case 
              right_mold left_mold full_sol [])
           else None
      ) left_mold.solutions ;;


let check_june_left_decompositions pt = 
     List.find_map (check_individual_june_left_decomposition pt)
        (!june_left_decompositions_ref) ;;

let check_june_right_decompositions pt =         
      List.find_map (check_individual_june_right_decomposition pt)
        (!june_right_decompositions_ref) ;; 


let add_explanation pt expl = 
     (explanations_ref := (pt,expl) :: (!explanations_ref));;

let eval_without_remembering_opt pt_with_1 =
   if Point.is_free pt_with_1 
   then Some(Mold.in_free_case pt_with_1) 
   else 
   let n = Finite_int_set.max (pt_with_1.base_set) in 
   let beheaded_pt = Point.remove pt_with_1 [n] in 
   let beheaded_mold_opt = lower_level_eval_on_pretranslated_opt beheaded_pt in 
   let opt1 = check_extension_case pt_with_1 n beheaded_mold_opt in 
   if opt1 <> None
   then let _ = add_explanation pt_with_1 "extension" in 
        opt1
   else 
   let opt2 = check_filled_complement_case pt_with_1 n beheaded_mold_opt in 
   if opt2 <> None
   then let _ = add_explanation pt_with_1 "filled complement" in 
        opt2
   else 
   let opt3 = check_june_left_decompositions pt_with_1 in 
   if opt3 <> None
   then let _ = add_explanation pt_with_1 "June left decomposition" in 
        opt3
   else 
   let opt4 = check_june_right_decompositions pt_with_1 in 
   if opt4 <> None
   then let _ = add_explanation pt_with_1 "June right decomposition" in 
        opt4
   else None ;;

let eval_on_pretranslated_opt pt_with_1 =
  match List.assoc_opt pt_with_1 (!impatient_ref) with 
  (Some old_answer) -> Some old_answer
  | None ->
   (
      match eval_without_remembering_opt pt_with_1 with 
      None -> None 
      |Some new_answer ->
        let _ = (
            if not(Point.is_free pt_with_1) then  
            impatient_ref := (pt_with_1,new_answer) :: 
          (!impatient_ref)) in 
        Some new_answer

   ) ;;      

   

let eval_opt pt =
    let (d,pretranslated_pt) = 
      Point.decompose_wrt_translation pt in 
    Option.map(Mold.translate d)
     (eval_on_pretranslated_opt pretranslated_pt);;

let unsafe_add pt mold expl = 
      (impatient_ref := (pt,mold) ::(!impatient_ref);
       explanations_ref := (pt,expl) ::(!explanations_ref);
      ) ;;

let explanation_on_pretranslated_opt pt_with_1 = 
    if Point.is_free pt_with_1 
    then Some "free"
    else List.assoc_opt pt_with_1 (!explanations_ref) ;; 

let explanation_opt pt =  
   let (_,pretranslated_pt) = 
      Point.decompose_wrt_translation pt in 
   explanation_on_pretranslated_opt  pretranslated_pt ;;   

end ;;

let eval_opt = Private.eval_opt ;; 

let explanation_opt = Private.explanation_opt ;;

let unsafe_add = Private.unsafe_add ;;

end ;;

module Deduce = struct 

exception Incorrect_constraint_in_fork_exn 
   of int * int * int  * point ;;

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

let find_translation_index_in_between_intlists l l1 l2 = 
   let translated_l2 = i_setminus l l1 in 
   (List.hd translated_l2) - (List.hd l2);;

let find_translation_index_in_between fis fis1 fis2 = 
   find_translation_index_in_between_intlists 
     (Finite_int_set.to_usual_int_list fis)
     (Finite_int_set.to_usual_int_list fis1)
     (Finite_int_set.to_usual_int_list fis2) ;;


let deduce_using_decomposition ?(extra_solutions=[]) pt (part1,sol1) (part2,sol2) = 
   let mold1 = check_part_in_decomposition pt part1 sol1 
   and pre_mold2 = check_part_in_decomposition pt part2 sol2 in 
   let t = find_translation_index_in_between pt.base_set part1 part2 in 
   let mold2 = Mold.translate t pre_mold2  in 
   let full_sol = i_merge sol1 (Image.image((+)t) sol2) in 
   if Point.subset_is_admissible pt full_sol 
   then let mold = Mold.in_decomposition_case mold1 mold2 full_sol extra_solutions in 
        let _ = One_more_small_step.unsafe_add pt mold "external decomposition" in 
        mold
   else raise(Nonadmissible_whole_in_decomposition(pt,full_sol));;



end ;;

let using_decomposition = Private.deduce_using_decomposition ;;

let using_fork ?(extra_solutions=[]) pt (i,j,k) = 
    let cstr = C [i;j;k] in 
    if not(Point.constraint_can_apply pt cstr)
    then raise(Incorrect_constraint_in_fork_exn(i,j,k,pt))
    else
    let temp1 = Image.image (fun t->
       One_more_small_step.eval_opt(Point.remove pt [t])
    ) [i;j;k] in 
    let m = List_again.find_index_of_in None temp1 in 
    if m > 0 
    then let p = List.nth [i;j;k] (m-1) in
         raise(Incomplete_fork_exn(Point.remove pt [p]))
    else 
    let molds = Image.image Option.get temp1 in 
    let mold = Mold.in_fork_case molds extra_solutions in 
    let _ = One_more_small_step.unsafe_add pt mold "External fork" in 
    mold ;;

end ;;

module Linear = struct 

module Private = struct 

let rec helper_for_rails_computing 
   (treated,current,vertices_to_be_removed) =
 match vertices_to_be_removed with 
  [] -> current :: treated 
 |v :: other_vertices ->
   let next_one = Point.remove current [v] in 
   helper_for_rails_computing 
   (current :: treated,next_one,other_vertices) ;;
    
let compute_rails pt =
   let base = List.tl(Finite_int_set.to_usual_int_list pt.base_set) in 
   helper_for_rails_computing 
   ([],pt,List.rev base) ;; 

let rec helper_for_rails_evaluation (current,to_be_treated) =
  match to_be_treated with 
  [] -> Some current 
  | pt :: other_points ->
     match One_more_small_step.eval_opt pt with 
       None -> None  
      |Some next_one ->
        helper_for_rails_evaluation (next_one,other_points) ;; 

let eval_on_rails_opt pt = 
   match compute_rails pt with 
   [] -> One_more_small_step.eval_opt pt 
   | spark :: rails ->
     (
      match One_more_small_step.eval_opt spark with 
      None -> None 
      |Some spark_mold ->
       helper_for_rails_evaluation (spark_mold,rails) 
     );; 

let eval_opt ?(extra_solutions=[]) pt = 
    match eval_on_rails_opt pt with 
       None -> None 
      |Some mold -> 
        let new_mold = Mold.add_solutions mold extra_solutions 
        and old_mold = Option.get (One_more_small_step.eval_opt pt) in 
        let _ = (
          if new_mold <> old_mold 
          then 
          let expl = Option.get(One_more_small_step.explanation_opt pt) in 
          One_more_small_step.unsafe_add pt new_mold expl
        ) in 
        Some(Mold.add_solutions mold extra_solutions) ;;

end ;;


let eval_opt = Private.eval_opt ;; 

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

         
let lower_level_eval_on_pretranslated_opt pt_with_1 = 
   if Point.is_free pt_with_1 
   then Some(Mold.in_free_case pt_with_1) 
   else 
   match List.assoc_opt pt_with_1 (!painstaking_ref) with 
  (Some old_answer) -> Some old_answer
  | None ->
    (match List.assoc_opt pt_with_1 (!new_discoveries_ref) with 
  (Some old_answer) -> Some old_answer
  | None ->
   (match Linear.eval_opt pt_with_1 with  
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
   match lower_level_eval_on_pretranslated_opt next_pt with 
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
          fun pt -> (pt,lower_level_eval_on_pretranslated_opt pt)
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
   match lower_level_eval_on_pretranslated_opt pt_with_1 with 
  (Some old_answer) -> old_answer
  | None ->
     let n = Finite_int_set.max (pt_with_1.base_set) in 
     let beheaded_pt = Point.remove pt_with_1 [n] in 
     let forced_pt = Point.force pt_with_1 [n] in
     let temp1 = Image.image (
          fun pt -> (pt,lower_level_eval_on_pretranslated_opt pt)
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



let eval_on_pretranslated pt_with_1 =
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
    let (d,pretranslated_pt) = 
      Point.decompose_wrt_translation pt in 
    Mold.translate d
     (eval_on_pretranslated pretranslated_pt);;

end ;;

let eval = Private.eval ;; 

end ;;

module BuiltOnEval = struct 

exception Find_sticky_vertex_exn ;;

type decomposition_t = D of (int list) * (int list) ;; 

type decomposition_to_be_deduced_t = 
    DTBD of ( finite_int_set * (int list) ) * 
            ( finite_int_set * (int list) ) ;;

type decomposition_data = {
    decompositions : decomposition_t list ;
    chosen_decomposition : decomposition_t ;
    to_be_deduced : decomposition_to_be_deduced_t ;
    canonical_solution : int list
} ;;

type breaking_point_data = {
    breaking_constraint : constraint_t ;
    broken_point : point ;
    broken_mold : mold 
} ;;

type analysis_result =
   Extension
  |Decomposition of decomposition_data
  |Breaking_point of breaking_point_data ;; 




module Private = struct 

let measure = Memoized.make(fun pt->
    Mold.solution_size(Painstaking.eval pt)
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

let rec helper_for_solution_chooser (pt,m,obtained_so_far,to_be_treated) =
     match to_be_treated with 
      [] -> obtained_so_far
     |v::others ->
       let beheaded_pt = Point.remove pt [v] in 
       if measure(beheaded_pt) = m 
       then helper_for_solution_chooser (beheaded_pt,m,obtained_so_far,others)
       else 
       let forced_pt = Point.force pt [v] in
       helper_for_solution_chooser (forced_pt,m-1,v::obtained_so_far,others)
       
let canonical_solution = Memoized.make(fun pt->
   let rev_base = List.rev(Finite_int_set.to_usual_int_list pt.base_set) 
   and m = measure pt  in 
   helper_for_solution_chooser (pt,m,[],rev_base)
) ;; 

let compute_data_around_decomposer (D(part1,part2)) full_sol= 
    let t = (List.hd part2) - 1 in 
    let translate = Image.image (fun x->x-t)  in  
    let translated_part2 = translate part2 in  
    let sol1 = i_intersect part1 full_sol 
    and sol2 = translate(i_intersect part2 full_sol) in 
    let fis1 = Finite_int_set.of_usual_int_list part1 
    and fis2 = Finite_int_set.of_usual_int_list translated_part2 in 
    DTBD((fis1,sol1),(fis2,sol2));;


let analysis_in_decomposition_case pt real_decs = 
   let n = Finite_int_set.max (pt.base_set) in 
   if il_mem [n] real_decs 
   then Extension  
   else
   let chosen_second_part = List.find (i_mem n) real_decs in
   let whole = Finite_int_set.to_usual_int_list (pt.base_set) in 
   let chosen_first_part = i_setminus whole chosen_second_part in
   let chosen_dec =D(chosen_first_part,chosen_second_part) in  
   let sol = canonical_solution pt in 
   let n = Finite_int_set.max (pt.base_set) in 
   let formal_decs = List.filter_map (
      fun part ->
       let other_part = i_setminus whole part in 
       if i_mem n other_part 
       then Some(D(part,other_part))
       else None
   ) real_decs in 
   Decomposition(
   {
      decompositions = formal_decs ;
      chosen_decomposition = chosen_dec ;
      to_be_deduced = compute_data_around_decomposer chosen_dec sol;
      canonical_solution = sol 
   });;


let rec helper_for_analysis (pt, size) = 
    let decs = distinguished_parts(pt) in 
    let real_decs = List.rev(List.tl(List.rev(List.tl decs))) in 
    if real_decs <> []
    then analysis_in_decomposition_case pt real_decs 
    else
    let cstr = Option.get (Point.highest_constraint_opt pt) in 
    let pt_before = Point.exclude pt cstr in 
    let sol_before = Painstaking.eval pt_before in 
    let size_before = Mold.solution_size sol_before in 
    if size_before<>size  
    then Breaking_point(
        {
          breaking_constraint = cstr ;
          broken_point = pt_before ;
          broken_mold = sol_before 
        })
   else helper_for_analysis (pt_before, size) ;; 

let analize pt = 
    let mold = Painstaking.eval pt in 
    let size = Mold.solution_size mold in
    helper_for_analysis (pt, size) ;;  


end ;;

let analize = Private.analize ;;
let canonical_solution = Private.canonical_solution ;; 

end ;;


module PointExample = struct 

let segment 
   ?imposed_max_width ?rightmost_cut n= 
   let default_width = ((n-1)/2) in 
   let effective_max_width = (
     match imposed_max_width with
      None -> default_width
     |Some(width) -> min width default_width 
   ) in 
   let maximal_rightmostcut_length = n-2*effective_max_width in
   let rightmost_cut_length = (
      match rightmost_cut with
      None -> 0
     |Some(r) -> max 0 (min r maximal_rightmostcut_length)
   ) in 
   let (final_width,final_constraints)= (
      if (rightmost_cut_length = maximal_rightmostcut_length)
      then (* the whole upper level has been erased,
              we go down one level *)
              (effective_max_width-1,[])
      else (effective_max_width,
            Int_range.scale(fun t->
              let p = n + rightmost_cut_length -t in 
              C[p-2*effective_max_width;p-effective_max_width;p]
            ) 1 rightmost_cut_length
            )
   ) in 
{
  base_set = FIS (n, []);
  max_width = (W final_width);
  excluded_full_constraints = final_constraints; 
  added_partial_constraints = []
} ;; 

end ;; 

module Initialization = struct 

module Private = struct 

let p3 n = PointExample.segment n ~imposed_max_width:3;;
let pr3 n r = Point.remove (p3 n) r ;;

let d = Deduce.using_decomposition;;
let e = Linear.eval_opt ;;
let f = Deduce.using_fork;;

let dpr3 ?extra_solutions n r = d ?extra_solutions (pr3 n r);;
let epr3 ?extra_solutions n r = e ?extra_solutions (pr3 n r);;
let fpr3 ?extra_solutions n r = f ?extra_solutions (pr3 n r);;


end ;;


open Private ;;

(* computing epr3 n [] *)


epr3 6 [];;
epr3 7 [4] ;; 
fpr3 7 [] (1,4,7) ;;

epr3 8 [2;4] ;;
dpr3 8 [2;7] (FIS(5,[2;4]),[1;3]) (FIS(5,[2;4]),[1;3]) 
   ~extra_solutions:[[1;3;4;8]];; 
fpr3 8 [2] (1,4,7) ;;

epr3 8 [5;4] ;;
dpr3 8 [5;7] (FIS(3,[]),[1;3]) (FIS(5,[2;4]),[1;3]) ;; 

fpr3 8 [5] (1,4,7) ;;
fpr3 8 [] (2,5,8) ;;

epr3 200 [];;


(* computing epr3 n [2] *)

(*
epr3 6 [2];;


fpr3 7 [2;6] (1,4,7) ;;
dpr3 7 [2] (FIS(6,[1;2;3;4;5]),[6]) (FIS(7,[2;6]),[1;3;4]) ;; 

fpr3 8 [2] (1,4,7) ;;
epr3 9 [2] ;; 

dpr3 10 [2] (FIS(1,[]),[1]) (FIS(8,[]),[1;2;6;7]);; 

epr3 12 [2] ;;
epr3 13 [2] ~extra_solutions:[[1;3;4;6;10;11;13]] ;;

epr3 14 [2] ;;


fpr3 7 [2;3] (1,4,7) ;;    
epr3 9 [2;3;8] ;;     
fpr3 9 [2;8] (3,6,9) ;;
dpr3 15 [2] (FIS(9,[2;8]),[1;3;4;6]) (FIS(8,[2]),[3;4;6;7]);; 
dpr3 16 [2] (FIS(8,[2]),[1;3;4;6]) (FIS(8,[]),[2;3;5;6])
   ~extra_solutions:[[1;3;4;8;9;11;12;16]] ;; 
epr3 17 [2] ;;






*)


end ;;






