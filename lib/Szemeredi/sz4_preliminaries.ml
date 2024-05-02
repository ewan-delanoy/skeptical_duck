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

type lightweight_mold_state = Sz4_types.lightweight_mold_state = U1 ;;
type heavyweight_mold_state = Sz4_types.heavyweight_mold_state = U2 ;; 

type mold = Sz4_types.mold = {
    solutions : (int list) list;
    forced_elements : int list;
} ;;

type mold_with_state = Sz4_types.mold_with_state = MWS of 
   mold * lightweight_mold_state * heavyweight_mold_state ;; 

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
    (left_complementary_pairs domain w j)@
    (middle_complementary_pairs domain w j)@
    (right_complementary_pairs domain w j) ;; 

  let other_complements pt j =
    List.filter_map (
         fun (C l)->
           if i_mem j l 
           then Some(i_outsert j l) 
           else None  
     ) pt.added_partial_constraints ;; 

  
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
   

  end ;;

  exception Excessive_forcing of point * int list ;; 
  
  let complements pt j = 
     let domain = Finite_int_set.to_usual_int_list pt.base_set 
     and w = pt.max_width in 
     (Private.complementary_pairs domain w j)@
     (Private.other_complements pt j) ;; 

  let constructor = Private.constructor ;; 
  
  let decompose_wrt_translation pt =
     let (d,_) = Finite_int_set.decompose_wrt_translation pt.base_set in 
     (d,Private.translate d pt) ;;  

  let exclude pt constraint_to_be_excluded = 
    if Private.check_excluded_constraint 
        pt.base_set pt.max_width 
        ~checked_added_constraints:[] 
        constraint_to_be_excluded
    then Private.exclude_full_arithmetic_progression pt constraint_to_be_excluded
    else Private.exclude_partial_arithmetic_progression pt constraint_to_be_excluded ;;
 
  let force pt vertices_to_be_forced =
    let new_base = Finite_int_set.remove pt.base_set vertices_to_be_forced in 
    let new_excluded_pcs = List.filter (
        fun (C l) -> i_does_not_intersect l vertices_to_be_forced
    )  pt.excluded_full_constraints 
    and new_added_pcs = Image.image (
        fun (C l) -> C(i_setminus l vertices_to_be_forced)
    )  pt.added_partial_constraints in  
    if List.mem (C []) new_added_pcs 
    then raise(Excessive_forcing(pt,vertices_to_be_forced))
    else 
    let (automatic,non_automatic) = 
      List.partition (fun (C l)->(List.length l)=1 ) new_added_pcs in 
    let forced_elements = Image.image (fun (C l)->List.hd l) automatic in 
    let draft = constructor new_base 
     ~max_width:pt.max_width 
      ~excluded_full_constraints:new_excluded_pcs
       ~added_partial_constraints:non_automatic in 
    if forced_elements = []
    then draft
    else Private.remove draft forced_elements;;
 
  let highest_constraint_opt pt =
    if pt.added_partial_constraints <> []
    then Some(List.hd(List.rev(pt.added_partial_constraints)))
    else 
    let (W w) = pt.max_width in 
    if w < 1 then None else  
    let domain = Finite_int_set.to_usual_int_list pt.base_set in 
    Highest_constraint.below_maximal_width 
       pt.max_width pt.excluded_full_constraints domain ;;

  let remove = Private.remove ;;

  let subset_is_admissible pt subset =
      ((Highest_constraint.below_maximal_width 
       pt.max_width [] subset) 
       =None);;

  let translate = Private.translate ;;     

  end ;; 
  

module Lightweight_mold_state = struct 

let default = U1 ;;
let in_extended_case 
   (_pt:point) (_n:int) (_extended_sols:int list list) 
   (_beheaded_mold:mold) (_light:lightweight_mold_state) =
   default ;;

let in_full_case 
   (_pt:point) (_n:int) (_complements:int list) 
   (_beheaded_mold:mold) (_light:lightweight_mold_state) =
   default ;;


let in_free_case (_pt:point) = default ;; 



end ;;

module Heavyweight_mold_state = struct 

let default = U2 ;;

let in_extended_case 
   (_pt:point) (_n:int) (_extended_sols:int list list) 
   (_beheaded_mold:mold) (_heavy:heavyweight_mold_state) =
   default ;;

let in_full_case 
   (_pt:point) (_n:int) (_complements:int list) 
   (_beheaded_mold:mold) (_heavy:heavyweight_mold_state) =
   default ;;

let in_free_case (_pt:point) = default ;; 

end ;;


module Mold = struct 

 let in_free_case pt =
    let base = Finite_int_set.to_usual_int_list pt.base_set in 
   { 
   solutions  = [base]; 
   forced_elements = base; 
   }
;;

end ;;

module Mold_with_state = struct 

let in_extended_case 
    pt n extended_sols beheaded_mold light heavy =
    let new_mold = {
       solutions = extended_sols;
       forced_elements = (beheaded_mold.forced_elements)@[n]
    } in 
    MWS(
     new_mold,
     Lightweight_mold_state.in_extended_case pt n extended_sols beheaded_mold light,
     Heavyweight_mold_state.in_extended_case pt n extended_sols beheaded_mold heavy
   );;

 let in_full_case 
    pt n complement beheaded_mold light heavy =
    let new_mold = {
       solutions = beheaded_mold.solutions;
       forced_elements = []
    } in 
    MWS(
     new_mold,
     Lightweight_mold_state.in_full_case pt n complement beheaded_mold light,
     Heavyweight_mold_state.in_full_case pt n complement beheaded_mold heavy
   );;


let in_free_case pt =
   MWS(
     Mold.in_free_case pt,
     Lightweight_mold_state.in_free_case pt,
     Heavyweight_mold_state.in_free_case pt
   );;

(*
       Mold_with_state.in_full_case 
         pt n complement beheaded_mold light heavy
     | None ->
       Mold_with_state.use_state
         pt n beheaded_mold light heavy 
*)

end ;;
