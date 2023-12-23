(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type point = Sz3_types.point = P of finite_int_set * width ;; 

type quantify_constraints = Sz3_types.quantify_constraints =
    Some_constraints
    |All_constraints ;; 


type point_with_breadth = Sz3_types.point_with_breadth = 
     No_constraint of finite_int_set
    |Usual of quantify_constraints * point * int ;; 

type handle = Sz3_types.handle = 
   Has_no_constraints
  |Select of int * int * int   
  |Rightmost_overflow of int * int * int 
  |Rightmost_pivot of width
  |Fork of int * int * int ;;  

type fan = Sz3_types.fan = F of int list list ;; 

type piece_of_help = Sz3_types.piece_of_help = {
   beneficiary : point_with_breadth ;
   extra_solutions : (int * solution list) list;
   imposed_fans : (int *fan) list;
   extra_grooves_for_fork : int list;
} ;; 

type small_mold = Sz3_types.small_mold = SM of (solution list) * fan ;; 

type mold = Sz3_types.mold = BM of extension_data * (int * small_mold) list ;;

type flexible_grocery = Sz3_types.flexible_grocery = Flg of  (point_with_breadth * (handle * mold)) list ;; 

type shortened_grocery = Sz3_types.shortened_grocery = {
  sg_helpers : piece_of_help list;
  sg_pair_level : ((width * int list) * (int -> int -> handle * mold)) list;
  sg_triple_level : ((width * int list * int) * (int -> handle * mold)) list
} ;;


  
type diagnosis = Sz3_types.diagnosis =
   Missing_fan of string * point_with_breadth * int * fan 
  |Missing_solution of string * point_with_breadth * solution 
  |Missing_subcomputation of string * point_with_breadth 
  |Missing_switch_in_fork of int * point_with_breadth ;;

type chain_inspection_result = Sz3_types.chain_inspection_result =
  Smooth of (handle * mold) * (unit -> ((point_with_breadth * (handle * mold)) list))
  |Counterexample_found of point_with_breadth * diagnosis ;; 

type fan_related_requirement = Sz3_types.fan_related_requirement = FRR of (int * fan) list ;;    

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

let select_in_list  l_cstr candidates =  
   List.filter (fun candidate->
    List.for_all( fun (C cstr) ->not(i_is_included_in cstr candidate)) l_cstr ) 
     candidates;;


let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  

module Find_highest_constraint = struct

  let rec for_exact_width (W w) domain to_be_treated =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if i_is_included_in [p-2*w;p-w] domain 
       then Some (C[p-2*w;p-w;p])
       else for_exact_width (W w) domain others ;;     
  
  let rec below_maximal_width (W w) domain =
   match for_exact_width (W w) domain (List.rev domain) with 
   Some (cstr) -> Some(cstr)
   |None ->
      if w<2 then None else 
      below_maximal_width (W (w-1)) domain ;;  
  
  let below_width_bound_pair (W w,bound) domain =
    match List.find_opt(fun b->
      i_is_included_in [b;b+(w+1);b+2*(w+1)] domain
      ) (List.rev(Int_range.range 1 bound)) with 
    Some bmax ->  Some (C[bmax;bmax+(w+1);bmax+2*(w+1)])
    | None -> below_maximal_width (W w) domain ;; 

  end ;;

module Finite_int_set = struct 

  exception Translation_goes_negative of int * finite_int_set ;; 

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  let translation_goes_negative d = function 
    [] -> false
    | m :: _ -> m+d<0 ;;      

  end ;;

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

  let empty_set = FIS(0,[]) ;;

  let max (FIS(n,_)) = n ;; 

  let of_usual_int_list = Private.of_usual_int_list ;; 

  let order = ((fun (FIS(n1,scr1)) (FIS(n2,scr2)) ->
    let trial1 = i_order n1 n2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    let trial2 = i_order (List.length scr2) (List.length scr1) in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else
      Total_ordering.silex_for_intlists scr1 scr2
  ): finite_int_set Total_ordering_t.t);;

  let remove_element fis k=
    let (FIS(n,scrappers)) = fis in 
    if (k>n)||(k<1) then fis else 
    let new_scrappers = i_insert k scrappers in 
    if k <> n then FIS(n,new_scrappers) else 
    if scrappers = Int_range.range 1 (n-1)
    then empty_set
    else   
    let new_z =  Private.to_usual_int_list (FIS(n-1,scrappers)) in 
    let new_max = List.hd(List.rev new_z) in 
    FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;         

  (*
  
  remove_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_element (FIS(3,[])) 3 ;;
  remove_element (FIS(1,[])) 1 ;;
  remove_element (FIS(1,[])) 4 ;;

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

  let decompose_wrt_translation (P(fis,w)) = 
     let (d,translated_fis) = Finite_int_set.decompose_wrt_translation fis in 
     (d,P(translated_fis,w));;

  let decrement (P(fis,W w)) = P(fis,W(w-1)) ;;    

  let highest_constraint_opt (P(fis,W w)) = 
    if w<1 then None else
    Find_highest_constraint.below_maximal_width 
      (W w) (Finite_int_set.to_usual_int_list fis);;

  let has_no_constraint pt = (highest_constraint_opt(pt)=None) ;; 

  let is_nontrivial (P(fis,w)) =
    let domain = Finite_int_set.to_usual_int_list fis in
    ((Find_highest_constraint.below_maximal_width w domain) <> None);;

  let max (P(fis,_w)) = Finite_int_set.max fis  ;; 

  let order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    Finite_int_set.order fis1 fis2
  ): point Total_ordering_t.t);;

  let remove_element (P(fis,w)) pivot = 
    let new_fis = Finite_int_set.remove_element fis pivot in 
    let new_w = (
      match Find_highest_constraint.below_maximal_width w (Finite_int_set.to_usual_int_list new_fis) with
      None -> 0
      |Some(C(l))->(List.nth l 1)-(List.nth l 0)
    ) in 
    P(new_fis,W new_w) ;;

  let remove_elements pt pivots = List.fold_left remove_element pt pivots ;;   

  let size (P(fis,_)) = Finite_int_set.size fis ;; 

  let standardize pt =
     let (P(fis,_)) = pt in 
      match highest_constraint_opt pt with  
      None -> P(fis,W 0)
    |Some(C cstr) -> 
      let nth = (fun k->List.nth cstr (k-1)) in 
      let w = W((nth 2)-(nth 1)) in 
      P(fis,w);;
 ;; 

  let supporting_set (P(fis,_)) = Finite_int_set.to_usual_int_list fis ;; 

  let subset_is_admissible (P(_,w)) subset =
      ((Find_highest_constraint.below_maximal_width w subset) =None);;

  let translate d (P(fis,w)) = 
      P(Finite_int_set.translate d fis,w);;

  let width (P(_,w)) = w ;; 


end ;;   



module Point_with_breadth = struct 

exception No_constraint_no_point_no_breadth_exn of finite_int_set ;;   

module Private = struct

let last_step_in_constructor pt b =
  let n = Point.max pt and (W w) = Point.width pt in 
  let qc = (
     if b= n - 2*(w+1)
     then All_constraints
     else Some_constraints
  ) in 
  Usual(qc,pt,b) ;; 

let constructor n scr (W old_w) b =  
  let fis = FIS(n,scr) in
  let pt = P(fis,W old_w) in 
  let support = Point.supporting_set pt in 
  let possible_breadths = List.rev(Int_range.range 1 b) in 
  match List.find_opt (fun t->i_is_included_in [t;t+(old_w+1);t+2*(old_w+1)] support) possible_breadths with
  Some(b0)-> last_step_in_constructor pt b0
  |None ->
  match Point.highest_constraint_opt pt with  
   None -> No_constraint(fis)
  |Some(C cstr) -> 
    let nth = (fun k->List.nth cstr (k-1)) in 
    let new_b = nth 1
    and new_w = (nth 2)-(nth 1)-1 in 
    last_step_in_constructor (P(fis,W new_w)) new_b;;   


let constructor_for_two (P(FIS(n,scr),w)) b = constructor n scr w b ;;    

let constructor_for_three (FIS(n,scr)) w b = constructor n scr w b ;; 

let all_constraints pt = constructor_for_two pt 0 ;; 


let usual_decomposition_opt = function 
    (No_constraint _fis) -> None
   |Usual(_qc,pt,b) ->  
    let (P(_,W w)) = pt in 
    Some(constructor_for_two pt (b-1),C[b;b+(w+1);b+2*(w+1)]);;

  
  let translate d  = function 
     (No_constraint fis) -> (No_constraint (Finite_int_set.translate d fis))
      (*
      when b+d<0, there is no extra constraint, 
      and this is equivalent to setting the new b to 0  
    *) 
    |Usual(_qc,pt,b) ->  
      let new_b = max(0)(b+d) in
      constructor_for_two(Point.translate d pt) new_b ;;

    let support = function 
    (No_constraint fis) -> fis
    |Usual(_qc,P(fis,_w),_b) -> fis  ;;

    let supporting_set pwb = Finite_int_set.to_usual_int_list (support pwb) ;;    
    
    let point_and_breadth = function 
    (No_constraint fis) -> raise(No_constraint_no_point_no_breadth_exn(fis))
    |Usual(_qc,pt,b) -> (pt,b)  ;;

  let has_no_constraint = function 
    (No_constraint _fis) -> true
    |Usual(_qc,_pt,_b) -> false  ;; 

   let complementary_pairs pwb = 
    if has_no_constraint pwb then [] else 
     let (P(FIS(n,_scr),W max_w),b) = point_and_breadth pwb 
     and domain = supporting_set pwb in 
     let candidates = Int_range.range 1 (max_w+1) in 
     List.filter_map (
         fun w->
            let u = n-2*w and v=n-w in 
           if not(i_is_included_in [u;v] domain) then None else
           if w<=max_w then Some(u,v) else 
           if u<=b then Some(u,v) else None  
     ) candidates ;;

   let obstructions pwb =
     if has_no_constraint pwb then [] else 
     let (P(FIS(n,_scr),W wmax),b) = point_and_breadth pwb in 
     let obstructions_for_width = (fun w->Int_range.scale(fun t->[t;t+w;t+2*w]) 1 (n-2*w)) in 
       List.flatten((Int_range.scale obstructions_for_width 1 wmax)@
       [Int_range.scale(fun t->[t;t+(wmax+1);t+2*(wmax+1)]) 1 b]);;

   let solutions pwb offset =
      let temp1 = il_sort(List_again.power_set (supporting_set pwb)) in 
      let obstrs = obstructions pwb in
      let temp2 = List.filter (fun y->List.for_all (fun obs->not(i_is_included_in obs y))obstrs) temp1 in 
      let m = List.length(List.hd(List.rev temp2)) in 
      List.filter (fun y->List.length(y)=m-offset) temp2 ;; 

   let rightmost_largest_width pwb =
      let (P(FIS(n,_scr),W w),b) = point_and_breadth pwb in 
      if b>=(n-2*(w+1))
      then W(w+1)
      else W(w) ;;   

    let inclusion_test_for_non_isolation wmax b domain w candidate =
        if w=wmax+1
        then (i_is_included_in candidate domain) && (List.hd(candidate)<=b)
        else i_is_included_in candidate domain  ;;

   let atomic_test_for_non_isolation wmax b domain x w = 
       if w>wmax+1 then false else 
        let incl_test = inclusion_test_for_non_isolation wmax b domain w in 
       (incl_test [x-2*w;x-w;x])
       || 
       (incl_test [x-w;x;x+w]) 
       ||
       (incl_test [x;x+w;x+2*w]) ;; 


   let individual_test_for_non_isolation wmax b domain x=
       List.exists(atomic_test_for_non_isolation wmax b domain x) (Int_range.range 1 (wmax+1)) ;;

    let nonisolated_version pwb =
       if has_no_constraint pwb 
       then let fis = support pwb in 
            (No_constraint Finite_int_set.empty_set,Finite_int_set.to_usual_int_list fis) 
       else let (P(fis,W wmax),b) = point_and_breadth pwb in 
       let domain = Finite_int_set.to_usual_int_list fis in 
       let (non_isolated,isolated) = List.partition (individual_test_for_non_isolation wmax b domain) domain in 
       let new_fis = Finite_int_set.of_usual_int_list non_isolated in 
       (constructor_for_three new_fis (W wmax) b,isolated);;
    
    let remove_element pwb elt = 
      if has_no_constraint pwb 
      then let fis = support pwb in 
             (No_constraint (Finite_int_set.remove_element fis elt)) 
      else let (P(fis,W wmax),b) = point_and_breadth pwb in 
      let new_fis = Finite_int_set.remove_element fis elt in 
      let new_domain = Finite_int_set.to_usual_int_list new_fis in 
      match Find_highest_constraint.below_width_bound_pair (W wmax,b) new_domain with
      None -> No_constraint(new_fis)
      |Some(C cstr)->
        let nth = (fun k->List.nth cstr (k-1)) in 
        let new_wmax = (nth 2)-(nth 1)-1 in 
        constructor_for_three new_fis (W new_wmax) (nth 1);;

    let compare_pbs pt1 b1 pt2 b2 =
      let trial1 = Point.order pt1 pt2 in 
      if trial1<>Total_ordering_result_t.Equal then trial1 else 
      i_order b1 b2;;

  

    let compare_in_no_constraints_case fis1 = function 
      (No_constraint fis2) -> Finite_int_set.order fis1 fis2
      |Usual(_qc,_pt,_b) -> Total_ordering_result_t.Lower  ;; 

    let compare_in_usual_case pt1 b1 = function 
      (No_constraint _fis2) -> Total_ordering_result_t.Greater
      |Usual(_qc2,pt2,b2) -> compare_pbs pt1 b1 pt2 b2  ;;   

    

    let order = ((fun pwb1 pwb2 ->
          match pwb1 with 
          (No_constraint fis1) -> compare_in_no_constraints_case fis1 pwb2 
          |Usual(_qc1,pt1,b1) -> compare_in_usual_case pt1 b1 pwb2
          ): point_with_breadth Total_ordering_t.t);;    

    let subset_is_admissible pwb subset = 
      if has_no_constraint pwb 
      then true
     else
     let (pt,b) = point_and_breadth pwb in    
      if not(Point.subset_is_admissible pt subset)
      then false 
      else 
        let (P(_,W w)) = pt in 
        List.for_all (fun t->not(i_is_included_in [t;t+(w+1);t+2*(w+1)] subset)) (Int_range.range 1 b);;    

    let decompose_wrt_translation pwb = match pwb with
        (No_constraint fis) -> 
            let (d,translated_fis) = Finite_int_set.decompose_wrt_translation fis in 
            (d,No_constraint translated_fis)
        |Usual(_qc,_pt,_b) -> 
          let (pt,_b) = point_and_breadth pwb in 
          let (d,_) = Point.decompose_wrt_translation pt in 
          (d,translate (-d) pwb);; 


    let left pwb =
        let n = Finite_int_set.max (support pwb) in 
        remove_element pwb n ;;        

    let predecessor_opt pwb = match usual_decomposition_opt pwb with 
      None -> None 
     |Some(prec_pwb,_) -> Some prec_pwb ;;   
       

end ;;  

let all_constraints = Private.all_constraints ;; 
let breadth pwb =snd(Private.point_and_breadth pwb);;
let constructor = Private.constructor ;; 
let complementary_pairs = Private.complementary_pairs ;;
let decompose_wrt_translation = Private.decompose_wrt_translation ;;  
let has_no_constraint pwb = (Private.usual_decomposition_opt pwb=None) ;;
let left = Private.left ;;  
let max pwb = Finite_int_set.max (Private.support pwb) ;;
let nonisolated_version = Private.nonisolated_version ;;
let order = Private.order ;; 
let predecessor_opt = Private.predecessor_opt ;;
let point pwb = fst(Private.point_and_breadth pwb) ;;  
let projection pwb = snd(decompose_wrt_translation pwb);;
let remove_element = Private.remove_element ;;
let rightmost_largest_width = Private.rightmost_largest_width ;;
let size pwb = match pwb with 
   No_constraint(fis)->Finite_int_set.size fis 
  |Usual(_qc,pt,_b)-> Point.size pt ;;
let solutions = Private.solutions ;;  
let subset_is_admissible= Private.subset_is_admissible ;; 
let support  = Private.support ;; 
let supporting_set  = Private.supporting_set ;;
let translate = Private.translate ;; 
let usual_decomposition_opt = Private.usual_decomposition_opt ;; 
let width pwb =
    let (pt,_b) = Private.point_and_breadth pwb in 
    Point.width pt ;;

end ;;  


module Fan = struct 

  exception Impose_exn of fan * (constraint_t list);;
  exception Badly_formed_fan ;;
  
  module Private = struct

    let constructor ll =
      if ll= [] then raise Badly_formed_fan else 
      let sorted_ll = il_sort ll in 
      F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  let distribute (F rays) addendum= F(Image.image (i_merge addendum) rays) ;;  

  let combine_two_conditions (F ll1) (F ll2) =
    let temp1 = Cartesian.product ll1 ll2 in 
    constructor( Image.image (fun (x,y)->i_merge x y) temp1 );; 

 let combine_conditions = function 
     [] -> F[]
    |first_fan :: other_fans ->
       List.fold_left combine_two_conditions first_fan other_fans ;; 

  let canonical_container_in_hard_case initial_competing_fans =
    let measure = (fun (F rays)->
      i_length_preserving_sort (Image.image List.length rays)
    ) in 
    let temp1 = Image.image measure initial_competing_fans in 
    let smallest_measure = il_min temp1 in 
    let competing_fans = 
        List.filter(fun mz->measure(mz)=smallest_measure)  
            initial_competing_fans in 
    combine_conditions competing_fans ;; 

  let canonical_container sample (F rays) =
     let indexed_rays = Int_range.index_everything rays in 
     let covering_indices = (fun x->
        List.filter_map (fun (idx,ray)->
           if i_is_included_in ray x 
           then Some idx 
          else None   
        ) indexed_rays
      ) in
      let temp1 = Image.image covering_indices sample in 
      let temp2 = Ordered_misc.minimal_transversals temp1 in 
      let (_,temp3) = Min.minimize_it_with_care List.length temp2 in 
      let return_to_original = (fun l->F(Image.image(fun idx->List.assoc idx indexed_rays) l)) in 
      if List.length temp3 = 1 
      then return_to_original (List.hd temp3) 
      else      
      let temp4 = Image.image return_to_original temp3 in
      canonical_container_in_hard_case temp4 ;;

    let impose_opt l_cstr (F rays) =  
        let new_rays =Constraint.select_in_list l_cstr rays in 
        if new_rays = []
        then None
        else Some(F new_rays);;  

  end ;;  

  let canonical_container = Private.canonical_container ;; 

  let combine_two_conditions = Private.combine_two_conditions ;; 

  let combine_conditions = Private.combine_conditions ;;  

  let constructor = Private.constructor ;;

  let core (F ll) = 
    i_fold_intersect ll ;; 

  let empty_one = F [[]] ;;

  let impose l_cstr fan = 
     match Private.impose_opt l_cstr fan with 
     None -> raise(Impose_exn(fan,l_cstr))
    |Some answer -> answer;;
  
  let impose_and_distribute  (l_cstr,addendum) fan = 
      Private.distribute ( impose l_cstr fan) addendum ;;

  let impose_opt = Private.impose_opt ;; 

  let translate d (F rays) = F(Image.image (fun ray->Image.image (fun t->t+d) ray) rays);;

  let union (F ll1) (F ll2) = constructor(ll1@ll2) ;; 

  let with_or_without (F(ll)) n complements_for_n =
    let rays_for_n=Image.image (fun (i,j)->[i;j]) complements_for_n in   
    let (with_n,without_n)=List.partition (i_mem n) ll in 
    let with_n_removed = Image.image (i_outsert n) with_n in 
    (constructor(with_n_removed@without_n@rays_for_n),F(without_n)) ;;   


end ;;   

module Small_mold = struct 

  let add_isolated_set (SM(sols,fan)) isolated_set =
    let add = i_merge isolated_set in 
    SM(Image.image add sols,Fan.impose_and_distribute ([],isolated_set) fan) ;;

  let constructor sols forced_elts = SM(sols,Fan.constructor [forced_elts]);;

  let empty_one = SM([],Fan.empty_one);;

  let fan (SM(_sols,fan_inside)) = fan_inside ;;

  let impose l_cstr (SM(sols,fan)) =
     SM(Constraint.select_in_list l_cstr sols,Fan.impose l_cstr fan) ;; 

  let translate d (SM(sols,fan))  = 
    SM(Image.image (Image.image (fun t->t+d)) sols,Fan.translate d fan) ;;  

  let test_for_impossible_constraint (SM(_sols,fan)) c_constraints =
     ((Fan.impose_opt c_constraints fan)=None) ;;

  let typical_selection (complements,addendum_opt) (SM(sols1,fan1)) = 
    let automatically_distributed = Option.to_list addendum_opt in 
    let for_a_solution_set = (fun sols->Image.image (i_merge automatically_distributed)
        (Constraint.select_in_list complements sols))
    and for_a_fan = Fan.impose_and_distribute (complements,automatically_distributed) in 
    SM(for_a_solution_set sols1,for_a_fan fan1)  
       ;;

  let typical_union (complements,n) (SM(sols1,fan1)) (SM(sols2,fan2))= 
    let for_a_solution_set = (fun sols->Image.image (i_insert n)
        (Constraint.select_in_list complements sols))
    and for_a_fan = Fan.impose_and_distribute (complements,[n]) in 
    SM(il_merge (for_a_solution_set sols1) sols2,
       Fan.union (for_a_fan fan1) fan2  
      ) ;;

end ;;
 

module Mold = struct 

  exception Negative_fan_index_exn of int ;;
  exception In_short_sized_case_exn of point_with_breadth ;;

  module Private = struct 
  
  let constructor_opt pwb naive_forced_elts l = 
      let (_,isolated_elts) = Point_with_breadth.nonisolated_version pwb 
      and (SM(_,first_fan)) = List.assoc 0 l in 
      let forced_elts = i_fold_merge [naive_forced_elts;isolated_elts;Fan.core first_fan] in  
      let (SM(sols,_)) = List.assoc 0 l in 
      if sols <> [] 
      then Some(BM(forced_elts,l))
      else None ;;
  
  let small_mold_at_index (BM(_,l)) i = 
      if i<0 then raise (Negative_fan_index_exn i) else
      match List.assoc_opt i l with 
     Some small_mold -> small_mold 
    |None -> SM([],Fan.empty_one) ;; 

  let solutions (BM(_,l)) = 
      let (SM(sols,_)) = List.assoc 0 l in sols ;;     
  
  end ;;  
  
  let add_isolated_set (BM(forced_elts,l)) isolated_set =
     let add = i_merge isolated_set in
     let tip = List.assoc 0 l in  
     BM(add forced_elts,[0,Small_mold.add_isolated_set tip isolated_set]) ;;
  
  let discrete domain = BM(domain,[0,SM([domain],Fan.constructor [domain])]) ;; 
  
  let fan_at_index mold i = Small_mold.fan(Private.small_mold_at_index mold i) ;;

  let forced_elements (BM(ext,_)) = ext ;; 
  
  let fork_opt pwb prec_mold pointed_ones (i,j,k) = 
      let (BM(_prec_ext,prec_l)) = prec_mold in 
      let c_constraints = [C[i;j;k]] 
      and sols = il_fold_merge(Image.image Private.solutions pointed_ones) in 
      let (SM(_,old_fan1)) = Private.small_mold_at_index prec_mold 1 in 
      let first_fan = Fan.impose c_constraints old_fan1 in  
      let new_l= (0,SM(sols,first_fan))::(List.filter_map (
            fun (i,old_indication)->
               if i<=1 then None else
               Some(i-1,Small_mold.impose c_constraints old_indication) 
      )  prec_l) in
     Private.constructor_opt pwb [] new_l ;; 
  
  let from_exhaustive_list_of_solutions all_sols =
     let forced_elts = i_fold_intersect all_sols in 
     BM(forced_elts,[0,Small_mold.constructor all_sols forced_elts]) ;; 

  let from_single_solution sol = BM(sol,[0,Small_mold.constructor [sol] sol]) ;; 

  let in_short_sized_case pwb =
    if ((Point_with_breadth.size pwb)>18)
    then raise(In_short_sized_case_exn(pwb))
    else 
      let all_sols = Point_with_breadth.solutions pwb 0 in   
      from_exhaustive_list_of_solutions all_sols  ;; 

  let rightmost_overflow_opt full_pwb left_mold =
      let (BM(_left_ext,left_data)) = left_mold in  
      let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
      and n = Point_with_breadth.max full_pwb in 
      let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
      let old_range = Image.image fst left_data in 
      let new_range = i_merge [0;1] (List.filter (fun i->(i_mem (i+1) old_range)) old_range) in   
      let get = Private.small_mold_at_index left_mold  in 
      let new_l = Image.image (
        fun i->
           (i,Small_mold.typical_union (c_constraints,n) (get(i+1)) (get i)) 
      )  (i_insert 0 new_range) in 
      Private.constructor_opt full_pwb [] new_l ;;     
  
  let rightmost_pivot_opt full_pwb left_mold =
    let (BM(left_ext,left_data)) = left_mold in  
    let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
    and n = Point_with_breadth.max full_pwb in 
    let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
    let old_range = Image.image fst left_data in 
    let new_range = i_merge [0;1] (List.filter (fun i->i_mem (i-1) old_range) old_range) in   
    let get = Private.small_mold_at_index left_mold in 
    if Small_mold.test_for_impossible_constraint (get 0) c_constraints
    then None 
    else  
    let new_l = Image.image (
          fun i->
           if i=0
           then (i,Small_mold.typical_selection (c_constraints,Some n) (get i) )
           else (i,Small_mold.typical_union (c_constraints,n) (get i) (get(i-1)) )
            ) 
    new_range in 
    Private.constructor_opt full_pwb (i_insert n left_ext) new_l  ;;         
  
  let select_opt pwb prec_mold (i,j,k) = 
    let (BM(prec_ext,prec_l)) = prec_mold in 
    let get = Private.small_mold_at_index prec_mold in 
    if Small_mold.test_for_impossible_constraint (get 0) [C[i;j;k]]
    then None 
    else  
    let new_l = Image.image (
          fun (t,old_data_for_t)->
           (t,Small_mold.typical_selection ([C[i;j;k]],None) old_data_for_t 
            ) 
    )  prec_l in 
  Private.constructor_opt pwb prec_ext new_l  ;; 
  
        let shallow sols = 
          BM([],[0,SM(sols,Fan.empty_one)])  ;; 
  
     let small_mold_at_index = Private.small_mold_at_index ;; 

       let solutions = Private.solutions ;;        
  
       let translate d (BM(ext,l)) = 
             BM(Image.image(fun t->t+d) ext,
              Image.image (fun (i,data)->(i,Small_mold.translate d data)) l
             );;
  
  
  end ;;    

module Handle = struct 

let translate d handle = 
   match handle with 
  Has_no_constraints
| Rightmost_pivot(_) -> handle 
| Select (i,j,k) -> Select (i+d,j+d,k+d)
| Rightmost_overflow (i,j,k) -> Rightmost_overflow (i+d,j+d,k+d)
| Fork (i,j,k) -> Fork(i+d,j+d,k+d) ;; 

end ;;  

module Piece_of_help = struct 

module Private = struct 

  let order_for_extra_solutions =((fun
    (v1:((int * solution list) list)) (v2:((int * solution list) list)) ->
        Total_ordering.standard v1 v2
  ): ((int * solution list) list) Total_ordering_t.t) ;; 

  let order_for_imposed_fans =((fun
    (v1:((int * fan) list)) (v2:((int * fan) list)) ->
        Total_ordering.standard v1 v2
  ): ((int * fan) list) Total_ordering_t.t) ;; 

  let order_for_extra_grooves =((fun
    (v1:(int list)) (v2:(int list)) ->
        Total_ordering.standard v1 v2
  ): (int list) Total_ordering_t.t) ;; 

  let order_for_fourtuples =
      Total_ordering.quadruple_product 
        Point_with_breadth.order  order_for_extra_solutions  order_for_imposed_fans  order_for_extra_grooves ;; 
  
  let to_uple gr = (gr.beneficiary,gr.extra_solutions,gr.imposed_fans,gr.extra_grooves_for_fork)

  let order =((fun
    (v1:piece_of_help) (v2:piece_of_help) ->
      order_for_fourtuples (to_uple v1) (to_uple v2)
  ): piece_of_help Total_ordering_t.t) ;; 

end ;;  

let extra_solutions_at_level help i = 
  match List.assoc_opt i help.extra_solutions with 
  (Some extra_sols) -> extra_sols 
  | None -> [] ;; 

let imposed_fan_at_level help i = List.assoc_opt i  help.imposed_fans ;; 

let order = Private.order ;; 

end ;;  

module Help = struct 

module Private = struct


let replace_perhaps original replacer_opt = match replacer_opt with 
  None -> original 
  |Some replacer -> replacer ;;

let apply_individual_help_except_extra_grooves help mold =
    let (BM(ext,old_data)) = mold in 
    let old_range = Image.image fst old_data in
    let new_range = i_merge old_range (Image.image fst help.imposed_fans) in 
    let new_data = Image.image (
      fun i->
         let (SM(old_sols,old_fan)) = Mold.small_mold_at_index mold i in 
         (i,SM(
           il_merge old_sols (Piece_of_help.extra_solutions_at_level help i ),
           replace_perhaps old_fan (Piece_of_help.imposed_fan_at_level help i) ))
    ) new_range in 
    BM(ext,new_data) ;; 
    
let apply_help_except_extra_grooves helpers pwb mold =
   match List.find_opt (fun help->help.beneficiary = pwb) helpers with 
      None -> mold 
     |Some help -> apply_individual_help_except_extra_grooves help mold ;; 

let extra_grooves helpers pwb = 
  match List.find_opt (fun help->help.beneficiary = pwb) helpers with 
      None -> []
     |Some help -> help.extra_grooves_for_fork ;; 
  
let rec get_opt pwb = function 
  [] -> None 
  | piece :: others ->
    match Point_with_breadth.order pwb piece.beneficiary with 
    Total_ordering_result_t.Lower -> None
   |Total_ordering_result_t.Greater -> get_opt pwb others  
   |Total_ordering_result_t.Equal -> Some piece ;;  

let institute_fan helpers pwb (FRR l) =
    match get_opt pwb helpers with 
    None -> let piece = { 
              beneficiary = pwb;
              extra_solutions = [];
              imposed_fans  = l;
              extra_grooves_for_fork = [];
            } in 
            Ordered.insert Piece_of_help.order piece helpers
   | Some old_piece ->
     let new_piece = {
          old_piece with 
          imposed_fans = l
     } in 
     Image.image (fun piece -> if piece.beneficiary=pwb then new_piece else piece) helpers ;;

     

end ;;

let apply_help_except_extra_grooves = Private.apply_help_except_extra_grooves ;; 
let extra_grooves = Private.extra_grooves ;;
let institute_fan = Private.institute_fan ;; 

end ;;  

module Flexible_grocery = struct 

  module Private = struct
  
    let handle_order = ((fun handle1 handle2 ->Total_ordering.standard handle1 handle2 
    ): handle Total_ordering_t.t);; 
  
    let mold_order = ((fun mold1 mold2 ->Total_ordering.standard mold1 mold2 
    ): mold Total_ordering_t.t);; 
  
    let hm_order = Total_ordering.product handle_order mold_order ;;
   
    let order = Total_ordering.product Point_with_breadth.order hm_order ;; 
   
    let rec get_opt key (Flg l) = match l with 
     [] -> None 
     | (key2,val2) :: others ->
        match Point_with_breadth.order key key2 with 
         Total_ordering_result_t.Lower -> None
        |Total_ordering_result_t.Greater -> get_opt key (Flg others) 
        |Total_ordering_result_t.Equal -> Some val2 ;;  
      
    let insert new_key data = Ordered.insert order new_key data ;; 
    let merge data1 data2 = Ordered.merge order data1 data2 ;; 
    let sort data = Ordered.sort order data ;; 

    
     end ;;
  
  let add (Flg l) pwb pair = 
    Flg(Private.insert (pwb,pair) l) ;;
   
  
  let add_if_it_has_constraints flg pwb pair =
    if Point_with_breadth.has_no_constraint pwb 
    then flg
    else add flg pwb pair;;

  let add_several (Flg l) pairs=
  Flg(Private.merge (Private.sort pairs) l) ;;
   
  
  let get_opt = Private.get_opt ;;   
  

end ;;

module Shortened_grocery = struct
  
  module Private = struct 

   let empty_one = {
   sg_helpers = [];
   sg_pair_level = [];
   sg_triple_level  = [];
   } ;;

  let immediate_eval_opt fgr pwb = 
    if Point_with_breadth.has_no_constraint pwb 
    then let domain = Point_with_breadth.supporting_set pwb in 
         Some(Has_no_constraints,
           Help.apply_help_except_extra_grooves (fgr.sg_helpers) pwb (Mold.discrete domain)) 
    else     
    let (FIS(n,scr)) = Point_with_breadth.support pwb 
    and w = Point_with_breadth.width pwb 
    and b = Point_with_breadth.breadth pwb in 
    let wpair = (w,scr) in
    match List.assoc_opt wpair fgr.sg_pair_level with 
    Some (f) -> let (handle,mold) =f b n in 
                Some(handle,mold)    
  | None ->
    let wtriple = (w,scr,b) 
    and n =  Point_with_breadth.max  pwb  in 
    match List.assoc_opt wtriple fgr.sg_triple_level with 
      Some (f) -> let (handle,mold) =f n in 
                  Some(handle,mold)    
    | None -> None ;;    
  
    let institute_fan fgr pwb frr =
      {
        fgr with
        sg_helpers = (Help.institute_fan (fgr.sg_helpers) pwb frr)
      } ;; 


  end ;; 

 let empty_one =  Private.empty_one ;; 
 let immediate_eval_opt = Private.immediate_eval_opt ;; 
 let institute_fan = Private.institute_fan ;;

end ;;  



module Conversion = struct

module Private = struct

let no_constraints_quote = "no_cstr" ;;  
let all_constraints_quote = "all_cstr" ;;
let some_constraints_quote = "some_cstr" ;;

let qc_to_string = function 
   Some_constraints -> some_constraints_quote
 | All_constraints -> all_constraints_quote ;;
  
exception Qc_of_string_exn of string ;; 

let qc_of_string qc_string= 
  try List.assoc qc_string
   [some_constraints_quote,Some_constraints;
    all_constraints_quote,All_constraints] with 
  _ -> raise(Qc_of_string_exn(qc_string))  
;; 

let point_with_breadth_to_t1 = function 
  No_constraint(FIS(n,scr)) -> (no_constraints_quote,n,scr,0,0)
  |Usual(qc,P(FIS(n,scr),W w),b) -> (qc_to_string qc,n,scr,w,b) ;; 
       
let point_with_breadth_of_t1 (variant_string,n,scr,w,b) =
    if variant_string=no_constraints_quote  then  No_constraint(FIS(n,scr)) else 
    Usual(qc_of_string variant_string,P(FIS(n,scr),W w),b) ;;   

let has_no_constraint_quote = "has_no_cstr" ;;
let rightmost_overflow_quote = "overflow" ;;
let rightmost_pivot_quote = "pivot" ;;
let select_quote = "select" ;;
let fork_quote = "fork" ;;


let handle_to_t2 = function 
 Has_no_constraints -> (has_no_constraint_quote,0,0,0) 
|Rightmost_overflow(u,v,n) ->  (rightmost_overflow_quote,u,v,n) 
|Rightmost_pivot(W w) -> (rightmost_pivot_quote,w,0,0) 
|Select (i,j,k) -> (select_quote,i,j,k)  
|Fork (i,j,k) -> (fork_quote,i,j,k) ;;  

exception Handle_of_t2_exn of string ;;

let handle_of_t2 (variant_string,i,j,k)= 
  try List.assoc variant_string
   [
    has_no_constraint_quote,Has_no_constraints;
    rightmost_overflow_quote,Rightmost_overflow(i,j,k);
    rightmost_pivot_quote,Rightmost_pivot(W i);
    select_quote,Select(i,j,k);
    fork_quote,Fork(i,j,k);
   ] with 
  _ -> raise(Handle_of_t2_exn(variant_string))  
;; 
  
let small_mold_to_t3 (SM(sols,F rays)) = (sols,rays) ;;
let small_mold_of_t3 (sols,rays)  = (SM(sols,F rays)) ;;

let indexed_small_mold_to_t4 (idx,sm) = 
  let (sols,rays) = small_mold_to_t3 sm in (idx,sols,rays) ;;

let indexed_small_mold_of_t4 (idx,sols,rays)= 
  (idx,small_mold_of_t3(sols,rays)) ;;


let mold_to_t5 (BM(ext,l))=(ext,Image.image indexed_small_mold_to_t4 l) ;;

let mold_of_t5 (ext,l)=BM(ext,Image.image indexed_small_mold_of_t4  l) ;;  

let pwb_hm_to_t6 (pwb,(handle,mold)) =
  let (variant_string_for_pwb,n,scr,w,b) = point_with_breadth_to_t1 pwb
  and (variant_string_for_handle,i,j,k) = handle_to_t2 handle 
  and (ext,l) = mold_to_t5 mold in 
  (variant_string_for_pwb,n,scr,w,b,variant_string_for_handle,i,j,k,ext,l) ;;

let pwb_hm_of_t6 
(variant_string_for_pwb,n,scr,w,b,variant_string_for_handle,i,j,k,ext,l) =
  let pwb = point_with_breadth_of_t1 (variant_string_for_pwb,n,scr,w,b) 
  and handle = handle_of_t2 (variant_string_for_handle,i,j,k)  
  and mold  = mold_of_t5 (ext,l) in 
  (pwb,(handle,mold));;
  
let flg_to_t7 (Flg l)=
  Image.image pwb_hm_to_t6 l ;; 

let flg_of_t7 l=
  Flg(Image.image pwb_hm_of_t6 l) ;; 

let intlist_to_string l =
    "["^(String.concat ";" (Image.image string_of_int l))^"]" ;;

let intlistlist_to_string ll =
    "["^(String.concat ";" (Image.image intlist_to_string ll))^"]" ;;


let t4_to_string (idx,sols,rays)= 
   "("^(string_of_int idx)^","
      ^(intlistlist_to_string sols)^","
      ^(intlistlist_to_string rays)^")" ;; 
   

let t6_to_string 
(variant_string_for_pwb,n,scr,w,b,variant_string_for_handle,i,j,k,ext,ll) =
   "("^(Strung.enclose variant_string_for_pwb)^","
      ^(string_of_int n)^","
      ^(intlist_to_string scr)^","
      ^(string_of_int w)^","
      ^(string_of_int b)^","
      ^(Strung.enclose variant_string_for_handle)^","
      ^(string_of_int i)^","
      ^(string_of_int j)^","
      ^(string_of_int k)^","
      ^(intlist_to_string ext)^","
      ^"["^(String.concat ";" (Image.image t4_to_string ll))^"])"
    ;;


let flg_to_string flg = 
  let tab = String.make 6 ' ' in
  let temp1 = Image.image (fun elt->tab^(t6_to_string elt))  (flg_to_t7 flg) in 
   "Conv"^"ersion.flg_of_t7([\n"^(String.concat ";\n" temp1)^"\n])" ;;

end ;; 

let flg_of_t7 = Private.flg_of_t7 ;; 
let flg_to_string = Private.flg_to_string ;;

end ;;  

module Instituted_shortened_grocery = struct

  module Private = struct 
  
    let main_ref = ref Shortened_grocery.empty_one ;; 

    let institute_fan pwb fan =
        let old_val = !main_ref in 
        let new_val = Shortened_grocery.institute_fan old_val pwb fan in 
        let _ = (main_ref:=new_val) in 
        () ;;

    institute_fan (No_constraint(FIS(2,[]))) 
    (FRR
    [(0, F [[1; 2]]);
     (1, F [[1]; [2]])]) ;; 

  end ;; 

  let main_ref = Private.main_ref ;;
  

end ;;  


module Precomputed_overchain = struct 

   (* the lists declared here are not exact chains but
     merely overchains, i.e. sequences [s(1);s(2);...] where
     for each i, the predecessor of s(i+1) is some s(j) for j<=i 
     (and not necessarily s(i) as it would be for an exact chain)

   *)

  exception Chain_not_computed_yet of point_with_breadth ;; 

  module Private = struct
    
    let pwb_order = Point_with_breadth.order ;;
    let pwb_list_order = Total_ordering.lex_compare pwb_order ;;
    let pair_order = Total_ordering.product pwb_order pwb_list_order ;;
    
    let data_ref =ref [];;
    let insert_new_pair pair = data_ref:= (Ordered.insert pair_order pair (!data_ref));; 

    let rec helper_for_all_subchains (treated,(pwb,chain_for_pwb),to_be_treated) = 
        match to_be_treated with 
         [] -> List.rev( (pwb,chain_for_pwb) :: treated )
        | pwb2 :: others ->
          helper_for_all_subchains ((pwb,chain_for_pwb)::treated,(pwb2,chain_for_pwb@[pwb2]),others) ;; 

    let all_subchains = function 
      [] -> []
      | a1 :: others1 -> (
         match others1 with 
         [] -> []
         | a2 :: others2 -> 
          helper_for_all_subchains ([],(a2,[a1;a2]),others2) 
      ) ;;

    let declare_overchain chain =
       List.iter insert_new_pair (all_subchains chain) ;; 

    let all_constraints n scr w = Point_with_breadth.all_constraints(P(FIS(n,scr),W w)) ;;

    let level3 n = all_constraints n [] 3 ;;
    
    declare_overchain (
      (Int_range.scale level3 2 6)@
      [all_constraints 7 [] 2;level3 7]@
      [Point_with_breadth.constructor 8 [] (W 2) 1;level3 8]@
      (Int_range.scale level3 9 14)@
      [Point_with_breadth.constructor 15 [] (W 2) 8;level3 15]@
      [Point_with_breadth.constructor 16 [] (W 2) 9;level3 16]@
      (Int_range.scale level3 17 22)@
      [Point_with_breadth.constructor 23 [] (W 2) 16]
    ) ;;
    
  
  end ;;

  let overchain pwb =
     match List.assoc_opt pwb (!(Private.data_ref)) with 
      None -> raise(Chain_not_computed_yet(pwb))
    | Some answer -> answer ;;
    
  let chained_points () = Image.image fst (!(Private.data_ref)) ;; 

  let declare_overchain = Private.declare_overchain ;;   

end ;;  

module Impatient = struct 

  module Private = struct

    let combined_eval_opt low_level pwb = 
      match Shortened_grocery.immediate_eval_opt (!(Instituted_shortened_grocery.main_ref)) pwb  with 
      Some (answer) -> let (handle,mold) =answer in 
                       Some(handle,mold)    
    | None -> 
         (  
          match Flexible_grocery.get_opt pwb low_level with 
          Some (answer) -> let (handle,mold) =answer in 
                           Some(handle,mold)    
        | None -> None
           ) ;;    

  let eval_opt low_level pwb =  
    let (d,grounded_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
     match combined_eval_opt low_level grounded_pwb with 
      None -> None
     |Some(handle,mold) -> Some(Handle.translate d handle,Mold.translate d mold) ;; 

  end ;;

  let eval_opt = Private.eval_opt ;; 

end ;;


module Minimal_effort = struct 

  module Private = struct

 
 let rec immediate_for_several_opt low_level (treated,to_be_treated) = 
    match to_be_treated with 
    [] -> Some(List.rev treated) 
    | pwb :: others ->
     (
      match Impatient.eval_opt low_level pwb with 
        None -> None 
        |Some (_,mold) -> immediate_for_several_opt low_level (mold::treated,others)
     ) ;;



let fork_opt low_level pwb =
  match Point_with_breadth.usual_decomposition_opt pwb with 
  None -> None
 |Some(prec_pwb,C cstr) ->
    match Impatient.eval_opt low_level prec_pwb with 
    None -> None
  | Some(_,prec_mold) -> 
        let ext = Mold.forced_elements prec_mold in 
        let nth_cstr = (fun k->List.nth cstr (k-1)) in 
        let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
        let (i,j,k) = ijk in 
        if not(i_is_included_in [i;j;k] ext)
        then None
        else
          let fgr = (!(Instituted_shortened_grocery.main_ref)) in 
          let grooves = i_insert k (Help.extra_grooves fgr.sg_helpers pwb) in 
              let pointed_pwbs = Image.image (Point_with_breadth.remove_element pwb) grooves in 
              (match immediate_for_several_opt low_level ([],pointed_pwbs) with 
                None -> None
              | Some(pointed_molds) -> 
                 (
                  match Mold.fork_opt pwb prec_mold pointed_molds ijk with 
                     None -> None 
                    |Some mold -> Some(Fork(i,j,k),mold) 
                 )
              );;   


           
let rightmost_overflow_opt low_level pwb  = 
 let n = Point_with_breadth.max pwb in 
 let left_pwb = Point_with_breadth.remove_element pwb n in 
 match Impatient.eval_opt low_level left_pwb with 
     None -> None
   | Some(_,left_mold) -> 
  let left_ext = Mold.forced_elements left_mold 
  and complements = Point_with_breadth.complementary_pairs pwb in  
  match List.find_opt  (
              fun (u,v) -> i_is_included_in [u;v] left_ext 
    ) complements with
    None -> None 
   |Some(u,v) ->
      (
        match Mold.rightmost_overflow_opt pwb left_mold with 
           None -> None 
          |Some mold -> Some(Rightmost_overflow(u,v,n),mold) 
       )
    ;;

           

 let rightmost_pivot_opt low_level pwb  = 
   let n = Point_with_breadth.max pwb in 
   let left_pwb = Point_with_breadth.remove_element pwb n in 
   match Impatient.eval_opt low_level left_pwb with 
       None -> None
     | Some(_,left_mold) -> 
     (match Mold.rightmost_pivot_opt pwb left_mold with 
       None -> None 
      |Some mold -> Some(Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),mold) 
     )    ;;  
 
let select_opt low_level pwb =
  match Point_with_breadth.usual_decomposition_opt pwb with 
  None -> None
 |Some(prec_pwb,C cstr) ->
    match Impatient.eval_opt low_level prec_pwb with 
    None -> None
  | Some(_,prec_mold) -> 
             let nth_cstr = (fun k->List.nth cstr (k-1)) in 
             let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
             let (i,j,k) = ijk in 
             (
                 match Mold.select_opt pwb prec_mold ijk with 
                    None -> None 
                   |Some mold -> Some(Select(i,j,k),mold) 
             );;        

let eval_opt low_level pwb =
  match Impatient.eval_opt low_level pwb with 
  Some(answer0) -> Some answer0
 | None -> 
  if Point_with_breadth.has_no_constraint pwb 
  then Some(Has_no_constraints,Mold.discrete(Point_with_breadth.supporting_set pwb))
  else    
  (match rightmost_pivot_opt low_level pwb with 
   Some(answer1) -> Some answer1
  | None -> 
    (
      match rightmost_overflow_opt low_level pwb with 
        Some(answer2) -> Some answer2
      | None -> 
        (
          match select_opt low_level pwb with 
            Some(answer3) -> Some answer3
          | None -> fork_opt low_level pwb
        )     
    )
   ) ;;
    
  let update_if_possible low_level pwb =  
     match Impatient.eval_opt low_level pwb with 
     Some pair1 -> (Some pair1,low_level) 
     | None -> 
      (
        match eval_opt low_level pwb with 
       Some pair2 -> (Some pair2,Flexible_grocery.add_if_it_has_constraints low_level pwb pair2) 
     | None -> (None,low_level)
      ) ;;  
 
  exception Pusher_for_scale_walking_exn1 ;;
  exception Pusher_for_scale_walking_exn2 ;;

  let pusher_for_scale_walking (treated,low_level,to_be_treated) =
        match to_be_treated with 
         [] ->raise Pusher_for_scale_walking_exn1
        |pwb :: others ->
            let (answer_opt,new_low_level) = update_if_possible low_level pwb in 
            match answer_opt with  
               None -> raise Pusher_for_scale_walking_exn2
              |Some answer -> ((pwb,answer)::treated,new_low_level,others) ;;
    

  let rec iterator_for_scale_walking triple =
    let (treated,low_level,to_be_treated) = triple in 
    if to_be_treated = [] then (None,Some(treated),low_level) else
    try  iterator_for_scale_walking(pusher_for_scale_walking triple) with 
    Pusher_for_scale_walking_exn2 ->  (Some (List.hd to_be_treated),None,low_level) ;;
    
  let walk_scale low_level to_be_treated = iterator_for_scale_walking ([],low_level,to_be_treated) ;;


  end ;;  

  let eval_opt = Private.eval_opt ;;
  let update_if_possible = Private.update_if_possible ;; 
  let walk_scale = Private.walk_scale ;; 

end ;;  


module Painstaking = struct 

  exception Push_exn ;; 

  exception Should_never_happen_in_push_1_exn of point_with_breadth;; 

  exception First_problem of point_with_breadth ;; 

  module Private = struct
    let painstaking_ref = ref (Flg[]) ;;

let pusher (low_level,to_be_treated) = match to_be_treated with 
   [] -> raise Push_exn 
  | pwb :: others ->
  let (opt_pair1,low_level1) = Minimal_effort.update_if_possible low_level pwb in 
  if opt_pair1<>None then (low_level1,others) else 
  let (nonisolated_pwb,isolated_elts) = Point_with_breadth.nonisolated_version pwb in 
  if isolated_elts<>[]
  then let (opt_pair6,low_level6) = Minimal_effort.update_if_possible low_level1 nonisolated_pwb in 
       if opt_pair6=None then (low_level6,(Point_with_breadth.projection nonisolated_pwb)::to_be_treated) else 
        let (_handle,nonisolated_mold) = Option.get opt_pair6 in
        let mold = Mold.add_isolated_set nonisolated_mold isolated_elts in 
       (Flexible_grocery.add low_level6 pwb (Rightmost_pivot(W 0),mold),others) 
  else
  let opt2 = Point_with_breadth.usual_decomposition_opt pwb in 
  if opt2=None then raise(Should_never_happen_in_push_1_exn(pwb)) else
  let (_,C cstr) = Option.get opt2 in 
  let nth_cstr = (fun k->List.nth cstr (k-1)) in 
  let (i,j,k)=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
  let pwb_i = Point_with_breadth.remove_element pwb i 
  and pwb_j = Point_with_breadth.remove_element pwb j 
  and pwb_k = Point_with_breadth.remove_element pwb k in 
  let (opt_pair3,low_level3) = Minimal_effort.update_if_possible low_level1 pwb_i in 
  if opt_pair3=None then (low_level3,(Point_with_breadth.projection pwb_i)::to_be_treated) else
  let (_,mold_i) = Option.get opt_pair3 in 
  let (opt_pair4,low_level4) = Minimal_effort.update_if_possible low_level3 pwb_j in 
  if opt_pair4=None then (low_level4,(Point_with_breadth.projection pwb_j)::to_be_treated) else
  let (_,mold_j) = Option.get opt_pair4 in 
  let (opt_pair5,low_level5) = Minimal_effort.update_if_possible low_level4 pwb_k in 
  if opt_pair5=None then (low_level5,(Point_with_breadth.projection pwb_k)::to_be_treated) else
  let (_,mold_k) = Option.get opt_pair5 in  
  let candidates = il_fold_merge(Image.image Mold.solutions [mold_i;mold_j;mold_k]) in 
  let (_,final_sols) = Max.maximize_it_with_care List.length candidates in 
  let answer=(Fork(i,j,k),Mold.shallow final_sols) in
  (Flexible_grocery.add  low_level5 pwb answer,others) ;;

let rec iterator (low_level,to_be_treated) =
    if to_be_treated = [] 
    then low_level
    else iterator(pusher (low_level,to_be_treated)) ;;
    
let eval_in_unblocked_mode  pwb =
    let new_low_level = iterator (!painstaking_ref,[pwb]) in 
    let _ = (painstaking_ref:=new_low_level) in 
    Option.get(Impatient.eval_opt new_low_level pwb);;    

  let eval_in_blocked_mode pwb = 
    let (low_level,to_be_treated) = pusher (!painstaking_ref,[pwb]) in
    if to_be_treated=[]
    then  Option.get(Impatient.eval_opt (low_level) pwb)
    else  raise(First_problem(List.hd to_be_treated)) ;; 

  let blocked_mode_ref = ref false ;;    

  let eval pwb =
     if !blocked_mode_ref 
     then eval_in_blocked_mode pwb
     else eval_in_unblocked_mode pwb ;;    

  let store data = 
       (painstaking_ref:=Flexible_grocery.add_several (!painstaking_ref) data);;

    

  end ;;

  let current_mode () = (!(Private.blocked_mode_ref)) ;;
  let eval = Private.eval  ;; 
  let measure pwb = let (_,mold) = eval pwb in List.length(List.hd (Mold.solutions mold)) ;; 
  let set_blocked_mode mode = (Private.blocked_mode_ref:=mode) ;; 
  let store = Private.store ;; 
      

end ;;


module Extra_constraints = struct
  
  type paint_with_extra_constraints = PWEC of point * constraint_t list ;;
  
  module Private = struct

  let usual_decomposition_for_bare_point_opt_for_pwc pt =
    match Point.highest_constraint_opt pt with 
     None -> None 
    |Some (C l)-> 
      let current_b = List.nth l 0 in 
      let effective_w=(List.nth l 1)-current_b in 
      let candidates=Int_range.descending_scale (
          fun b->C[b;b+effective_w;b+2*effective_w]
      ) 1 (current_b-1) in 
      let (P(fis,_)) = pt in 
      let domain = Finite_int_set.to_usual_int_list fis in 
      let selected_candidates = List.filter (
         fun (C l)->i_is_included_in l domain
      ) candidates in 
      Some(PWEC(P(fis,W(effective_w-1)),selected_candidates), C l)
      ;;  
  
    let usual_decomposition_opt_for_pwc (PWEC(pt,l_cstr)) = 
        match l_cstr with 
      [] -> usual_decomposition_for_bare_point_opt_for_pwc pt 
    |highest :: others -> Some(PWEC(pt,others),highest) ;; 
  
  
  let pwc_has_no_constraint pwc = ((usual_decomposition_opt_for_pwc pwc)=None);;
  
  
  let remove_element_on_pwc (PWEC(pt,l_cstr)) t =
    let smaller_pt = Point.remove_element pt t in 
    PWEC(smaller_pt,List.filter (fun (C l)->not(i_mem t l)) l_cstr) ;; 
  
  let remove_rightmost_element_on_pwc pt_with_constraints =
    let (PWEC(pt,_)) = pt_with_constraints in 
    remove_element_on_pwc  pt_with_constraints (Point.max pt) ;; 
  
  let remove_rightmost_element_but_keep_constraints_on_pwc (PWEC(possibly_nonstandard_pt,l_cstr)) =
     let pt = Point.standardize possibly_nonstandard_pt in 
     let (W w) = Point.width pt and n=Point.max pt in 
     let smaller_pt = Point.remove_element pt n in
     let constraints1 = 
        (Int_range.descending_scale (fun d->[n-(2*d);n-d]) w 1)@
        (Image.image (fun (C l)->i_outsert n l) l_cstr) in
     let constraints2 = Ordered_misc.minimal_elts_wrt_inclusion constraints1 in 
     let (singletons,constraints3) = List.partition (fun cstr->List.length(cstr)=1) constraints2 in
     let removable_subset = List.flatten singletons in 
     let final_pt = Point.remove_elements smaller_pt removable_subset 
     and final_constraints = Image.image (fun l->C l) constraints3 in 
     PWEC(final_pt,final_constraints) ;;  
  
     let easy_measure_opt (PWEC(pt,l_cstr)) = 
       let (_,mold) = Painstaking.eval (Point_with_breadth.all_constraints pt) in 
       let sols = Mold.solutions mold in 
       let selected_sols = Constraint.select_in_list l_cstr sols in 
       if selected_sols<>[]
       then Some(List.length(List.hd selected_sols))
       else None ;;


     let measure_for_pwc = Memoized.recursive (fun 
     old_f pwc-> 
       let (PWEC(pt,l_cstr)) = pwc in 
       if l_cstr = []
       then Painstaking.measure(Point_with_breadth.all_constraints pt)
       else 
       let pwc2 = remove_rightmost_element_on_pwc pwc
       and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
       let m2 = (match easy_measure_opt pwc2 with Some m->m |None->old_f pwc2) 
       and m3 = (match easy_measure_opt pwc3 with Some m->m |None->old_f pwc3) in  
       max(m2)(m3+1)
     );;
  
  let standard_solution_for_pwc  = Memoized.recursive (fun 
    old_f pwc-> 
    let (PWEC(pt,_l_cstr)) = pwc in 
    if pwc_has_no_constraint pwc 
    then Point.supporting_set pt 
    else  
    let pwc2 = remove_rightmost_element_on_pwc pwc
    and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
    if (measure_for_pwc pwc2)>=((measure_for_pwc pwc3)+1)
    then old_f(pwc2)
    else (old_f(pwc3))@[Point.max pt]  
  );;
  
  let pwb_to_extra_constraints pwb = 
    let pt = Point_with_breadth.point pwb 
    and b = Point_with_breadth.breadth pwb in 
    if b = 0 then PWEC(pt,[]) else 
    let (W w)=Point.width pt 
    and domain = Point.supporting_set pt in 
    let all_constraints = Int_range.descending_scale 
       (fun k->C[k;k+(w+1);k+2*(w+1)]) b 1 in 
    let meaningful_constraints = List.filter(
      fun (C cstr) -> i_is_included_in cstr domain
    )  all_constraints in 
    PWEC(pt,meaningful_constraints) ;;


  end ;;


  let of_point_with_breadth = Private.pwb_to_extra_constraints ;;   
  let measure = Private.measure_for_pwc ;;  
  let remove = Private.remove_element_on_pwc ;; 
  let standard_solution = Private.standard_solution_for_pwc ;;  
  

  end ;;    

module Compute_standard_solution = struct 

  let compute = Memoized.make(fun pwb->
    Extra_constraints.standard_solution
       (Extra_constraints.of_point_with_breadth pwb)
  )  ;;
  
end;;
  
module Decompose = struct 

  module Private = struct
  
  let test_for_individual_rightmost_overflow left_pwb m (u,v) = 
        List.for_all (fun t->Painstaking.measure(Point_with_breadth.remove_element left_pwb t)=m-1) [u;v] ;;
    
  let test_for_rightmost_overflow pwb m =
        let pairs =  Point_with_breadth.complementary_pairs pwb 
        and left_pwb = Point_with_breadth.remove_element pwb (Point_with_breadth.max pwb) in 
        List.find_opt (test_for_individual_rightmost_overflow left_pwb m) pairs ;; 
  
  
  let decompose = Memoized.make(fun pwb->
    match Point_with_breadth.usual_decomposition_opt pwb with 
        None -> (Has_no_constraints,None)
        |Some(prec_pwb,C cstr) -> 
          let n = Point_with_breadth.max pwb in
          let left_pwb = Point_with_breadth.remove_element pwb n in 
          let pwc = Extra_constraints.of_point_with_breadth pwb in  
          if Extra_constraints.measure(Extra_constraints.remove pwc n)=
            Extra_constraints.measure(pwc)-1   
          then (Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),
                Some left_pwb)
          else   
          let m = Painstaking.measure pwb in   
            ( match test_for_rightmost_overflow pwb m with 
           (Some(u,v))->(Rightmost_overflow(u,v,n), Some left_pwb)
           |None ->   
         
         let nth = (fun k->List.nth cstr (k-1)) in 
         if Painstaking.measure prec_pwb = m
         then 
              (Select(nth 1,nth 2,nth 3),Some prec_pwb)
         else (Fork(nth 1,nth 2,nth 3),Some prec_pwb)   
  ))  ;;     
  
 
  end ;;
  
  
  
  let decompose = Private.decompose ;; 
  
  end ;;

  module Chain = struct 
    
   
   module Private = struct 

    let hints = ref ([
    ]: (point_with_breadth * point_with_breadth list) list) ;; 
    
    let main_hashtbl = ((Hashtbl.create 100): (point_with_breadth, point_with_breadth list) Hashtbl.t) ;; 
   
    let pwb_order = Point_with_breadth.order ;;

    let pwblist_order = Total_ordering.lex_compare pwb_order ;;

    let order_for_hints = Total_ordering.product pwb_order pwblist_order ;; 

    let rec adhoc_assoc pwb l =
        match l with 
        [] -> []
        |(pwb2,answer2) :: others ->
          (
            match pwb_order pwb pwb2 with 
              Total_ordering_result_t.Lower -> []
              |Total_ordering_result_t.Equal -> answer2
              |Total_ordering_result_t.Greater -> adhoc_assoc pwb others 
          ) ;;

    let needed_extras pwb = adhoc_assoc pwb (!hints) ;; 

     let rec compute_chain pwb = 
      let (handle,prec_pwb_opt) = Decompose.decompose pwb in 
      if handle = Has_no_constraints 
      then [pwb]
      else
      let  prec_pwb = Option.get prec_pwb_opt in   
       match Hashtbl.find_opt main_hashtbl pwb with 
       (Some old_answer) -> old_answer
       |None ->
      let ancestors = Ordered.sort pwb_order (prec_pwb :: (needed_extras pwb)) in 
      let temp1 = Image.image compute_chain ancestors in 
      let temp2 = Ordered.fold_merge pwb_order temp1 in 
      let final_answer = Ordered.insert pwb_order pwb temp2 in 
      let _ = Hashtbl.add main_hashtbl pwb final_answer in 
      final_answer ;;  
     
  let add_hint pwb pwb2 = 
    let old_hints_for_pwb = needed_extras pwb in 
    if old_hints_for_pwb=[]
    then hints := (Ordered.insert order_for_hints (pwb,[pwb2]) (!hints)) 
    else
    let new_hints_for_pwb = Ordered.insert pwb_order pwb2 old_hints_for_pwb in 
    hints := Image.image (
      fun pair->if fst(pair)=pwb then (pwb,new_hints_for_pwb) else pair
    ) (!hints) ;;
        
  let add_hint_and_reset_hashtbl pwb pwb2 =
     (add_hint pwb pwb2; Hashtbl.clear main_hashtbl) ;;

  end ;;

 let add_hint = Private.add_hint_and_reset_hashtbl ;;    
 let chain = Private.compute_chain ;; 

  end ;;   



  module Expanded_painstaking = struct 

    exception Next_problem_in_decompose_exn ;;

    let next_problem_in_decompose pwb = 
      let old_mode = Painstaking.current_mode() in
     try (fun _->
      Painstaking.set_blocked_mode old_mode;
      raise Next_problem_in_decompose_exn)(
       let _ = Painstaking.set_blocked_mode true in  
       Decompose.decompose pwb) with 
     Painstaking.First_problem(pwb2) -> 
      Painstaking.set_blocked_mode old_mode;
      pwb2 ;;  

    exception Next_problem_in_eval_exn ;;

      let next_problem_in_eval pwb = 
        let old_mode = Painstaking.current_mode() in
       try (fun _->
        Painstaking.set_blocked_mode old_mode;
        raise Next_problem_in_eval_exn)(
          let _ = Painstaking.set_blocked_mode true in    
        Painstaking.eval pwb) with 
       Painstaking.First_problem(pwb2) -> 
        Painstaking.set_blocked_mode old_mode;
        pwb2 ;;     

  end ;;
  

  module Impatient_on_chains = struct 

  exception Eval_exn of point_with_breadth ;;   

  module Private = struct 

  let eval_opt pwb = 
      let (_opt_counterexample,opt_list,_new_low_level) 
              = Minimal_effort.walk_scale (Flg[])  (Precomputed_overchain.overchain pwb) in     
           match opt_list  with 
      None -> None
    |Some data -> List.assoc_opt pwb data ;;

  let eval pwb = match eval_opt pwb with 
    Some answer -> answer 
    |None -> raise(Eval_exn(pwb)) ;;    
    
  let store_all_half_impatient_expansions () =
      let half_impatient_expansions =  Image.image (
        fun pwb ->(pwb,eval pwb)
      ) (Precomputed_overchain.chained_points ()) in 
       Painstaking.store half_impatient_expansions ;; 

  let declare_overchain  data =     
       (Precomputed_overchain.declare_overchain data;
       store_all_half_impatient_expansions ());;

  (* let predecessor_in_chain_opt pwb  = function 
       Has_no_constraints -> None 
      |Rightmost_overflow(u,v,n) ->  
      |Rightmost_pivot(W w) -> (rightmost_pivot_quote,w,0,0) 
      |Select (i,j,k) -> (select_quote,i,j,k)  
      |Fork (i,j,k) -> (fork_quote,i,j,k) ;; *)


  end ;;   

  let declare_overchain = Private.declare_overchain ;; 
  let eval = Private.eval ;; 
  let eval_opt = Private.eval_opt ;; 

  end ;;   


  module Diagnose = struct 
      
    module Private = struct 
      
      exception Nothing_to_diagnose_exn ;;
      exception Has_no_constraints_not_diagnosable_exn ;; 
      exception Half_impatient_eval_exn of point_with_breadth ;; 
      
        let diagnose_rightmost_overflow low_level (u,v,_n)  left_pwb = 
           match Impatient.eval_opt low_level left_pwb with 
           None -> Missing_subcomputation("rightmost_overflow",left_pwb)
           |Some (_,mold) -> 
            let missing_forced_elts = i_setminus [u;v] (Mold.forced_elements mold) in 
            Missing_fan("rightmost_overflow",left_pwb,0,F[missing_forced_elts]) ;; 
      
       let diagnose_rightmost_pivot low_level pwb left_pwb = 
        match Impatient.eval_opt low_level left_pwb with 
        None -> Missing_subcomputation("rightmost_pivot",left_pwb)
        |Some (_,_) ->
          let the_sol = Compute_standard_solution.compute pwb 
          and n = Point_with_breadth.max pwb in
          Missing_solution("rightmost_pivot",left_pwb,i_outsert n the_sol) ;; 
      
        let diagnose_select low_level pwb prec_pwb = 
          match Impatient.eval_opt low_level prec_pwb with 
        None -> Missing_subcomputation("select",prec_pwb)
        |Some (_,_) ->
            let the_sol = Compute_standard_solution.compute pwb in
            Missing_solution("select",prec_pwb,the_sol) ;;  
      
        let diagnose_fork low_level (i,j,k) pwb prec_pwb = 
          match Impatient.eval_opt low_level prec_pwb with 
           None -> Missing_subcomputation("fork",prec_pwb)
           |Some (_,prec_mold) -> 
          let missing_forced_elts = i_setminus [i;j;k] (Mold.forced_elements prec_mold) in 
          if missing_forced_elts <> []
          then Missing_fan("fork",prec_pwb,0,F[missing_forced_elts])   
          else   
          let the_sol = Compute_standard_solution.compute pwb in 
          let l = List.find (fun t->not(i_mem t the_sol)) [k;j;i] in
          let shorter_pwb = Point_with_breadth.remove_element pwb l in 
          match  Impatient.eval_opt low_level shorter_pwb with 
           None -> Missing_subcomputation("fork",shorter_pwb)
           |Some (_,mold) -> 
             let sols = Mold.solutions mold in 
             if not(List.mem the_sol sols)
             then Missing_solution("fork",shorter_pwb,the_sol)
             else Missing_switch_in_fork(l,pwb) ;;  
            
      
      let diagnose_precedent low_level pwb =
        let (handle,pwb2_opt) = Decompose.decompose pwb in 
        let pwb2=(match pwb2_opt with 
           Some pwb3 -> pwb3
           | None -> Point_with_breadth.constructor 0 [] (W 0) 0) in 
          match handle with
           Has_no_constraints -> raise(Has_no_constraints_not_diagnosable_exn)
          |Rightmost_overflow(u,v,n) ->  diagnose_rightmost_overflow low_level (u,v,n) pwb2
          |Rightmost_pivot(_) -> diagnose_rightmost_pivot low_level pwb pwb2
          |Select (_,_,_) -> diagnose_select low_level pwb pwb2 
          |Fork (i,j,k) -> diagnose_fork low_level (i,j,k) pwb pwb2 ;;  
      
    
      
      let inspect_along_chain 
          low_level pwb = 
        let (opt_counterexample,opt_list,new_low_level) =
        Minimal_effort.walk_scale low_level (Precomputed_overchain.overchain pwb) in 
         match opt_counterexample  with 
         None -> let data = Option.get(opt_list) in 
                 Smooth(List.assoc pwb data,(fun ()->data))
         |Some precedent -> Counterexample_found(precedent,
           diagnose_precedent  new_low_level precedent);; 
  
   
    
    end ;;

   let inspect_along_chain = 
    Private.inspect_along_chain (Flg[]);;
      

  end ;;


module Fan_related_requirement = struct 

  exception No_pullback_without_a_constraint_exn ;; 

  module Private = struct

    let adjust_required_fan pwb level_in_mold original_required_fan = 
      let mold = snd(Option.get(Impatient_on_chains.eval_opt pwb)) in 
      let older_fan = Mold.fan_at_index mold level_in_mold in 
      let pre_adjusted_requirement = Fan.combine_two_conditions older_fan original_required_fan in 
      let all_sols = Point_with_breadth.solutions pwb level_in_mold in 
      Fan.canonical_container all_sols pre_adjusted_requirement;;

  let pull_on_single_requirement (n,complements_for_n) handle (level_in_mold,required_fan)= 
   let draft = (
     match handle with  
    Has_no_constraints -> raise(No_pullback_without_a_constraint_exn)
  | Rightmost_pivot(_) -> let (with_n,without_n) = Fan.with_or_without required_fan n complements_for_n in 
                          [level_in_mold,with_n;level_in_mold-1,without_n]
  | Rightmost_overflow (_,_,_) -> 
                          let (with_n,without_n) = Fan.with_or_without required_fan n complements_for_n in 
                          [level_in_mold+1,with_n;level_in_mold,without_n]
  | Select (i,j,k) ->  let relaxed_fan = Fan.union required_fan (F[[i;j;k]])   in
                        [level_in_mold,relaxed_fan]
  | Fork (i,j,k) -> let relaxed_fan = Fan.union required_fan (F[[i;j;k]])   in
                        [level_in_mold+1,relaxed_fan]
    ) in
    List.filter (fun (level,_fan)->level>=0) draft  ;; 
      
  let pull_on_several_requirements (FRR(old_requirements)) pair handle = 
     let temp1 = List.flatten(Image.image (pull_on_single_requirement pair handle) old_requirements) in 
     let indices = i_sort(Image.image fst temp1) in 
     List.filter_map (
       fun level_in_mold ->
        let requirements =List.filter_map (
           fun (level_in_mold2,fan) ->
             if level_in_mold2 = level_in_mold 
             then Some fan
            else None  
        ) temp1 in 
        let final_fan = Fan.combine_conditions requirements in 
        if final_fan = Fan.empty_one
        then None  
        else Some(level_in_mold,Fan.combine_conditions requirements)
     ) indices ;;
  
   let pull_and_adjust pwb handle pwb_before old_frr  = 
      let n = Point_with_breadth.max pwb 
      and comps = Point_with_breadth.complementary_pairs pwb in 
      let possibly_not_adjusted_reqs = pull_on_several_requirements old_frr (n,comps) handle in 
      let adjusted_reqs = Image.image (
        fun (level_in_mold,fan) ->
           (level_in_mold,adjust_required_fan pwb_before level_in_mold fan)
      ) possibly_not_adjusted_reqs in 
      FRR adjusted_reqs ;;  

    let rec iterator_for_fan_pulling (pwb,handle,pwb_before,frr,treated) = 
      let frr_before = pull_and_adjust pwb handle  pwb_before frr 
      and (handle_before,pwb_much_before_opt) = Decompose.decompose pwb_before in 
      let updated = (pwb_before,frr_before)::treated in 
      match pwb_much_before_opt with 
       None -> updated
      |Some(pwb_much_before) -> iterator_for_fan_pulling (pwb_before,handle_before,pwb_much_before,frr_before,updated) ;;
          
    let pull_all_fans pwb original_required_fan =
      let frr=FRR[0,adjust_required_fan pwb 0 original_required_fan]
      and (handle,pwb_before_opt) = Decompose.decompose pwb in 
      match pwb_before_opt with 
      None -> [pwb,frr]
     |Some(pwb_before) -> iterator_for_fan_pulling (pwb,handle,pwb_before,frr,[pwb,frr]) ;; 

end ;;

let constructor pwb level_in_mold original_required_fan = 
     FRR[0,Private.adjust_required_fan pwb level_in_mold original_required_fan];;
let pull pwb old_frr  = 
      let (handle,pwb_before_opt) = Decompose.decompose pwb in 
     let pwb_before = Option.get pwb_before_opt in 
    Private.pull_and_adjust pwb handle pwb_before old_frr ;; 
let pull_all_fans = Private.pull_all_fans ;; 

end ;;   