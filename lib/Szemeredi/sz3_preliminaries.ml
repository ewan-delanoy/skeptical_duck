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

type point_with_breadth = Sz3_types.point_with_breadth = PWB of point * int ;; 

type handle = Sz3_types.handle = 
   Discrete
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

type grocery = Sz3_types.grocery = {
  helpers : piece_of_help list;
  pair_level : ((width * int list) * (int -> int -> handle * mold)) list;
  triple_level : ((width * int list * int) * (int -> handle * mold)) list;
  low_level : (point_with_breadth * (handle * mold)) list;
} ;;
  


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


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_insert = Ordered.insert il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_min= Ordered.min il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;


let fis_order = ((fun (FIS(n1,scr1)) (FIS(n2,scr2)) ->
    let trial1 = i_order n1 n2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    let trial2 = i_order (List.length scr2) (List.length scr1) in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else
      Total_ordering.silex_for_intlists scr1 scr2
  ): finite_int_set Total_ordering_t.t);;

(* let point_order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    fis_order fis1 fis2
  ): point Total_ordering_t.t);;
let point_insert = Ordered.insert point_order ;; *)

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
   List.filter (fun candidate->List.for_all( fun (C cstr) ->not(i_is_included_in cstr candidate)) l_cstr ) 
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

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

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

  let to_usual_int_list = Private.to_usual_int_list ;; 

  let translate d fis = 
    let domain = Private.to_usual_int_list fis in
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

  let is_discrete pt = (highest_constraint_opt(pt)=None) ;; 

  let is_nontrivial (P(fis,w)) =
    let domain = Finite_int_set.to_usual_int_list fis in
    ((Find_highest_constraint.below_maximal_width w domain) <> None);;

  let max (P(fis,_w)) = Finite_int_set.max fis  ;; 

  let order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    fis_order fis1 fis2
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

module Private = struct

let small_standardization pwb =
    let (PWB(pt,b)) = pwb in
    let (P(fis,W old_w)) = pt in 
    let support = Point.supporting_set pt in 
    match List.find_opt (fun t->i_is_included_in [t;t+(old_w+1);t+2*(old_w+1)] support) (List.rev(Int_range.range 1 b)) with
    Some(b0)->PWB(pt,b0)
    |None ->
    match Point.highest_constraint_opt pt with  
     None -> PWB(P(fis,W 0),0)
    |Some(C cstr) -> 
      let nth = (fun k->List.nth cstr (k-1)) in 
      let w = W((nth 2)-(nth 1)-1) in 
      PWB(P(fis,w),nth 1);;
    ;; 

let usual_decomposition_opt pwb =
  let (PWB(pt,b)) = small_standardization pwb in 
  let (P(_,W w)) = pt in 
  if b=0
  then None 
  else Some(small_standardization(PWB(pt,b-1)),C[b;b+(w+1);b+2*(w+1)]);;

  (* when d<0, this opeartion makes sense 
  only if the lowest element is >|d|. *)    
  let translate d (PWB(pwc,b)) = 
    (*
      when b+d<0, there is no extra constraint, 
      and this is equivalent to setting the new b to 0S    
    *) 
    let new_b = max(0)(b+d) in
      PWB(Point.translate d pwc,new_b) ;;

    let supporting_set (PWB(pt,_)) = Point.supporting_set pt ;;     

   let complementary_pairs pwb =
     let (PWB(P(FIS(n,_scr),W max_w),b)) = pwb 
     and domain = supporting_set pwb in 
     let candidates = Int_range.range 1 (max_w+1) in 
     List.filter_map (
         fun w->
            let u = n-2*w and v=n-w in 
           if not(i_is_included_in [u;v] domain) then None else
           if w<=max_w then Some(u,v) else 
           if u<=b then Some(u,v) else None  
     ) candidates ;;

   let obstructions (PWB(P(FIS(n,_scr),W wmax),b)) = 
     let obstructions_for_width = (fun w->Int_range.scale(fun t->[t;t+w;t+2*w]) 1 (n-2*w)) in 
       List.flatten((Int_range.scale obstructions_for_width 1 wmax)@
       [Int_range.scale(fun t->[t;t+(wmax+1);t+2*(wmax+1)]) 1 b]);;

   let solutions pwb offset =
      let temp1 = il_sort(List_again.power_set (supporting_set pwb)) in 
      let obstrs = obstructions pwb in
      let temp2 = List.filter (fun y->List.for_all (fun obs->not(i_is_included_in obs y))obstrs) temp1 in 
      let m = List.length(List.hd(List.rev temp2)) in 
      List.filter (fun y->List.length(y)=m-offset) temp2 ;; 

   let rightmost_largest_width (PWB(P(FIS(n,_scr),W w),b)) =
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

    let nonisolated_version (PWB(P(fis,W wmax),b)) = 
       let domain = Finite_int_set.to_usual_int_list fis in 
       let (non_isolated,isolated) = List.partition (individual_test_for_non_isolation wmax b domain) domain in 
       let new_fis = Finite_int_set.of_usual_int_list non_isolated in 
       (PWB(P(new_fis,W wmax),b),isolated);;
    
    let remove_element (PWB(P(fis,W wmax),b)) elt = 
      let new_fis = Finite_int_set.remove_element fis elt in 
      let new_domain = Finite_int_set.to_usual_int_list new_fis in 
      match Find_highest_constraint.below_width_bound_pair (W wmax,b) new_domain with
      None -> PWB(P(new_fis,W 0),0)
      |Some(C cstr)->
        let nth = (fun k->List.nth cstr (k-1)) in 
        let new_wmax = (nth 2)-(nth 1)-1 in 
        PWB(P(new_fis,W new_wmax),nth 1);;


    let order = ((fun (PWB(pt1,b1)) (PWB(pt2,b2)) ->
          let trial1 = Point.order pt1 pt2 in 
          if trial1<>Total_ordering_result_t.Equal then trial1 else 
          i_order b1 b2
        ): point_with_breadth Total_ordering_t.t);;    

    let subset_is_admissible (PWB(pt,b)) subset =
      if not(Point.subset_is_admissible pt subset)
      then false 
      else 
        let (P(_,W w)) = pt in 
        List.for_all (fun t->not(i_is_included_in [t;t+(w+1);t+2*(w+1)] subset)) (Int_range.range 1 b);;    


end ;;  

let breadth (PWB(_pt,b))= b ;;
let complementary_pairs = Private.complementary_pairs ;;
let decompose_wrt_translation pwb = 
  let (PWB(pt,_b)) = pwb in 
  let (d,_) = Point.decompose_wrt_translation pt in 
  (d,Private.translate (-d) pwb);; 

let everything_but_the_size (PWB(P(FIS(_n,scr),w),b)) = (w,scr,b) ;;  
let is_discrete pwb = (Private.usual_decomposition_opt pwb=None) ;; 
let max (PWB(pt,_b)) = Point.max pt ;;
let nonisolated_version = Private.nonisolated_version ;;
let order = Private.order ;; 
let projection pwb = snd(decompose_wrt_translation pwb);;
let remove_element = Private.remove_element ;;
let rightmost_largest_width = Private.rightmost_largest_width ;; 
let size (PWB(P(FIS(n,_scr),_w),_b)) = n ;;
let solutions = Private.solutions ;;  
let subset_is_admissible= Private.subset_is_admissible ;; 
let supporting_set (PWB(pt,_)) = Point.supporting_set pt ;;
let supporting_set = Private.supporting_set ;; 
let translate = Private.translate ;; 
let usual_decomposition_opt = Private.usual_decomposition_opt ;; 
let width (PWB(pt,_b))= Point.width pt ;;

end ;;  


module Fan = struct 
  
  module Private = struct

  let distribute (F rays) addendum= F(Image.image (i_merge addendum) rays) ;;  

  end ;;  

  let constructor ll =
    let sorted_ll = il_sort ll in 
    F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  let core (F ll) = i_fold_intersect ll ;; 

  let empty_one = F [[]] ;;

  let impose l_cstr (F rays) =  F(Constraint.select_in_list l_cstr rays);;
  
  let impose_and_distribute  (l_cstr,addendum) fan = 
      Private.distribute ( impose l_cstr fan) addendum ;;

  let translate d (F rays) = F(Image.image (fun ray->Image.image (fun t->t+d) ray) rays);;

  let union (F ll1) (F ll2) = constructor(ll1@ll2) ;; 

end ;;   

module Small_mold = struct 

  let add_isolated_set (SM(sols,fan)) isolated_set =
    let add = i_merge isolated_set in 
    SM(Image.image add sols,Fan.impose_and_distribute ([],isolated_set) fan) ;;

  let empty_one = SM([],Fan.empty_one);;

  let impose l_cstr (SM(sols,fan)) =
     SM(Constraint.select_in_list l_cstr sols,Fan.impose l_cstr fan) ;; 

  let translate d (SM(sols,fan))  = 
    SM(Image.image (Image.image (fun t->t+d)) sols,Fan.translate d fan) ;;  

  let typical_union (complements,automatically_distributed) (SM(sols1,fan1)) (SM(sols2,fan2))= 
    let for_a_solution_set = (fun sols->Image.image (i_merge automatically_distributed)
        (Constraint.select_in_list complements sols))
    and for_a_fan = Fan.impose_and_distribute (complements,automatically_distributed) in 
    SM(il_merge (for_a_solution_set sols1) sols2,
       Fan.union (for_a_fan fan1) fan2  
      ) ;;

end ;;
 

module Mold = struct 

  
  
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
  
  let forced_elements (BM(ext,_)) = ext ;; 
  
  let fork_opt pwb prec_mold pointed_ones (i,j,k) = 
      let (BM(_prec_ext,prec_l)) = prec_mold in 
      let c_constraints = [C[i;j;k]] 
      and sols = il_fold_merge(Image.image Private.solutions pointed_ones) in 
      let (SM(_,old_fan1)) = Private.small_mold_at_index prec_mold 1 in 
      let first_fan = Fan.impose c_constraints old_fan1 in  
      let new_l= (0,SM(sols,first_fan))::(List.filter_map (
            fun (i,old_indication)->
               if i=0 then None else
               Some(i-1,Small_mold.impose c_constraints old_indication) 
      )  prec_l) in
     Private.constructor_opt pwb [] new_l ;; 
  
  let rightmost_overflow_opt full_pwb (BM(_old_ext,old_data)) = 
      let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
      and n = Point_with_breadth.max full_pwb in 
      let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
      let old_range = Image.image fst old_data in 
      let new_range = List.filter (fun i->(i_mem (i+1) old_range)) old_range in   
      let get = (fun i->List.assoc i old_data) in 
      let small_mold_at_level1 = (
         if List.mem 1 old_range then get 1 else Small_mold.empty_one
      ) in
      let get_next_one = (fun i->
         if i=0 then small_mold_at_level1 else get(i+1)
      ) in
      let new_l = Image.image (
        fun i->
           (i,Small_mold.typical_union (c_constraints,[n]) (get_next_one i) (get i)) 
      )  (i_insert 0 new_range) in 
      Private.constructor_opt full_pwb [] new_l ;;     
  
  let rightmost_pivot_opt full_pwb (BM(old_ext,old_data)) = 
        let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
        and n = Point_with_breadth.max full_pwb in 
        let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
        let old_range = Image.image fst old_data in 
        let new_range = List.filter (fun i->(i=0)||(i_mem (i-1) old_range)) old_range in   
        let  get = (fun i->List.assoc i old_data) in 
        let get_preceding_one = (fun i->
          if i=0 then Small_mold.empty_one else get(i-1)
       ) in
        let new_l = Image.image (
          fun i->
           (i,Small_mold.typical_union (c_constraints,[n]) (get 0) (get_preceding_one i)
            ) 
        )  new_range in 
        Private.constructor_opt full_pwb (i_insert n old_ext) new_l  ;;         
  
        let select_opt pwb (BM(prec_ext,prec_l)) (i,j,k) = 
          let new_l = Image.image (
          fun (t,old_data_for_t)->
           (t,Small_mold.typical_union ([C[i;j;k]],[]) old_data_for_t Small_mold.empty_one
            ) 
        )  prec_l in 
        Private.constructor_opt pwb prec_ext new_l  ;; 
  
        let shallow sols = 
          BM([],[0,SM(sols,Fan.empty_one)])  ;; 
  
     let small_mold_at_index (BM(_,l)) i =
         match List.assoc_opt i l with 
          Some small_mold -> small_mold 
         |None -> SM([],Fan.empty_one) ;; 

       let solutions = Private.solutions ;;        
  
       let translate d (BM(ext,l)) = 
             BM(Image.image(fun t->t+d) ext,
              Image.image (fun (i,data)->(i,Small_mold.translate d data)) l
             );;
  
  
  end ;;    

module Handle = struct 

let translate d handle = 
   match handle with 
  Discrete
| Rightmost_pivot(_) -> handle 
| Select (i,j,k) -> Select (i+d,j+d,k+d)
| Rightmost_overflow (i,j,k) -> Rightmost_overflow (i+d,j+d,k+d)
| Fork (i,j,k) -> Fork(i+d,j+d,k+d) ;; 

end ;;  

module Help = struct 

module Private = struct

let extra_solutions_at_index help i = 
    match List.assoc_opt i help.extra_solutions with 
    (Some extra_sols) -> extra_sols 
    | None -> [] ;; 

let imposed_fan_at_index help i = List.assoc_opt i  help.imposed_fans ;; 

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
           il_merge old_sols (extra_solutions_at_index help i ),
           replace_perhaps old_fan (imposed_fan_at_index help i) ))
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
  

end ;;

let apply_help_except_extra_grooves = Private.apply_help_except_extra_grooves ;; 
let extra_grooves = Private.extra_grooves ;;

end ;;  


module Grocery = struct 



 module Low_level = struct 

  let handle_order = ((fun handle1 handle2 ->Total_ordering.standard handle1 handle2 
  ): handle Total_ordering_t.t);; 

  let mold_order = ((fun mold1 mold2 ->Total_ordering.standard mold1 mold2 
  ): mold Total_ordering_t.t);; 

  let hm_order = Total_ordering.product handle_order mold_order ;;
 
  let order = Total_ordering.product Point_with_breadth.order hm_order ;; 
 
  let rec get_opt key = function 
   [] -> None 
   | (key2,val2) :: others ->
      match Point_with_breadth.order key key2 with 
       Total_ordering_result_t.Lower -> None
      |Total_ordering_result_t.Greater -> get_opt key others  
      |Total_ordering_result_t.Equal -> Some val2 ;;  
    
  let insert new_key data = Ordered.insert order new_key data ;; 
  

 end ;; 

let empty_one = {
  helpers = [];
  pair_level = [];
  triple_level  = [];
  low_level = [];
}  ;;  

let add_to_low_level grc pwb pair = {
   grc with 
   low_level = Low_level.insert (pwb,pair) (grc.low_level);
} ;;

let add_to_low_level_if_nondiscrete grc pwb pair =
  if Point_with_breadth.is_discrete pwb 
  then grc
  else add_to_low_level grc pwb pair;;

let immediate_eval_opt grc_ref pwb = 
  if Point_with_breadth.is_discrete pwb 
  then let domain = Point_with_breadth.supporting_set pwb in 
       Some(Discrete,
         Help.apply_help_except_extra_grooves ((!grc_ref).helpers) pwb (Mold.discrete domain)) 
  else     
  let (PWB(P(FIS(n,scr),w),b)) = pwb in  
  let wpair = (w,scr) in
  match List.assoc_opt wpair (!grc_ref).pair_level with 
  Some (f) -> let (handle,mold) =f b n in 
              Some(handle,mold)    
| None ->
  let wtriple = (w,scr,b) 
  and n =  Point_with_breadth.size  pwb  in 
  match List.assoc_opt wtriple (!grc_ref).triple_level with 
    Some (f) -> let (handle,mold) =f n in 
                Some(handle,mold)    
  | None ->
     (  
      match Low_level.get_opt pwb (!grc_ref).low_level with 
      Some (answer) -> let (handle,mold) =answer in 
                       Some(handle,mold)    
    | None -> None
       ) ;;    


end ;;  


 

module Generic = struct 

module Impatient = struct 

  module Private = struct

 let immediate_opt grc pwb =  
    let (d,grounded_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
     match Grocery.immediate_eval_opt (ref grc) grounded_pwb with 
      None -> None
     |Some(handle,mold) -> Some(Handle.translate d handle,Mold.translate d mold) ;; 
 
 let rec immediate_for_several_opt grc (treated,to_be_treated) = 
    match to_be_treated with 
    [] -> Some(List.rev treated) 
    | pwb :: others ->
     (
      match immediate_opt grc pwb with 
        None -> None 
        |Some (_,mold) -> immediate_for_several_opt grc (mold::treated,others)
     )



let fork_opt grc pwb =
  match Point_with_breadth.usual_decomposition_opt pwb with 
  None -> None
 |Some(prec_pwb,C cstr) ->
    match immediate_opt grc prec_pwb with 
    None -> None
  | Some(_,prec_mold) -> 
        let ext = Mold.forced_elements prec_mold in 
        let nth_cstr = (fun k->List.nth cstr (k-1)) in 
        let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
        let (i,j,k) = ijk in 
        if not(i_is_included_in [i;j;k] ext)
        then None
        else
              let grooves = i_insert k (Help.extra_grooves grc.helpers pwb) in 
              let pointed_pwbs = Image.image (Point_with_breadth.remove_element pwb) grooves in 
              (match immediate_for_several_opt grc ([],pointed_pwbs) with 
                None -> None
              | Some(pointed_molds) -> 
                 (
                  match Mold.fork_opt pwb prec_mold pointed_molds ijk with 
                     None -> None 
                    |Some mold -> Some(Fork(i,j,k),mold) 
                 )
              );;   


           
let rightmost_overflow_opt grc pwb  = 
 let n = Point_with_breadth.max pwb in 
 let left_pwb = Point_with_breadth.remove_element pwb n in 
 match immediate_opt grc left_pwb with 
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

           

 let rightmost_pivot_opt grc pwb  = 
   let n = Point_with_breadth.max pwb in 
   let left_pwb = Point_with_breadth.remove_element pwb n in 
   match immediate_opt grc left_pwb with 
       None -> None
     | Some(_,left_mold) -> 
     (match Mold.rightmost_pivot_opt pwb left_mold with 
       None -> None 
      |Some mold -> Some(Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),mold) 
     )    ;;  
 
let select_opt grc pwb =
  match Point_with_breadth.usual_decomposition_opt pwb with 
  None -> None
 |Some(prec_pwb,C cstr) ->
    match immediate_opt grc prec_pwb with 
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

let eval_opt grc pwb =
  match immediate_opt grc pwb with 
  Some(answer0) -> Some answer0
 | None -> 
  if Point_with_breadth.is_discrete pwb 
  then Some(Discrete,Mold.discrete(Point_with_breadth.supporting_set pwb))
  else    
  (match rightmost_pivot_opt grc pwb with 
   Some(answer1) -> Some answer1
  | None -> 
    (
      match rightmost_overflow_opt grc pwb with 
        Some(answer2) -> Some answer2
      | None -> 
        (
          match select_opt grc pwb with 
            Some(answer3) -> Some answer3
          | None -> fork_opt grc pwb
        )     
    )
   ) ;;
    
  let update_if_possible grc pwb =  
     match immediate_opt grc pwb with 
     Some pair1 -> (Some pair1,grc) 
     | None -> 
      (
        match eval_opt grc pwb with 
       Some pair2 -> (Some pair2,Grocery.add_to_low_level_if_nondiscrete grc pwb pair2) 
     | None -> (None,grc)
      ) ;;

  

  let rec iterator_for_scale_walking (treated,grc,to_be_treated) =
    match to_be_treated with 
     [] ->(None,Some(treated),grc)
    |pwb :: others ->
        let (answer_opt,new_grc) = update_if_possible grc pwb in 
        match answer_opt with  
           None -> (Some pwb,None,grc)
          |Some answer -> iterator_for_scale_walking ((pwb,answer)::treated,new_grc,others) ;;

  let walk_scale grc to_be_treated = iterator_for_scale_walking ([],grc,to_be_treated) ;;


  end ;;  

  let eval_opt = Private.eval_opt ;;
  let immediate_opt = Private.immediate_opt ;;
  let update_if_possible = Private.update_if_possible ;; 
  let walk_scale = Private.walk_scale ;; 

end ;;  

module Painstaking = struct 

exception Push_exn ;; 

exception Should_never_happen_in_push_1_exn of point_with_breadth;; 

let pusher (grc,to_be_treated) = match to_be_treated with 
   [] -> raise Push_exn 
  | pwb :: others ->
  let (opt_pair1,grc1) = Impatient.update_if_possible grc pwb in 
  if opt_pair1<>None then (grc1,others) else 
  let (nonisolated_pwb,isolated_elts) = Point_with_breadth.nonisolated_version pwb in 
  if isolated_elts<>[]
  then let (opt_pair6,grc6) = Impatient.update_if_possible grc1 nonisolated_pwb in 
       if opt_pair6=None then (grc6,(Point_with_breadth.projection nonisolated_pwb)::to_be_treated) else 
        let (_handle,nonisolated_mold) = Option.get opt_pair6 in
        let mold = Mold.add_isolated_set nonisolated_mold isolated_elts in 
       (Grocery.add_to_low_level grc6 pwb (Rightmost_pivot(W 0),mold),others) 
  else
  let opt2 = Point_with_breadth.usual_decomposition_opt pwb in 
  if opt2=None then raise(Should_never_happen_in_push_1_exn(pwb)) else
  let (_,C cstr) = Option.get opt2 in 
  let nth_cstr = (fun k->List.nth cstr (k-1)) in 
  let (i,j,k)=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
  let pwb_i = Point_with_breadth.remove_element pwb i 
  and pwb_j = Point_with_breadth.remove_element pwb j 
  and pwb_k = Point_with_breadth.remove_element pwb k in 
  let (opt_pair3,grc3) = Impatient.update_if_possible grc1 pwb_i in 
  if opt_pair3=None then (grc3,(Point_with_breadth.projection pwb_i)::to_be_treated) else
  let (_,mold_i) = Option.get opt_pair3 in 
  let (opt_pair4,grc4) = Impatient.update_if_possible grc3 pwb_j in 
  if opt_pair4=None then (grc4,(Point_with_breadth.projection pwb_j)::to_be_treated) else
  let (_,mold_j) = Option.get opt_pair4 in 
  let (opt_pair5,grc5) = Impatient.update_if_possible grc4 pwb_k in 
  if opt_pair5=None then (grc5,(Point_with_breadth.projection pwb_k)::to_be_treated) else
  let (_,mold_k) = Option.get opt_pair5 in  
  let candidates = il_fold_merge(Image.image Mold.solutions [mold_i;mold_j;mold_k]) in 
  let (_,final_sols) = Max.maximize_it_with_care List.length candidates in 
  let answer=(Fork(i,j,k),Mold.shallow final_sols) in
  (Grocery.add_to_low_level grc5 pwb answer,others) ;;

let rec iterator (grc,to_be_treated) =
    if to_be_treated = [] 
    then grc
    else iterator(pusher (grc,to_be_treated)) ;;
    
let eval grc_ref pwb =
    let new_grc = iterator (!grc_ref,[pwb]) in 
    let _ = (grc_ref:=new_grc) in 
    Option.get(Impatient.immediate_opt new_grc pwb);;    


end ;;   

end ;; 

module Impatient = struct 

  module Private = struct
    let impatient_ref = ref Grocery.empty_one ;;
  end ;;

  let eval_opt = Generic.Impatient.eval_opt (!(Private.impatient_ref)) ;; 
  let immediate_opt = Generic.Impatient.immediate_opt (!(Private.impatient_ref)) ;; 
  let update_if_possible pwb =
     let (opt_answer,new_grc) = Generic.Impatient.update_if_possible (!(Private.impatient_ref)) pwb in 
     let _ = (Private.impatient_ref:=new_grc) in 
     opt_answer ;;   
  let walk_scale scale = 
    let (opt_counterexample,opt_list,_new_grc) 
       = Generic.Impatient.walk_scale (!(Private.impatient_ref)) scale in 
    (* let _ = (Private.impatient_ref:=new_grc) in *) 
    (opt_counterexample,opt_list) ;;   

end ;;

module Painstaking = struct 

  module Private = struct
    let painstaking_ref = ref Grocery.empty_one ;;
  end ;;

  let eval = Generic.Painstaking.eval Private.painstaking_ref ;; 
  let measure pwb = let (_,mold) = eval pwb in List.length(List.hd (Mold.solutions mold)) ;; 

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
  
  
  let pwc_is_discrete pwc = ((usual_decomposition_opt_for_pwc pwc)=None);;
  
  
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
  
  
     let measure_for_pwc = Memoized.recursive (fun 
     old_f pwc-> 
       let (PWEC(pt,l_cstr)) = pwc in 
       if l_cstr = []
       then Painstaking.measure(PWB(pt,0))
       else 
       let pwc2 = remove_rightmost_element_on_pwc pwc
       and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
       max(old_f pwc2)((old_f pwc3)+1)
  );;
  
  let standard_solution_for_pwc  = Memoized.recursive (fun 
    old_f pwc-> 
    let (PWEC(pt,_l_cstr)) = pwc in 
    if pwc_is_discrete pwc 
    then Point.supporting_set pt 
    else  
    let pwc2 = remove_rightmost_element_on_pwc pwc
    and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
    if (measure_for_pwc pwc2)>=((measure_for_pwc pwc3)+1)
    then old_f(pwc2)
    else (old_f(pwc3))@[Point.max pt]  
  );;
  
  let pwb_to_extra_constraints (PWB(pt,b)) =
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
        None -> (Discrete,PWB(P(FIS(0,[]),W 0),0))
        |Some(prec_pwb,C cstr) -> 
          let n = Point_with_breadth.max pwb in
          let left_pwb = Point_with_breadth.remove_element pwb n in 
          let pwc = Extra_constraints.of_point_with_breadth pwb in  
          if Extra_constraints.measure(Extra_constraints.remove pwc n)=
            Extra_constraints.measure(pwc)-1   
          then (Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),left_pwb)
          else   
          let m = Painstaking.measure pwb in   
            ( match test_for_rightmost_overflow pwb m with 
           (Some(u,v))->(Rightmost_overflow(u,v,n),left_pwb)
           |None ->   
         
         let nth = (fun k->List.nth cstr (k-1)) in 
         if Painstaking.measure prec_pwb = m
         then 
              (Select(nth 1,nth 2,nth 3),prec_pwb)
         else (Fork(nth 1,nth 2,nth 3),prec_pwb)   
  ))  ;;     
  
  let rec iterator_for_chain (treated,pwb_to_be_treated) =
     let (handle,prec_pwb) = decompose pwb_to_be_treated in 
     let newly_treated = (handle,prec_pwb) :: treated in 
     if handle = Discrete 
     then newly_treated
     else  iterator_for_chain (newly_treated,prec_pwb) ;;   
  
       ;;
        
  
  let chain_of_pairs pwb = iterator_for_chain ([],pwb) ;; 

  end ;;
  
  
  let chain pwb =  (Image.image snd (Private.chain_of_pairs pwb))@[pwb] ;; 
  let decompose = Private.decompose ;; 
  
  end ;;

  module Diagnose = struct 

    type diagnosis =
       Missing_forced_elements of (int list) * point_with_breadth 
      |Missing_solution of solution * point_with_breadth 
      |Missing_subcomputation of string * point_with_breadth 
      |Missing_switch_in_fork of int * point_with_breadth ;;
      
    exception Nothing_to_diagnose_exn ;;
    exception Discrete_not_diagnosable_exn ;; 
    
    module Private = struct
    
      let diagnose_rightmost_overflow (u,v,_n)  left_pwb = 
         match  Impatient.immediate_opt left_pwb with 
         None -> Missing_subcomputation("rightmost_overflow",left_pwb)
         |Some (_,mold) -> 
           Missing_forced_elements(i_setminus [u;v] (Mold.forced_elements mold),left_pwb) ;; 
    
     let diagnose_rightmost_pivot pwb left_pwb = 
        let the_sol = Compute_standard_solution.compute pwb 
        and n = Point_with_breadth.max pwb in
        Missing_solution(i_outsert n the_sol,left_pwb) ;; 
    
      let diagnose_select pwb prec_pwb = 
          let the_sol = Compute_standard_solution.compute pwb in
          Missing_solution(the_sol,prec_pwb) ;;  
    
      let diagnose_fork (i,j,k) pwb prec_pwb = 
        match  Impatient.immediate_opt prec_pwb with 
         None -> Missing_subcomputation("fork",prec_pwb)
         |Some (_,prec_mold) -> 
        let missing_forced_elts = i_setminus [i;j;k] (Mold.forced_elements prec_mold) in 
        if missing_forced_elts <> []
        then Missing_forced_elements(missing_forced_elts,prec_pwb)   
        else   
        let the_sol = Compute_standard_solution.compute pwb in 
        let l = List.find (fun t->not(i_mem t the_sol)) [k;j;i] in
        let shorter_pwb = Point_with_breadth.remove_element pwb l in 
        match  Impatient.immediate_opt shorter_pwb with 
         None -> Missing_subcomputation("fork",shorter_pwb)
         |Some (_,mold) -> 
           let sols = Mold.solutions mold in 
           if not(List.mem the_sol sols)
           then Missing_solution(the_sol,shorter_pwb)
           else Missing_switch_in_fork(l,pwb) ;;  
          
    
    let diagnose_precedent pwb =
      let (handle,pwb2) = Decompose.decompose pwb in 
        match handle with
         Discrete -> raise(Discrete_not_diagnosable_exn)
        |Rightmost_overflow(u,v,n) ->  diagnose_rightmost_overflow (u,v,n) pwb2
        |Rightmost_pivot(_) -> diagnose_rightmost_pivot pwb pwb2
        |Select (_,_,_) -> diagnose_select pwb pwb2 
        |Fork (i,j,k) -> diagnose_fork (i,j,k) pwb pwb2 ;;  
    
    
    let half_impatient_eval_opt pwb = 
      let (_opt_counterexample,opt_list) = Impatient.walk_scale (Decompose.chain pwb) in 
       match opt_list  with 
      None -> None
      |Some data -> List.assoc_opt pwb data ;;
        
    type chain_inspection_result =
       Smooth of (handle * mold) * (unit -> ((point_with_breadth * (handle * mold)) list))
       |Counterexample_found of point_with_breadth * diagnosis ;; 

    let inspect_along_chain pwb = 
      let (opt_counterexample,opt_list) = Impatient.walk_scale (Decompose.chain pwb) in 
       match opt_counterexample  with 
       None -> let data = Option.get(opt_list) in 
               Smooth(List.assoc pwb data,(fun ()->data))
       |Some precedent -> Counterexample_found(precedent,diagnose_precedent precedent);; 
    
    end ;;

   let half_impatient_eval_opt = Private.half_impatient_eval_opt ;;   
   let inspect_along_chain = Private.inspect_along_chain ;; 
  
  end ;;