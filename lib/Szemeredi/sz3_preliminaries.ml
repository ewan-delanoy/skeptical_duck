(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type breadth = Sz3_types.breadth = B of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type fan = Sz3_types.fan = F of extension_data list ;; 

type mold = Sz3_types.mold = M of (solution list) * fan ;;

type upper_bound_on_breadth = 
    Sz3_types.upper_bound_on_breadth = 
   Unrestricted |Up_to of breadth ;; 

type upper_bound_on_constraint = 
   Sz3_types.upper_bound_on_constraint = UBC of width * upper_bound_on_breadth ;; 

type key = 
   Sz3_types.key = Key of finite_int_set * upper_bound_on_constraint ;; 

type medium_hook = Sz3_types.medium_hook = Mh_cumulative of int | Mh_select of int * int *int | Mh_fork of int * int *int  ;; 

type simplified_key = int * (int list) * int * int ;;

type entry = Sz3_types.entry = E of simplified_key * (medium_hook * mold) ;;

type partially_polished = Sz3_types.partially_polished = PP of entry list ;; 

type small_polish = Sz3_types.small_polish =
     Add_entry of entry
    |Replace_entry_by of entry;; 

let i_order = Total_ordering.for_integers ;;
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

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;

let sk_order =((fun (n1,scr1,w1,b1) (n2,scr2,w2,b2) ->
   let try1 = Total_ordering.for_integers w1 w2 in 
   if try1 <> Total_ordering_result_t.Equal then try1 else 
   let try2 = Total_ordering.for_integers n1 n2 in 
   if try2 <> Total_ordering_result_t.Equal then try2 else  
   let try3 = Total_ordering.for_integers (List.length scr2) (List.length scr1) in 
   if try3 <> Total_ordering_result_t.Equal then try3 else  
   let try4 = Total_ordering.silex_for_intlists scr1 scr2 in 
   if try4 <> Total_ordering_result_t.Equal then try4 else   
   Total_ordering.for_integers b1 b2
) : simplified_key Total_ordering_t.t);;  
  

let hm_order = ((fun st1 st2->Total_ordering.standard st1 st2): (medium_hook * mold) Total_ordering_t.t);;

let entry_order = 
  ((fun (E(sk1,hm1))  (E(sk2,hm2))->
    Total_ordering.product sk_order hm_order (sk1,hm1) (sk2,hm2)): 
  entry Total_ordering_t.t);;

let entry_insert = Ordered.insert entry_order ;;
let entry_merge = Ordered.merge entry_order ;;
let entry_sort = Ordered.sort entry_order ;;


module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  

module Fan = struct 

   module Private = struct

  let constructor ll =
     let sorted_ll = il_sort ll in 
     F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  end ;;  
     
  let combine_two_conditions (F ll1) (F ll2) =
     let temp1 = Cartesian.product ll1 ll2 in 
     Private.constructor( Image.image (fun (x,y)->i_merge x y) temp1 );; 

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
      
    

    let insert ray (F rays) =  F(il_insert ray rays);;

    let insert_several more_rays (F rays) =  F(il_merge (il_sort more_rays) rays);;

    let is_stronger_than (F rays1) (F rays2) =
      List.for_all (fun ray1->List.exists (fun ray2->i_is_included_in ray2 ray1) rays2) rays1 ;;  

    let remove_vertex pivot (F rays) =
        Private.constructor (Image.image (i_outsert pivot) rays) ;;  

end ;;   

module Mold = struct 

let translate d (M(sols,F ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    M(Image.image tr sols,F(Image.image tr ext)) ;; 

end ;;


module Finite_int_set = struct 

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  end ;;

  let decompose_wrt_translation fis_domain = 
    let domain = Private.to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,Private.of_usual_int_list core_domain) ;; 

  let max (FIS(n,_)) = n ;; 

  let of_usual_int_list = Private.of_usual_int_list ;; 

  let remove_one_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       let new_z =  Private.to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;     
  
  
  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_one_element (FIS(3,[])) 3 ;;
  
  *)

  let to_usual_int_list = Private.to_usual_int_list ;; 


end ;;    


module Upper_bound_on_breadth = struct 

  let constructor b = 
     if b=0 
     then Unrestricted
     else Up_to(B b) ;; 
  
    exception Get_exn ;;
  
    let get = function 
      Unrestricted -> raise(Get_exn)
      |Up_to(B b) -> B b ;; 
  
    let deconstructor = function 
      Unrestricted -> 0
      |Up_to(B b) -> b ;; 

  let translate d = function  
    Unrestricted -> Unrestricted 
   |Up_to(B b) -> Up_to(B(b+d)) ;; 
  
  let allows upper_bound b = match upper_bound with 
  Unrestricted -> true 
  |Up_to(B bmax) -> (b<=bmax) ;; 

  
  end ;;  
  
module Upper_bound_on_constraint = struct 
  
    exception A_priori_bound_on_breadth_exn ;; 
    exception Two_steps_back_exn_1 of finite_int_set * upper_bound_on_constraint ;;
    exception Two_steps_back_exn_2 of finite_int_set * upper_bound_on_constraint ;;

  module Private = struct
  
  let rec finder_for_exact_width (W w,domain,to_be_treated) =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if i_is_included_in [p-2*w;p-w] domain 
       then Some (B(p-2*w))
       else finder_for_exact_width (W w,domain,others) ;;     
  
  let rec finder_for_maximal_width (W w,domain) =
   match finder_for_exact_width (W w,domain,List.rev domain) with 
   Some (breadth_max) -> Some(UBC(W w,Up_to breadth_max))
   |None ->
      if w<2 then None else 
      finder_for_maximal_width (W (w-1),domain) ;;  
  
      let a_priori_bound_on_breadth fis (UBC(W w,ub_on_breadth)) = 
        match ub_on_breadth with 
        Up_to(max_breadth) -> max_breadth
        |Unrestricted -> 
         let domain = Finite_int_set.to_usual_int_list fis in 
         if domain = [] 
         then raise A_priori_bound_on_breadth_exn 
         else B(List.hd(List.rev domain)-2*w)  ;; 
     
     let attained_upper_bound_opt_for_dissociated_data fis (W w) (B max_breadth) = 
       let domain = Finite_int_set.to_usual_int_list fis in 
       let temp1 = List.rev domain in 
       let candidates = List.filter (fun t->t<=max_breadth+2*w) temp1 in 
       if candidates=[] then None else 
        match finder_for_exact_width (W w,domain,candidates) with 
       Some (max_breadth2) -> Some(UBC(W w,Up_to max_breadth2))
         |None -> finder_for_maximal_width (W (w-1),domain) ;;  
  
      

      let predecessor_for_dissociated_data_opt fis (W w1) (B b1)= 
         if b1>1 
              then Some(W w1, B (b1-1))  
              else if w1=1 
                   then None 
                   else 
                    let n= Finite_int_set.max fis in 
                    let new_b = n-2*(w1-1) in 
                    if new_b < 1 
                    then None  
                    else Some(W (w1-1), B (n-2*(w1-1))) ;;    
      end ;;
  
let allows (UBC(W w,ub_on_breadth)) l=
      let a1=List.nth l 0 
      and a2=List.nth l 1 in 
      (a2-a1<=w) && (Upper_bound_on_breadth.allows ub_on_breadth a1);;  
  
  
let attained_upper_bound_opt fis ub_on_constraint = 
        let bmax = Private.a_priori_bound_on_breadth fis ub_on_constraint
        and (UBC(w,_ub_on_breadth)) = ub_on_constraint in 
        Private.attained_upper_bound_opt_for_dissociated_data fis w bmax;;  

let decrement ub_on_constraint =
    let (UBC(W w,_)) = ub_on_constraint in 
    UBC(W(w-1),Unrestricted) ;; 
     
      
let list_is_admissible upper_bound candidate = 
    if candidate = [] then true else 
    let fis =  Finite_int_set.of_usual_int_list candidate in 
   ((attained_upper_bound_opt fis upper_bound)=None);;

 let predecessor_for_dissociated_data_opt = Private.predecessor_for_dissociated_data_opt ;; 

  let translate d (UBC(w,b)) = UBC(w,Upper_bound_on_breadth.translate d b) ;;  
    
  
  end ;;  
  
  
  
  module Kay = struct 
  
    module Private = struct 

    let partial_obstruction_set (domain,ub_on_breadth,pivot,wmax) w=
         List.filter (fun l->
          (i_is_included_in l domain)
          &&
          (if w=wmax 
          then Upper_bound_on_breadth.allows ub_on_breadth (List.hd l)
          else true)
          )
          [[pivot-2*w;pivot-w;pivot];
          [pivot-w;pivot;pivot+w];
          [pivot;pivot+w;pivot+2*w]] ;;
    
    let full_obstruction_set (domain,ub_on_breadth,pivot,wmax) =
       List.flatten(Int_range.scale 
       (partial_obstruction_set (domain,ub_on_breadth,pivot,wmax)) 1 wmax);;

    

     end ;;

    let complements (Key(fis,UBC(W wmax,ub_on_breadth))) pivot =
       let domain = Finite_int_set.to_usual_int_list fis in 
       let obstructions = 
        Private.full_obstruction_set (domain,ub_on_breadth,pivot,wmax)  in 
       il_sort (Image.image (i_outsert pivot) obstructions)
       ;;

    let constructor (n,scrappers,w,b) =
        Key(FIS(n,scrappers),UBC(W w,Upper_bound_on_breadth.constructor b)) ;; 
  
    let decrement (Key(fis,ub_on_constraint)) =
      Key(fis,Upper_bound_on_constraint.decrement ub_on_constraint) ;;  

    let deconstructor (Key(FIS(n,scrappers),UBC(W w,ub_on_breadth))) =
           (n,scrappers,w,Upper_bound_on_breadth.deconstructor ub_on_breadth) ;;        
 
    let decompose_wrt_translation (Key(old_fis,ubc)) = 
       let (d,new_fis) = Finite_int_set.decompose_wrt_translation old_fis in 
       (d,Key(new_fis,Upper_bound_on_constraint.translate (-d) ubc)) ;;

    let is_admissible key proposal =    
       let (Key(_fis,upper_bound)) = key in 
       Upper_bound_on_constraint.list_is_admissible upper_bound proposal ;;  

    let largest_constraint_with_predecessor_opt key =
       let (Key(fis,ub_on_constraint)) = key in 
       match Upper_bound_on_constraint.attained_upper_bound_opt fis ub_on_constraint with 
       None -> None 
       |Some(UBC(W w1,ub_on_breadth1)) ->
         let (B b1) = Upper_bound_on_breadth.get ub_on_breadth1 in 
         let cstr1 = [b1;b1+w1;b1+2*w1] in 
         match Upper_bound_on_constraint.predecessor_for_dissociated_data_opt fis (W w1) (B b1) with 
          None -> Some(cstr1,None)         
         |Some(w2,B b2) ->Some(cstr1,Some(Key(fis,UBC(w2,Up_to(B b2))))) ;; 

    let max (Key(fis,_)) = Finite_int_set.max fis ;; 

    let remove_one_element (Key(old_fis,old_upper_bound)) k=
       let (UBC(W _w,ub_on_breadth)) = old_upper_bound 
       and new_fis = Finite_int_set.remove_one_element old_fis k in 
       let new_upper_bound = (
       (match Upper_bound_on_constraint.attained_upper_bound_opt new_fis old_upper_bound with 
        None -> UBC(W 1,Unrestricted)
        | Some upper_bound -> 
          if ub_on_breadth = Unrestricted
          then let (UBC(W smaller_w,_)) = upper_bound in 
               UBC(W smaller_w,Unrestricted)
          else upper_bound)
       ) in 
       Key(new_fis,new_upper_bound) ;; 

    let vertex_decomposition key =
        let (Key(fis,_upper_bound)) = key in 
        let (FIS(n,_)) = fis in 
        (n,remove_one_element key n) ;;
        
    let width (Key(_,UBC(w,_))) = w ;;
        
end ;;   
  
exception Bad_remainder_by_three of int ;; 

module Extra_tools = struct 

module Width_one = struct 
  
  let compute_without_upper_bound fis =
    let domain = Finite_int_set.to_usual_int_list fis in 
    let intervals = Arithmetic_list.decompose_into_connected_components domain in 
    let sol_components = Image.image (
      fun (a,b) ->
        List.filter(fun k->((k-a+1) mod 3)<>0)(Int_range.range a b)
    ) intervals 
    and forced_elements = Image.image (
      fun (a,b) ->
        match ((b-a+1) mod 3) with 
         0 -> []
        |1 -> List.filter(fun k->List.mem ((k-a+1) mod 3) [1])(Int_range.range a b)
        |2 -> List.filter(fun k->List.mem ((k-a+1) mod 3) [1;2])(Int_range.range a b)
        |r -> raise(Bad_remainder_by_three(r)) 
    ) intervals in 
    M([List.flatten sol_components],F[List.flatten forced_elements]);;

  let compute (Key(fis1,UBC(W w0,ub_on_breadth))) =
    if w0>1 then compute_without_upper_bound fis1 else  
    let domain1 = Finite_int_set.to_usual_int_list fis1 in 
    let (domain2,extra) = (
      match ub_on_breadth with 
      Unrestricted ->(domain1,[])
      |Up_to(B b)->List.partition (fun t->t<=(b+2)) domain1
    )  in 
    let fis2 = Finite_int_set.of_usual_int_list domain2 in 
    let (M(sols2,F ext2))  = compute_without_upper_bound fis2 in 
    M(Image.image (fun sol->sol@extra) sols2,F(Image.image(fun ext->ext@extra) ext2));; 

   let compute_opt key = Some(compute key) ;; 

end ;;   

let compute_opt key =  
    match List.assoc_opt (Kay.width key) [
        W 1, Width_one.compute_opt
    ] with 
    None -> None 
    | Some f -> f key ;;  

end ;;  

module Hashtbl_here = struct 

  let greedy = ((Hashtbl.create 50) : (key, mold) Hashtbl.t) ;; 

  let add key answer =
      Hashtbl.replace greedy key answer;;    

end ;;

module Crude = struct 


  module Peek_and_seek = struct 

    type peek_result = 
    P_Success of mold  
   |P_Failure
   |P_Unfinished_computation of key list ;;

    let seek_non_translated_obvious_access helper key = 
      match List.assoc_opt key helper with 
        Some (mold1) -> Some(mold1)
      | None ->
         (
            match  Hashtbl.find_opt Hashtbl_here.greedy key with 
            Some (mold2) -> Some(mold2)
          | None -> 
            let (Key(fis,_upper_bound)) = key in 
            let domain = Finite_int_set.to_usual_int_list fis in 
            if Kay.is_admissible key domain 
            then  Some(M([domain],F[domain]))
             else 
              (
                match Extra_tools.compute_opt key with 
                Some answer2 -> Some(answer2)
                |None -> None         
              ) 
           ) ;; 
  
     let seek_translated_obvious_access helper key =
        let (d,translated_key) = Kay.decompose_wrt_translation key in 
        match seek_non_translated_obvious_access helper translated_key with 
        None -> None 
        |Some (translated_mold) -> 
           Some(Mold.translate (-d) translated_mold);; 
  
      let peek_for_import_case helper key = 
        let smaller_key = Kay.decrement key  in 
        match seek_non_translated_obvious_access helper smaller_key with 
            None -> P_Unfinished_computation([smaller_key])  
           |Some(M(sols2,ext2)) ->
          let sols3 = List.filter_map (fun sol->
                      if Kay.is_admissible key sol 
                      then Some(sol) 
                      else None    
          ) sols2 in 
          if sols3 <> [] 
          then P_Success(M(sols3,ext2))  
          else P_Failure
      ;;
  
      let peek_for_cumulative_case helper key pivot= 
        let smaller_key = Kay.remove_one_element key pivot in 
        match seek_non_translated_obvious_access helper smaller_key with 
            None -> P_Unfinished_computation([smaller_key])  
           |Some(M(sols2,F ext2)) ->
          let extend_and_filter = List.filter_map (fun sol->
            let increased_sol = i_insert pivot sol in 
            if Kay.is_admissible key increased_sol 
            then Some(increased_sol) 
            else None    
          ) in  
          let new_ext = extend_and_filter ext2 in 
          if new_ext = []
          then P_Failure
          else
          let sols3 = extend_and_filter sols2 in 
          if sols3 <> [] 
          then P_Success(M(sols3,F(Image.image (i_insert pivot) ext2)))  
          else P_Failure
      ;;
  
      let peek_for_easy_case helper key =
         match seek_non_translated_obvious_access helper key with 
         Some(answer1) -> (P_Success(answer1),false)
         |None ->
          let peek_res1=peek_for_import_case helper key in 
          (match peek_res1 with 
        
             P_Success(_) -> (peek_res1,true)
            |P_Unfinished_computation(_) -> (peek_res1,false)
            |P_Failure -> 
               let n = Kay.max key in 
                (peek_for_cumulative_case helper key n,true)
               
           ) ;;
     
         let partition_candidates_in_fork_case helper candidates =
          let candidates2 = Image.image (
              fun triple -> 
                let (_smaller_key,_d,translated_smaller_key) = triple in 
                (triple,seek_non_translated_obvious_access helper translated_smaller_key)
          ) candidates in 
          let (bad_leaves,good_leaves) = 
              List.partition (fun (_,opt) -> opt = None ) candidates2 in 
          (Image.image (fun ((smaller_key,d,_translated_smaller_key),opt) -> 
                let translated_mold = Option.get opt in 
                (smaller_key,Mold.translate (-d) translated_mold)
            ) good_leaves,
           Image.image (fun ((_smaller_key,_d,translated_smaller_key),_)->
            translated_smaller_key
            ) bad_leaves) ;; 
        
        let peek_for_fork_case helper key (i,j,k)= 
          let cstr = [i;j;k] in 
          let candidates = Image.image (fun t->
            let smaller_key=Kay.remove_one_element key t in 
            let (d,translated_smaller_key) = Kay.decompose_wrt_translation smaller_key in 
            (smaller_key,d,translated_smaller_key)
          ) cstr in 
          let (candidates2,bad_ones) = partition_candidates_in_fork_case helper candidates in 
          if bad_ones <> []
          then P_Unfinished_computation(bad_ones)
          else   
          let lengths = Image.image (fun (_cand,M(sols,_ext))->
                  List.length(List.hd sols)) candidates2 in 
          let indexed_lengths = Int_range.index_everything lengths in 
          let (min1,min_indices) = Min.minimize_it_with_care snd indexed_lengths 
          and (max1,max_indices) = Max.maximize_it_with_care snd indexed_lengths in 
          if min1 = max1 
          then let (M(sols4,_)) = snd(List.hd(List.rev candidates2)) in 
                P_Success(M(sols4,F[[]]))
          else let (max_idx,_) = List.hd(List.rev max_indices) in 
                let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
                let ext5 = Image.image (fun (k,_)->List.nth cstr (k-1)) min_indices in 
                P_Success(M(sols5,F[ext5]));;    
  
  end ;;   
    
  module Compute = struct 

    exception Pusher_for_needed_subcomputations_exn_1 ;; 
    exception Pusher_for_needed_subcomputations_exn_2 ;; 
    exception Pusher_for_needed_subcomputations_exn_3 of key ;;  

    let pusher_for_needed_subcomputations (helper,to_be_treated) =
        match to_be_treated with 
         [] -> raise (Pusher_for_needed_subcomputations_exn_1) 
        |key :: others ->
          let (peek_res1,is_new) = Peek_and_seek.peek_for_easy_case helper key in 
          (
            match peek_res1 with 
          | Peek_and_seek.P_Unfinished_computation (new_to_be_treated) -> 
               (helper,new_to_be_treated@to_be_treated)
          | Peek_and_seek.P_Success (answer) -> 
              let new_helper =(
                 if is_new 
                 then (key,answer) :: helper 
                 else helper 
              ) in 
              (new_helper,others)
          | Peek_and_seek.P_Failure -> 
            (
              match Kay.largest_constraint_with_predecessor_opt key with 
               None -> raise (Pusher_for_needed_subcomputations_exn_2)
              |Some(cstr,_) ->
                 let nth = (fun k->List.nth cstr (k-1)) in 
                  match Peek_and_seek.peek_for_fork_case helper key (nth 1,nth 2,nth 3) with 
                  | Peek_and_seek.P_Unfinished_computation (new_to_be_treated) -> 
                       (helper,new_to_be_treated@to_be_treated)
                  | Peek_and_seek.P_Success (answer) -> 
                      ((key,answer) :: helper ,others)
                  | Peek_and_seek.P_Failure ->  raise (Pusher_for_needed_subcomputations_exn_3(key))
            )
            );;      

        
  
     let rec iterator_for_needed_subcomputations walker = 
        if snd walker = [] then List.rev(fst walker) else 
        let new_walker = pusher_for_needed_subcomputations walker in      
        iterator_for_needed_subcomputations new_walker ;;
  
     let needed_subcomputations items = 
      iterator_for_needed_subcomputations ([],items) ;;  
      
     let compute_recursively_and_remember key = 
        match Peek_and_seek.seek_translated_obvious_access [] key with 
        Some(mold) -> mold
        |None ->
           let subcomps =  needed_subcomputations [key] in 
           let _ = List.iter (fun (key,answer)->
            Hashtbl_here.add key answer
          ) subcomps in 
           List.assoc key subcomps  ;;                           
      
  end ;;  

  let compute key = Compute.compute_recursively_and_remember key ;; 

end ;;   



module Medium = struct 

  exception Key_is_too_easy of key ;; 
  exception Fork_or_select_exn of key ;; 
  exception Improved_crude_hook_finder_exn_1 of key ;; 
  exception Improved_crude_hook_finder_exn_2 of key ;; 

  module Private = struct   

 let measure key =
   let (M(sols,_)) = Crude.compute key in 
   List.length(List.hd sols) ;;  
 
  let rigorous_test_for_import_case old_key = 
    let (W w)=Kay.width(old_key) in 
    if w<2 then false else
    let simpler_key = Kay.decrement old_key in 
    (measure simpler_key)=(measure old_key) ;; 


let rigorous_quest_for_individual_cumulative_case old_key pivot = 
  let simpler_key = Kay.remove_one_element old_key pivot in 
  let (M(sols1,_ext1)) = Crude.compute simpler_key  
  and (M(sols2,_ext2)) = Crude.compute old_key in 
  if List.length(List.hd sols2)=List.length(List.hd sols1)+1 
  then Some(Mh_cumulative(pivot),Some(simpler_key))  
  else None ;;

let rigorous_quest_for_cumulative_case old_key =
    let (Key(fis,_)) = old_key in 
    let domain = List.rev(Finite_int_set.to_usual_int_list fis) in 
    List.find_map (rigorous_quest_for_individual_cumulative_case old_key) domain ;; 


let rigorous_quest_for_fork_or_select key = 
  match Kay.largest_constraint_with_predecessor_opt key with 
  None ->  raise(Fork_or_select_exn(key))
  |Some (cstr,opt) ->
     let c = (fun k->List.nth cstr (k-1))  in 
     (
       match opt with 
        None -> (Mh_fork(c 1,c 2,c 3),None)
       |Some preceding_key ->    
          if measure(preceding_key) = measure(key) 
          then  (Mh_select(c 1,c 2,c 3),Some(preceding_key))
          else  (Mh_fork(c 1,c 2,c 3),None)  
     ) ;; 

  let medium_hook_finder key = 
    match Kay.largest_constraint_with_predecessor_opt key with 
  None ->  None
  | Some (_,_) ->
    (match rigorous_quest_for_cumulative_case key with 
    Some(hook1,_opt1) -> Some hook1
    | None ->
       let (hook2,_opt2)= rigorous_quest_for_fork_or_select key in
       Some hook2);;

       type hook = 
          H_import  
          | H_cumulative of int 
          | H_fork of int * int *int  ;; 

  let improved_crude_hook_finder =Memoized.recursive( fun old_f key -> 
    match Kay.largest_constraint_with_predecessor_opt key with 
    None ->  raise(Improved_crude_hook_finder_exn_1(key))
    | Some (_,predecessor_opt) ->
        if rigorous_test_for_import_case key then H_import else
        (match medium_hook_finder key with 
        None -> raise(Improved_crude_hook_finder_exn_2(key))
        | Some hook -> 
        (match hook with 
         Mh_cumulative(pivot) -> H_cumulative(pivot) 
        |Mh_fork(i,j,k) -> H_fork(i,j,k) 
        |Mh_select (_,_,_) ->
             old_f(Option.get predecessor_opt) 
        ))     
    );;


    let pre_compute key = 
      let sol = Crude.compute key in 
      if Kay.largest_constraint_with_predecessor_opt key = None 
      then   (None,None,sol)
      else  match rigorous_quest_for_cumulative_case key with 
           Some(hook1,opt1) -> (Some hook1,opt1,sol)
           | None ->
              let (hook2,opt2)= rigorous_quest_for_fork_or_select key in
               (Some hook2,opt2,sol)
      ;; 
        
      
     let all_solutions =Memoized.recursive(fun old_f key -> 
       let (Key(fis,_ub_on_constraint)) = key in 
       let domain = Finite_int_set.to_usual_int_list fis 
       and is_ok = Kay.is_admissible key in 
       if is_ok domain 
       then [domain]
       else
       let compute_below = (fun t->
          old_f (Kay.remove_one_element key t)
       ) in 
       match improved_crude_hook_finder key with 
        H_cumulative(m)->
          List.filter_map (
             fun sol->
               let new_sol = i_insert m sol in 
               if is_ok new_sol then Some new_sol else None
          )(compute_below m) 
       |H_fork(i,j,k)->
         il_fold_merge(Image.image compute_below [i;j;k])
       |H_import ->  
         let smaller_key = Kay.decrement key in 
         List.filter is_ok (old_f smaller_key)
    );;        

  
   let helper_for_canonical_solution (sols,n) =
     let (with_n,without_n) = List.partition (List.mem n) sols in 
     if without_n=[]
     then (true,Image.image(i_outsert n) with_n) 
    else (false,without_n) ;;      
    
    let rec iterator_for_canonical_solution (to_be_treated,n,treated) =
      if n<1 then treated else  
      let (n_needed,to_be_treated2) = helper_for_canonical_solution (to_be_treated,n) in 
      let treated2 = (if n_needed then n::treated else treated) in
      iterator_for_canonical_solution (to_be_treated2,n-1,treated2) ;; 

    let canonical_solution key =
       let all_sols = all_solutions key 
       and n = Kay.max key in 
       iterator_for_canonical_solution (all_sols,n,[]) ;; 
    
    let compute key = 
       let (hook_opt,key_opt,M(_,fan)) = pre_compute key in 
       (hook_opt,key_opt,M([canonical_solution key],fan)) ;;   

end ;;   

let all_solutions = Private.all_solutions ;; 
let compute = Private.compute ;;

end ;;


module Partially_polished = struct 

  exception Missing_entry_exn of simplified_key ;; 
  exception Missing_solutions_exn of entry * (solution list) ;; 
  exception Insufficient_fan_exn of entry * fan ;;

  module Check = struct

  let rec assoc_opt skey (PP l)= match l with
     [] -> None 
     |(E(skey2,hm2)) :: others ->
        match sk_order skey skey2 with
         Total_ordering_result_t.Lower -> None 
        |Total_ordering_result_t.Equal -> Some hm2
        |Total_ordering_result_t.Greater -> assoc_opt skey (PP others) ;; 

  let assoc_or_raise key pp exen =
     let skey=Kay.deconstructor key in 
      match assoc_opt (Kay.deconstructor key) pp with 
      None -> raise exen
      |Some answer -> E(skey,answer) ;; 

  let compute_naively_without_translating_opt pp key = 
    match assoc_opt (Kay.deconstructor key) pp with 
    Some (_hook1,mold1) -> Some(mold1)
  | None -> 
    let (Key(fis,_upper_bound)) = key in 
    let domain = Finite_int_set.to_usual_int_list fis in 
    if Kay.is_admissible key domain 
    then  Some(M([domain],F[domain]))
    else Extra_tools.compute_opt key ;; 
 
  let compute_naively_without_translating pp key = 
   match compute_naively_without_translating_opt pp key with
    None -> raise(Missing_entry_exn(Kay.deconstructor key))
   |Some(answer) -> answer ;; 

  let compute_naively pp key =
      let (d,translated_key) = Kay.decompose_wrt_translation key in 
      let translated_mold = compute_naively_without_translating pp translated_key in 
      Mold.translate (-d) translated_mold;;          

  exception Noncumulability_check of partially_polished * key * int ;;  

  let check_that_noncumulability_was_predictable pp key pivot = 
     let beheaded_key = Kay.remove_one_element key pivot in 
     let mold = compute_naively pp beheaded_key 
     and complements = Kay.complements key pivot in 
     let (M(_sols2,F l_ext2)) = mold in 
     let is_not_treated = ( 
      fun case ->List.for_all (fun c->not(i_is_included_in c case)) complements  
     ) in   
     let untreated_cases = List.filter is_not_treated l_ext2 in 
     if untreated_cases=[]
     then ()
     else let old_entry = assoc_or_raise beheaded_key pp 
                (Noncumulability_check(pp,key,pivot)) in 
                raise(Insufficient_fan_exn(old_entry,F(complements)))
            ;;

  exception Check_fork_exn_1 ;; 
  exception Check_fork_exn_2 ;; 

  let check_fork pp (i,j,k) key (M(sols,fan)) = 
      let _ = check_that_noncumulability_was_predictable pp key (Kay.max key) in 
      let parts = Image.image (
        fun t->
            let subkey = Kay.remove_one_element key t in 
            (t,(subkey,compute_naively pp subkey)) 
      ) [k;j;i] in 
      let sols_with_pivots = Image.image (fun 
        sol->(sol,List.find (fun t->not(List.mem t sol)) [k;j;i])) sols in
      let unpredicted_sols = List.filter_map (
        fun (sol,t) -> 
           let (key2,M(sols2,_l_ext2)) = List.assoc t parts in 
           if not(List.mem sol sols2)
           then Some(sol,t,key2)
           else None 
      ) sols_with_pivots in 
      if unpredicted_sols<>[]
      then let (_sol3,t3,key3) = List.hd unpredicted_sols in 
           let slice_of_missing_sols = List.filter_map(
              fun (sol4,t4,_) ->
                   if t4=t3 then Some sol4 else None 
           ) unpredicted_sols 
          and old_entry = assoc_or_raise key3 pp Check_fork_exn_1 in 
          raise(Missing_solutions_exn(old_entry,slice_of_missing_sols)) 
      else 
      match List.find_opt(fun (_t,(_subkey3,M(_sols3,fan3)))->
          not(Fan.is_stronger_than fan3 fan)
      ) parts with 
      None -> ()
      |Some(_t,(subkey,_)) ->
        let old_entry = assoc_or_raise subkey pp Check_fork_exn_2 in   
        raise(Insufficient_fan_exn(old_entry,fan)) ;;
        
    exception Check_select_exn_1 ;;  
    exception Check_select_exn_2 ;;  

    let check_select pp (i,j,k) key (M(sols,fan)) = 
      let _ = check_that_noncumulability_was_predictable pp key  (Kay.max key) in 
      let (_,opt) = Option.get(Kay.largest_constraint_with_predecessor_opt key) in 
      let preceding_key = Option.get opt in 
      let (M(sols2,fan2)) = compute_naively pp preceding_key in 
      let unpredicted_sols = List.filter_map (
          fun sol -> 
             if not(List.mem sol sols2)
             then Some(sol)
            else None 
      ) sols in 
      if unpredicted_sols<>[]
      then let old_entry = assoc_or_raise preceding_key pp Check_select_exn_1 in 
          raise(Missing_solutions_exn(old_entry,unpredicted_sols)) 
         
      else 
      let weaker_fan = Fan.insert [i;j;k] fan in   
      if Fan.is_stronger_than fan2 weaker_fan
      then ()
      else
      let old_entry = assoc_or_raise preceding_key pp Check_select_exn_2 in   
      raise(Insufficient_fan_exn(old_entry,weaker_fan)) ;;

    exception Check_cumulative_exn_1 ;;  
    exception Check_cumulative_exn_2 ;;  

    let check_cumulative pp pivot key (M(sols,fan)) = 
        let smaller_key = Kay.remove_one_element key pivot in 
        let (M(sols2,fan2)) = compute_naively pp smaller_key in 
        let unpredicted_sols = List.filter_map (
            fun sol -> 
               let ssol = i_setminus sol [pivot] in  
               if not(List.mem ssol sols2)
               then Some(ssol)
              else None 
        ) sols in 
        if unpredicted_sols<>[]
        then let old_entry = assoc_or_raise smaller_key pp Check_cumulative_exn_1 in 
              raise(Missing_solutions_exn(old_entry,unpredicted_sols)) 
        else 
        let simplified_fan = Fan.remove_vertex pivot fan 
        and complements = Kay.complements key pivot in 
        let final_fan = Fan.insert_several complements simplified_fan in 
        if Fan.is_stronger_than fan2 final_fan
        then ()
        else let old_entry = assoc_or_raise smaller_key pp Check_cumulative_exn_2 in   
        raise(Insufficient_fan_exn(old_entry,final_fan)) ;; ;; 
  
    let check_item pp (E(skey,(hook,mold))) = 
        let key = Kay.constructor skey in 
        match hook with 
         Mh_cumulative(pivot) -> check_cumulative pp pivot key mold   
        |Mh_select(i,j,k) -> check_select pp (i,j,k) key mold  
        |Mh_fork(i,j,k) -> check_fork pp (i,j,k) key mold  ;;

    let check_all pp = 
        let (PP l) = pp in 
        List.iter (check_item pp) (List.rev l) ;;
   
    end ;;    

   let apply_small_polish (PP l) = function
     Add_entry(E(skey,(hook,mold))) ->
          PP (entry_insert (E(skey,(hook,mold))) l)
    |Replace_entry_by(E(skey, hook_and_mold)) ->
         PP(Image.image (fun pair->
             let (E(skey2,_)) = pair in 
             if skey2=skey
             then E(skey,hook_and_mold)
             else pair 
          ) l)    ;;

   let next_needed_small_polish_opt pp = 
    try (fun _->None)(Check.check_all pp) with 
    Missing_entry_exn(n,scr,w,b) ->
        let uple = (n,scr,w,b) in 
        let (hook_opt,_subkey_opt,mold) = Medium.compute (Kay.constructor uple) in 
        Some(Add_entry(E(uple,(Option.get hook_opt,mold)))) 
    |Missing_solutions_exn(E(skey,(hook,M(sols,fan))),new_sols)->
      Some(Replace_entry_by(E(skey,(hook,M(il_merge sols (il_sort new_sols),fan)))))    
    |Insufficient_fan_exn(E(skey,(hook,M(sols,fan))),new_fan)->    
      let large_fan = Fan.combine_two_conditions fan new_fan in 
      let all_sols = Medium.all_solutions (Kay.constructor skey) in 
      let smaller_fan = Fan.canonical_container all_sols large_fan in 
      Some(Replace_entry_by(E(skey,(hook,M(sols,smaller_fan)))))
    ;;   
    

end ;; 



