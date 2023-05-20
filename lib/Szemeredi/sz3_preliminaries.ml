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

type mold = Sz3_types.mold = M of (solution list) * extension_data ;;

type upper_bound_on_breadth = 
    Sz3_types.upper_bound_on_breadth = 
   Unrestricted |Up_to of breadth ;; 

type upper_bound_on_constraint = 
   Sz3_types.upper_bound_on_constraint = UBC of width * upper_bound_on_breadth ;; 

type key = 
   Sz3_types.key = Key of finite_int_set * upper_bound_on_constraint ;; 

type peek_result = Sz3_types.peek_result = 
    P_Success of mold  
   |P_Failure
   |P_Unfinished_computation of key list ;;


type patience = Sz3_types.patience = Impatient | Patient ;; 

type small_step = Sz3_types.small_step = St_cumulative | St_fork | St_import ;; 

let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;


module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  

module Mold = struct 

let translate d (M(sols,ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    M(Image.image tr sols,tr ext) ;; 

end ;;

let peek_result_to_mold_opt = function 
 P_Success (answer) -> Some answer
| P_Unfinished_computation (_)  
| P_Failure -> None ;;


module Finite_int_set = struct 

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  let remove_one_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       let new_z =  to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;     
  
  
  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_one_element (FIS(3,[])) 3 ;;
  
  *)
  

  let decompose_wrt_translation fis_domain = 
    let domain = to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,of_usual_int_list core_domain) ;; 


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
  
  let recoil d = function  
    Unrestricted -> Unrestricted 
   |Up_to(B b) -> Up_to(B(b-d)) ;; 
  
  
  end ;;  
  
module Upper_bound_on_constraint = struct 
  
    exception A_priori_bound_on_breadth_exn ;; 
  
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
     
  
  
      end ;;
  
  
  
  
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
  
  let recoil d (UBC(w,b)) = UBC(w,Upper_bound_on_breadth.recoil d b) ;;  
     
  
  
  end ;;  
  
  
  
  module Kay = struct 
  
    let constructor (n,scrappers,w,b) =
        Key(FIS(n,scrappers),UBC(W w,Upper_bound_on_breadth.constructor b)) ;; 
  
    let decrement (Key(fis,ub_on_constraint)) =
      Key(fis,Upper_bound_on_constraint.decrement ub_on_constraint) ;;  

    let width (Key(_,UBC(w,_))) = w ;;

    let decompose_wrt_translation (Key(old_fis,ubc)) = 
       let (d,new_fis) = Finite_int_set.decompose_wrt_translation old_fis in 
       (d,Key(new_fis,Upper_bound_on_constraint.recoil d ubc)) ;;
    
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
    
    let set_to_lower_width (Key(fis,_)) new_width=
        Key(fis,UBC(new_width,Unrestricted)) ;;  

    let vertex_decomposition key =
        let (Key(fis,_upper_bound)) = key in 
        let n = List.hd(List.rev(Finite_int_set.to_usual_int_list fis)) in 
        (n,remove_one_element key n) ;;   
    
    let usual_pair (n,scrappers,W w) =
        let fis = FIS(n,scrappers) in
        let upper_bound =UBC(W w,Unrestricted) in 
        Key(fis,upper_bound) ;;
    end ;;   
  
exception Get_below_exn of int * key ;;
exception Peek_for_fork_case_should_never_happen_1_exn of int ;;
exception Multiple_peek_exn of int ;; 
exception Simplified_multiple_peek_exn of int ;;
exception Pusher_for_needed_subcomputations_exn_1 of int ;; 
exception Pusher_for_needed_subcomputations_exn_2 of int ;;  
exception Bad_remainder_by_three of int ;; 
  
module Level1 = struct 
  
      let current_width = 1 ;; 
      
      let simpler_without_upper_bound fis =
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
            |_ -> raise(Bad_remainder_by_three(current_width)) 
        ) intervals in 
        M([List.flatten sol_components],List.flatten forced_elements);;
    
      let simpler (Key(fis1,UBC(W w0,ub_on_breadth))) =
        if w0>1 then simpler_without_upper_bound fis1 else  
        let domain1 = Finite_int_set.to_usual_int_list fis1 in 
        let (domain2,extra) = (
          match ub_on_breadth with 
          Unrestricted ->(domain1,[])
          |Up_to(B b)->List.partition (fun t->t<=(b+2)) domain1
        )  in 
        let fis2 = Finite_int_set.of_usual_int_list domain2 in 
        let (M(sols2,ext2))  = simpler_without_upper_bound fis2 in 
        M(Image.image (fun sol->sol@extra) sols2,ext2@extra);; 
    
      let compute_fast_opt hashtbl key =
        match Hashtbl.find_opt hashtbl key with 
        (Some answer) -> Some answer 
        | None -> Some(simpler key);;
    
       let compute_reasonably_fast_opt hashtbl fis_with_ub = 
        compute_fast_opt hashtbl fis_with_ub ;;     
    
       
    
end ;;  
    
(* Beginning of Level2 *)
module Level2 = struct 
  
    let current_width = 2 ;; 
    
    let get_below (hashtbl,patience) key = 
      match Level1.compute_reasonably_fast_opt hashtbl key with 
      Some answer -> (P_Success(answer),true)  
      |None -> 
        match patience with  
         Impatient -> raise(Get_below_exn(current_width-1,key))
        |Patient -> 
             (*
                this function get_below is only used when peeking for obvious accesses.
                If the answer is not immediate, we count it as a failure.   
             *)
            (P_Failure,false);;
  
    
    let peek_for_obvious_accesses (hashtbl,patience) helper key = 
      match List.assoc_opt key helper with 
        Some answer1 -> (P_Success(answer1),false) 
      | None ->
         (
            match  Hashtbl.find_opt hashtbl key with 
            Some answer2 -> (P_Success(answer2),false)
          | None -> 
            let (Key(fis,upper_bound)) = key in 
           (match Upper_bound_on_constraint.attained_upper_bound_opt  fis upper_bound with 
            None -> let domain = Finite_int_set.to_usual_int_list fis in 
                     (P_Success(M([domain],domain)),false)
           |Some (UBC(W w,_new_ub_on_breadth)) ->   
              if w<current_width 
              then let simplified_key = Kay.set_to_lower_width key (W w) in 
                   get_below (hashtbl,patience) simplified_key
              else (P_Failure,false)          
           )
         ) ;; 
    
  
    
    let peek_for_cumulative_case (hashtbl,patience) helper old_key = 
        let (n,new_key) = Kay.vertex_decomposition old_key in 
        let (peek_res,_) = peek_for_obvious_accesses (hashtbl,patience) helper new_key in 
          match peek_res with 
         P_Unfinished_computation(subcomp)  -> P_Unfinished_computation(subcomp@[new_key])
        |P_Failure -> P_Unfinished_computation([new_key]) 
        |P_Success(M(sols2,ext2)) ->
          let (Key(_,old_ub)) = old_key in 
          if not(Upper_bound_on_constraint.list_is_admissible old_ub (ext2@[n]))
          then P_Success(M(sols2,[]))
          else
          let sols3 = List.filter_map (fun sol->
                      if Upper_bound_on_constraint.list_is_admissible old_ub (sol@[n]) 
                      then Some(sol@[n]) 
                      else None    
          ) sols2 in 
          if sols3 <> [] 
          then P_Success(M(sols3,ext2@[n]))  
          else P_Failure
      ;;
  
  (*
     
    We use translations as little as possible. Most of the functions
    of this module are supposed to work on arguments where translation
    does not apply. The function below is an exception.
    
    *)
  
    
  
    let seek_obvious_accesses_using_translation 
      (hashtbl,patience) helper original_key = 
      let (d,translated_key) = Kay.decompose_wrt_translation original_key in 
      let (peek_res,_) = peek_for_obvious_accesses (hashtbl,patience) helper translated_key in 
          match peek_res with 
         P_Unfinished_computation(_)  -> (None,Some translated_key)
        |P_Failure -> (None,Some translated_key)
        |P_Success(translated_answer) ->
           let answer_to_original = Mold.translate d translated_answer in 
           (Some(answer_to_original),None);; 

  let partition_leaves_in_fork_case (hashtbl,patience) helper leaves =
    let leaves2 = Image.image (
        fun cand ->
          (cand,seek_obvious_accesses_using_translation (hashtbl,patience) helper cand)
    ) leaves in 
    let (good_leaves,bad_leaves) = 
        List.partition (fun (_,(_,opt_bad)) -> opt_bad = None ) leaves2 in 
    (Image.image (fun ( cand,( opt_good,_opt_bad)) -> (cand,Option.get opt_good)) good_leaves,
     Image.image (fun (_cand,(_opt_good, opt_bad)) -> Option.get opt_bad        ) bad_leaves) ;; 
  
  let peek_for_fork_case (hashtbl,patience) helper old_key = 
    let (Key(fis,upper_bound)) = old_key in 
    let opt1 = Upper_bound_on_constraint.attained_upper_bound_opt fis upper_bound in 
    if opt1=None  
    then raise(Peek_for_fork_case_should_never_happen_1_exn(current_width))
    else    
    let UBC(W w,ub_on_breadth) = Option.get opt1 in   
    let (B b)=Upper_bound_on_breadth.get ub_on_breadth in  
    let candidates = Image.image (
           fun i-> Kay.remove_one_element old_key i
    ) [b;b+w;b+2*w] in 
    let (candidates2,bad_ones) = partition_leaves_in_fork_case (hashtbl,patience) helper candidates in 
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
          P_Success(M(sols4,[]))
    else let (max_idx,_) = List.hd(List.rev max_indices) in 
          let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
          let ext5 = Image.image (fun (k,_)->List.nth [b;b+w;b+2*w] (k-1)) min_indices in 
          P_Success(M(sols5,ext5));;    
  
  
    let multiple_peek (hashtbl,patience) helper old_key = 
      let (peek_res1,to_be_remembered) = peek_for_obvious_accesses (hashtbl,patience) helper old_key in 
        match peek_res1 with 
        P_Success (_) -> (peek_res1,to_be_remembered)
      | P_Unfinished_computation (_) -> (peek_res1,false)  
      | P_Failure ->
      let peek_res2= peek_for_cumulative_case (hashtbl,patience) helper old_key in 
        match peek_res2 with 
        P_Success (_) -> (peek_res2,true)
      | P_Unfinished_computation (_) -> (peek_res2,false)  
      | P_Failure -> 
      let peek_res3= peek_for_fork_case (hashtbl,patience) helper old_key in 
        match peek_res3 with 
        P_Success (_) -> (peek_res3,true)
      | P_Unfinished_computation (_) -> (peek_res3,false)  
      | P_Failure -> raise(Multiple_peek_exn(current_width)) ;; 
          
    let simplified_multiple_peek (hashtbl,patience) helper key =   
      let (peek_res,_)= multiple_peek (hashtbl,patience) helper key in 
      match peek_res with 
            P_Failure -> raise (Simplified_multiple_peek_exn(current_width))
          | P_Unfinished_computation (new_to_be_treated) -> 
               (None,Some new_to_be_treated)
          | P_Success (answer) -> 
              (Some answer,None) ;; 
  
    let pusher_for_needed_subcomputations (hashtbl,patience) (helper,to_be_treated) =
        match to_be_treated with 
         [] -> raise (Pusher_for_needed_subcomputations_exn_1(current_width)) 
        |key :: others ->
          let (peek_res,to_be_remembered)= multiple_peek (hashtbl,patience) helper key in
          (
            match peek_res with 
            P_Failure -> raise (Pusher_for_needed_subcomputations_exn_2(current_width))
          | P_Unfinished_computation (new_to_be_treated) -> 
               (helper,new_to_be_treated@to_be_treated)
          | P_Success (answer) -> 
              let new_helper =(
                 if to_be_remembered 
                 then (key,answer) :: helper 
                 else helper 
              ) in 
              (new_helper,others)
          )  ;;     
  
     let rec iterator_for_needed_subcomputations (hashtbl,patience) walker = 
        if snd walker = [] then List.rev(fst walker) else 
        let new_walker = pusher_for_needed_subcomputations (hashtbl,patience) walker in      
        iterator_for_needed_subcomputations (hashtbl,patience) new_walker ;;
  
     let needed_subcomputations (hashtbl,patience) items = 
      iterator_for_needed_subcomputations (hashtbl,patience) ([],items) ;;  
      
     

     let compute_fast_opt (hashtbl,patience) key = 
      let (W w) = Kay.width(key) in 
      if w < current_width 
      then peek_result_to_mold_opt(fst(get_below (hashtbl,Patient) key))
      else peek_result_to_mold_opt(fst(multiple_peek (hashtbl,patience) [] key))
      
  
     let compute_reasonably_fast_opt (hashtbl,patience) key = 
       compute_fast_opt (hashtbl,patience) key ;;     
  
     
  
  end ;;  
  (* End of Level2 *)
  
  (* Beginning of Level3 *)
module Level3 = struct 
  
    let current_width = 3 ;; 
    
    let get_below (hshtbl,patience) key = 
      match Level2.compute_reasonably_fast_opt  (hshtbl,patience) key with 
      Some answer -> (P_Success(answer),true)  
      |None -> 
        match patience with  
         Impatient -> raise(Get_below_exn(current_width-1,key))
        |Patient -> 
             (*
                this function get_below is only used when peeking for obvious accesses.
                If the answer is not immediate, we count it as a failure.   
             *)
            (P_Failure,false);;
  
    
    let peek_for_obvious_accesses (hashtbl,patience) helper key = 
      match List.assoc_opt key helper with 
        Some answer1 -> (P_Success(answer1),false) 
      | None ->
         (
            match  Hashtbl.find_opt hashtbl key with 
            Some answer2 -> (P_Success(answer2),false)
          | None -> 
            let (Key(fis,upper_bound)) = key in 
           (match Upper_bound_on_constraint.attained_upper_bound_opt  fis upper_bound with 
            None -> let domain = Finite_int_set.to_usual_int_list fis in 
                     (P_Success(M([domain],domain)),false)
           |Some (UBC(W w,_new_ub_on_breadth)) ->   
              if w<current_width 
              then let simplified_key = Kay.set_to_lower_width key (W w) in 
                   get_below (hashtbl,patience) simplified_key
              else (P_Failure,false)          
           )
         ) ;; 
    
  
    
    let peek_for_cumulative_case (hashtbl,patience) helper old_key = 
        let (n,new_key) = Kay.vertex_decomposition old_key in 
        let (peek_res,_) = peek_for_obvious_accesses (hashtbl,patience) helper new_key in 
          match peek_res with 
         P_Unfinished_computation(subcomp)  -> P_Unfinished_computation(subcomp@[new_key])
        |P_Failure -> P_Unfinished_computation([new_key]) 
        |P_Success(M(sols2,ext2)) ->
          let (Key(_,old_ub)) = old_key in 
          if not(Upper_bound_on_constraint.list_is_admissible old_ub (ext2@[n]))
          then P_Success(M(sols2,[]))
          else
          let sols3 = List.filter_map (fun sol->
                      if Upper_bound_on_constraint.list_is_admissible old_ub (sol@[n]) 
                      then Some(sol@[n]) 
                      else None    
          ) sols2 in 
          if sols3 <> [] 
          then P_Success(M(sols3,ext2@[n]))  
          else P_Failure
      ;;
  
  (*
     
    We use translations as little as possible. Most of the functions
    of this module are supposed to work on arguments where translation
    does not apply. The function below is an exception.
    
    *)
  
    
  
    let seek_obvious_accesses_using_translation 
      (hashtbl,patience) helper original_key = 
      let (d,translated_key) = Kay.decompose_wrt_translation original_key in 
      let (peek_res,_) = peek_for_obvious_accesses (hashtbl,patience) helper translated_key in 
          match peek_res with 
         P_Unfinished_computation(_)  -> (None,Some translated_key)
        |P_Failure -> (None,Some translated_key)
        |P_Success(translated_answer) ->
           let answer_to_original = Mold.translate d translated_answer in 
           (Some(answer_to_original),None);; 

  let partition_leaves_in_fork_case (hashtbl,patience) helper leaves =
    let leaves2 = Image.image (
        fun cand ->
          (cand,seek_obvious_accesses_using_translation (hashtbl,patience) helper cand)
    ) leaves in 
    let (good_leaves,bad_leaves) = 
        List.partition (fun (_,(_,opt_bad)) -> opt_bad = None ) leaves2 in 
    (Image.image (fun ( cand,( opt_good,_opt_bad)) -> (cand,Option.get opt_good)) good_leaves,
     Image.image (fun (_cand,(_opt_good, opt_bad)) -> Option.get opt_bad        ) bad_leaves) ;; 
  
  let peek_for_fork_case (hashtbl,patience) helper old_key = 
    let (Key(fis,upper_bound)) = old_key in 
    let opt1 = Upper_bound_on_constraint.attained_upper_bound_opt fis upper_bound in 
    if opt1=None  
    then raise(Peek_for_fork_case_should_never_happen_1_exn(current_width))
    else    
    let UBC(W w,ub_on_breadth) = Option.get opt1 in   
    let (B b)=Upper_bound_on_breadth.get ub_on_breadth in  
    let candidates = Image.image (
           fun i-> Kay.remove_one_element old_key i
    ) [b;b+w;b+2*w] in 
    let (candidates2,bad_ones) = partition_leaves_in_fork_case (hashtbl,patience) helper candidates in 
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
          P_Success(M(sols4,[]))
    else let (max_idx,_) = List.hd(List.rev max_indices) in 
          let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
          let ext5 = Image.image (fun (k,_)->List.nth [b;b+w;b+2*w] (k-1)) min_indices in 
          P_Success(M(sols5,ext5));;    
  
  
    let multiple_peek (hashtbl,patience) helper old_key = 
      let (peek_res1,to_be_remembered) = peek_for_obvious_accesses (hashtbl,patience) helper old_key in 
        match peek_res1 with 
        P_Success (_) -> (peek_res1,to_be_remembered)
      | P_Unfinished_computation (_) -> (peek_res1,false)  
      | P_Failure ->
      let peek_res2= peek_for_cumulative_case (hashtbl,patience) helper old_key in 
        match peek_res2 with 
        P_Success (_) -> (peek_res2,true)
      | P_Unfinished_computation (_) -> (peek_res2,false)  
      | P_Failure -> 
      let peek_res3= peek_for_fork_case (hashtbl,patience) helper old_key in 
        match peek_res3 with 
        P_Success (_) -> (peek_res3,true)
      | P_Unfinished_computation (_) -> (peek_res3,false)  
      | P_Failure -> raise(Multiple_peek_exn(current_width)) ;; 
          
    let simplified_multiple_peek (hashtbl,patience) helper key =   
      let (peek_res,_)= multiple_peek (hashtbl,patience) helper key in 
      match peek_res with 
            P_Failure -> raise (Simplified_multiple_peek_exn(current_width))
          | P_Unfinished_computation (new_to_be_treated) -> 
               (None,Some new_to_be_treated)
          | P_Success (answer) -> 
              (Some answer,None) ;; 
  
    let pusher_for_needed_subcomputations (hashtbl,patience) (helper,to_be_treated) =
        match to_be_treated with 
         [] -> raise (Pusher_for_needed_subcomputations_exn_1(current_width)) 
        |key :: others ->
          let (peek_res,to_be_remembered)= multiple_peek (hashtbl,patience) helper key in
          (
            match peek_res with 
            P_Failure -> raise (Pusher_for_needed_subcomputations_exn_2(current_width))
          | P_Unfinished_computation (new_to_be_treated) -> 
               (helper,new_to_be_treated@to_be_treated)
          | P_Success (answer) -> 
              let new_helper =(
                 if to_be_remembered 
                 then (key,answer) :: helper 
                 else helper 
              ) in 
              (new_helper,others)
          )  ;;     
  
     let rec iterator_for_needed_subcomputations (hashtbl,patience) walker = 
        if snd walker = [] then List.rev(fst walker) else 
        let new_walker = pusher_for_needed_subcomputations (hashtbl,patience) walker in      
        iterator_for_needed_subcomputations (hashtbl,patience) new_walker ;;
  
     let needed_subcomputations (hashtbl,patience) items = 
      iterator_for_needed_subcomputations (hashtbl,patience) ([],items) ;;  
      
     

     let compute_fast_opt (hashtbl,patience) key = 
      let (W w) = Kay.width(key) in 
      if w < current_width 
      then peek_result_to_mold_opt(fst(get_below (hashtbl,Patient) key))
      else peek_result_to_mold_opt(fst(multiple_peek (hashtbl,patience) [] key))
      
  
     let compute_reasonably_fast_opt (hashtbl,patience) key = 
       compute_fast_opt (hashtbl,patience) key ;;     
  
     
  
  end ;;  
  (* End of Level3 *)
  
  exception Bad_index_in_selection of int ;; 
  
  module Selector = struct 
   
  
  let impatient_hashtbl = ((Hashtbl.create 50) : (key, mold) Hashtbl.t) ;; 
  let patient_hashtbl = ((Hashtbl.create 50) : (key, mold) Hashtbl.t) ;; 
  
  let get_hashtbl = function 
     Impatient -> impatient_hashtbl 
     |Patient -> patient_hashtbl ;; 
  
  let needed_subcomputations key_list=
    let widths = Image.image Kay.width key_list in 
    let (_,max_width) = Max.maximize_it (fun (W w)->w) widths in 
     match max_width with 
      1 -> []
     |2 -> Level2.needed_subcomputations (patient_hashtbl,Patient) key_list
     |3 -> Level3.needed_subcomputations (patient_hashtbl,Patient) key_list 
     |_ -> raise(Bad_index_in_selection max_width) ;; 
    
  let compute_reasonably_fast_opt key = 
    let (W max_width) = Kay.width key in 
    match max_width with 
    1 -> Level1.compute_reasonably_fast_opt impatient_hashtbl key
   |2 -> Level2.compute_reasonably_fast_opt (impatient_hashtbl,Impatient) key
   |3 -> Level3.compute_reasonably_fast_opt (impatient_hashtbl,Impatient) key 
   |_ -> raise(Bad_index_in_selection max_width) ;;    
  
   let half_impatient_peek_for_fork_case key = 
    let (W max_width) = Kay.width key in 
    match max_width with 
    1 -> P_Success(Option.get(Level1.compute_reasonably_fast_opt impatient_hashtbl key))
   |2 -> Level2.peek_for_fork_case (impatient_hashtbl,Patient) [] key
   |3 -> Level3.peek_for_fork_case (impatient_hashtbl,Patient) [] key 
   |_ -> raise(Bad_index_in_selection max_width) ;;  

  let half_impatient_peek_for_cumulative_case key = 
    let (W max_width) = Kay.width key in 
    match max_width with 
    1 -> P_Success(Option.get(Level1.compute_reasonably_fast_opt impatient_hashtbl key))
   |2 -> Level2.peek_for_cumulative_case (impatient_hashtbl,Patient) [] key
   |3 -> Level3.peek_for_cumulative_case (impatient_hashtbl,Patient) [] key 
   |_ -> raise(Bad_index_in_selection max_width) ;;  


  end ;;

  module Main = struct 
  
  let compute_recursively_and_remember key = 
      match Hashtbl.find_opt Selector.patient_hashtbl key with 
      Some(old_answer) -> old_answer
      | None ->
      let subcomps = Selector.needed_subcomputations [key] in 
      if subcomps = []
      then Option.get(Selector.compute_reasonably_fast_opt key)
      else    
      let _ = List.iter (fun (key,answer)->
        Hashtbl.replace Selector.patient_hashtbl key answer
      ) subcomps in 
      List.assoc key subcomps ;;      

  let rigorous_quest_for_cumulative_case old_key = 
    let (_n,simpler_key) = Kay.vertex_decomposition old_key in 
    let res1 = compute_recursively_and_remember simpler_key 
    and res2 = compute_recursively_and_remember old_key in 
    let M(sols1,_ext1) = res1 
    and M(sols2,_ext2) = res2 in 
    if List.length(List.hd sols2)=List.length(List.hd sols1)+1 
    then Some(res1,res2,[simpler_key])  
    else None ;;

  let rigorous_quest_for_fork_case old_key = 
      let (Key(fis,upper_bound)) = old_key in 
      let opt1 = Upper_bound_on_constraint.attained_upper_bound_opt fis upper_bound in 
      if opt1=None  
      then None
      else    
      let UBC(W w,ub_on_breadth) = Option.get opt1 in   
      let (B b)=Upper_bound_on_breadth.get ub_on_breadth in  
      let bare_candidates = Image.image 
      (Kay.remove_one_element old_key) [b;b+w;b+2*w] in 
      let candidates = Image.image (
             fun cand-> (cand,compute_recursively_and_remember cand)
      ) bare_candidates 
      and translated_candidates = Image.image (
        fun cand-> snd(Kay.decompose_wrt_translation cand)
      ) bare_candidates in 
      let sizes = Image.image (fun (_,M(sol,_ext))->List.length(List.hd sol)) candidates in 
      let first_size = List.hd sizes in 
      if List.for_all (fun size->size=first_size) sizes 
      then Some([b;b+w;b+2*w],candidates,translated_candidates)
      else None;;   
  
  exception Possibly_needed_exn of key ;; 

  let possibly_needed key = 
    match rigorous_quest_for_cumulative_case key with 
    Some(_,_,candidates)-> candidates
    | None ->
      (
       match rigorous_quest_for_fork_case key with 
       Some(_,_,candidates)-> candidates
       | None -> raise(Possibly_needed_exn(key))
      );;

  

  let directly_needed key = 
    List.filter (fun cand->
      (Selector.compute_reasonably_fast_opt cand)=None
    )(possibly_needed key) ;; 

    

  end ;;   
  

module Small_step = struct 
  
  exception Compute_easy_cumulative_exn of key ;;
  exception Compute_easy_fork_exn of key ;;
  exception Import_exn1 of key ;;    
  exception Import_exn2 of key ;;  

  module Private = struct

  let compute_easy_cumulative key =
      match Selector.half_impatient_peek_for_cumulative_case key with 
     P_Success(answer) -> answer 
   | P_Unfinished_computation(_)
   | P_Failure -> raise(Compute_easy_cumulative_exn(key)) ;; 
    
  let add_easy_cumulative key =
      let answer = compute_easy_cumulative key in 
  (
    Hashtbl.replace Selector.impatient_hashtbl key answer ;
    Hashtbl.replace Selector.patient_hashtbl key answer 
  ) ;;
 
  let compute_easy_fork key =
        match Selector.half_impatient_peek_for_fork_case key with 
         P_Success(answer) -> answer 
        | P_Unfinished_computation(_)
        | P_Failure -> raise(Compute_easy_fork_exn(key)) ;; 

  let add_easy_fork key =
      let answer = compute_easy_fork key in 
      (
            Hashtbl.replace Selector.impatient_hashtbl key answer ;
            Hashtbl.replace Selector.patient_hashtbl key answer 
      ) ;;

  let import key = 
     let new_key = Kay.decrement key 
     and (Key(_,ub_on_constraints))=key in 
     match Selector.compute_reasonably_fast_opt new_key with 
     None -> raise(Import_exn1(key))
     |Some(M(sols,ext)) ->    
     let sols2 = List.filter (Upper_bound_on_constraint.list_is_admissible ub_on_constraints) sols in 
      if sols2 = []
      then raise(Import_exn2(key))
      else 
      let answer = M(sols2,ext) in   
      let _=  (
            Hashtbl.replace Selector.impatient_hashtbl key answer ;
            Hashtbl.replace Selector.patient_hashtbl key answer 
          ) in 
      ();;
   
    end ;;

   let apply = function 
       St_cumulative -> Private.add_easy_cumulative 
      |St_fork -> Private.add_easy_fork
      |St_import -> Private.import ;;      

end ;;   


module Fill = struct 
  
  let bound = 40 ;; 
  let apply ((n,scr,w,b),small_step)=
    Small_step.apply small_step (Kay.constructor(n,scr,w,b)) ;;


   let for_level2 = [
        ((7,[4],2,0),St_import);
   ] ;; 
   
   let for_level3 = [
        ((5,[],3,0),St_import);
        ((6,[],3,0),St_import);
        ((7,[],3,0),St_fork);
   ] ;; 

   let all () =
    let _ = Int_range.scale (fun k->apply ((k,[],2,0),St_import)) 1 bound in 
    (
    List.iter apply for_level2;
    List.iter apply for_level3;
    )
    ;;    

    let reset () = 
      (
         Hashtbl.clear Selector.patient_hashtbl ;
         Hashtbl.clear Selector.impatient_hashtbl ;
         all ();
      ) ;;

end ;;