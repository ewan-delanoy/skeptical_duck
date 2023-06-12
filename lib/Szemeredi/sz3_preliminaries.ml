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

type mold = Sz3_types.mold = M of (solution list) * (extension_data list) ;;

type upper_bound_on_breadth = 
    Sz3_types.upper_bound_on_breadth = 
   Unrestricted |Up_to of breadth ;; 

type upper_bound_on_constraint = 
   Sz3_types.upper_bound_on_constraint = UBC of width * upper_bound_on_breadth ;; 

type key = 
   Sz3_types.key = Key of finite_int_set * upper_bound_on_constraint ;; 

type crude_hook = Sz3_types.crude_hook =  Ch_import | Ch_cumulative of int | Ch_fork of int * int *int  ;; 

type medium_hook = Sz3_types.medium_hook = Mh_cumulative of int | Mh_select of int * int *int | Mh_fork of int * int *int  ;; 

type simplified_key = int * (int list) * int * int ;;

type partially_polished = Sz3_types.partially_polished = PP of (simplified_key * (medium_hook * mold)) list ;; 

let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersect = Ordered.intersect i_order ;;
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

let pp_element_order = Total_ordering.product sk_order hm_order ;; 

let pp_element_merge = Ordered.merge pp_element_order ;;
let pp_element_sort = Ordered.sort pp_element_order ;;


module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  

module Hook = struct 

  let translate d = function 
      Ch_import -> Ch_import 
    | Ch_cumulative (m) -> Ch_cumulative (m+d) 
    | Ch_fork (i,j,k) -> Ch_fork (i+d,j+d,k+d) ;;
    
  let translate_opt d = function 
     None -> None 
     | Some hook -> Some (translate d hook) ;;   
  
end ;;

module Mold = struct 

let translate d (M(sols,ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    M(Image.image tr sols,Image.image tr ext) ;; 

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
  
    let constructor (n,scrappers,w,b) =
        Key(FIS(n,scrappers),UBC(W w,Upper_bound_on_breadth.constructor b)) ;; 
  
    let decrement (Key(fis,ub_on_constraint)) =
      Key(fis,Upper_bound_on_constraint.decrement ub_on_constraint) ;;  

    let deconstructor (Key(FIS(n,scrappers),UBC(W w,ub_on_breadth))) =
           (n,scrappers,w,Upper_bound_on_breadth.deconstructor ub_on_breadth) ;;        
 
    let decompose_wrt_translation (Key(old_fis,ubc)) = 
       let (d,new_fis) = Finite_int_set.decompose_wrt_translation old_fis in 
       (d,Key(new_fis,Upper_bound_on_constraint.translate (-d) ubc)) ;;


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
    M([List.flatten sol_components],[List.flatten forced_elements]);;

  let compute (Key(fis1,UBC(W w0,ub_on_breadth))) =
    if w0>1 then compute_without_upper_bound fis1 else  
    let domain1 = Finite_int_set.to_usual_int_list fis1 in 
    let (domain2,extra) = (
      match ub_on_breadth with 
      Unrestricted ->(domain1,[])
      |Up_to(B b)->List.partition (fun t->t<=(b+2)) domain1
    )  in 
    let fis2 = Finite_int_set.of_usual_int_list domain2 in 
    let (M(sols2,ext2))  = compute_without_upper_bound fis2 in 
    M(Image.image (fun sol->sol@extra) sols2,Image.image(fun ext->ext@extra) ext2);; 

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

  let greedy = ((Hashtbl.create 50) : (key, crude_hook * mold) Hashtbl.t) ;; 
  let cautious = ((Hashtbl.create 50) : (key, crude_hook * mold) Hashtbl.t) ;;   
  
  let add_to_all key answer =
      (
        Hashtbl.replace cautious key answer;
        Hashtbl.replace greedy key answer;
      ) ;;

  let add_to_greedy_only key answer =
      Hashtbl.replace greedy key answer;;    

end ;;

module Crude = struct 


  module Peek_and_seek = struct 

    type peek_result = 
    P_Success of crude_hook * mold  
   |P_Failure
   |P_Unfinished_computation of key list ;;

    let seek_non_translated_obvious_access helper key = 
      match List.assoc_opt key helper with 
        Some (hook1,mold1) -> Some(Some hook1,mold1)
      | None ->
         (
            match  Hashtbl.find_opt Hashtbl_here.greedy key with 
            Some (hook2,mold2) -> Some(Some hook2,mold2)
          | None -> 
            let (Key(fis,upper_bound)) = key in 
            let domain = Finite_int_set.to_usual_int_list fis in 
            if Upper_bound_on_constraint.list_is_admissible upper_bound domain 
            then  Some(None,M([domain],[domain]))
             else 
              (
                match Extra_tools.compute_opt key with 
                Some answer2 -> Some(None,answer2)
                |None -> None         
              ) 
           ) ;; 
  
     let seek_translated_obvious_access helper key =
        let (d,translated_key) = Kay.decompose_wrt_translation key in 
        match seek_non_translated_obvious_access helper translated_key with 
        None -> None 
        |Some (hook_opt,translated_mold) -> 
           Some(Hook.translate_opt (-d) hook_opt,
             Mold.translate (-d) translated_mold);; 
  
      let peek_for_import_case helper key = 
        let smaller_key = Kay.decrement key  in 
        match seek_non_translated_obvious_access helper smaller_key with 
            None -> P_Unfinished_computation([smaller_key])  
           |Some(_,M(sols2,ext2)) ->
          let (Key(_,upper_bound)) = key in   
          let sols3 = List.filter_map (fun sol->
                      if Upper_bound_on_constraint.list_is_admissible upper_bound sol 
                      then Some(sol) 
                      else None    
          ) sols2 in 
          if sols3 <> [] 
          then P_Success(Ch_import,M(sols3,ext2))  
          else P_Failure
      ;;
  
      let peek_for_cumulative_case helper key pivot= 
        let smaller_key = Kay.remove_one_element key pivot in 
        match seek_non_translated_obvious_access helper smaller_key with 
            None -> P_Unfinished_computation([smaller_key])  
           |Some(_,M(sols2,ext2)) ->
          let (Key(_,old_ub)) = key in 
          let extend_and_filter = List.filter_map (fun sol->
            let increased_sol = i_insert pivot sol in 
            if Upper_bound_on_constraint.list_is_admissible old_ub increased_sol 
            then Some(increased_sol) 
            else None    
          ) in  
          let new_ext = extend_and_filter ext2 in 
          if new_ext = []
          then P_Failure
          else
          let sols3 = extend_and_filter sols2 in 
          if sols3 <> [] 
          then P_Success(Ch_cumulative(pivot),M(sols3,Image.image (i_insert pivot) ext2))  
          else P_Failure
      ;;
  
      let peek_for_easy_case helper key =
         match seek_non_translated_obvious_access helper key with 
         Some(_,answer1) -> (P_Success(Ch_import,answer1),None)
         |None ->
          let peek_res1=peek_for_import_case helper key in 
          (match peek_res1 with 
        
             P_Success(_) -> (peek_res1,Some Ch_import)
            |P_Unfinished_computation(_) -> (peek_res1,None)
            |P_Failure -> 
               let n = Kay.max key in 
               let peek_res2=peek_for_cumulative_case helper key n in 
               (match peek_res2 with 
               P_Success(_) -> (peek_res2,Some (Ch_cumulative(n)))
              |P_Unfinished_computation(_) 
              |P_Failure -> (peek_res2,None)
               )
           ) ;;
     
         let partition_candidates_in_fork_case helper candidates =
          let candidates2 = Image.image (
              fun triple -> 
                let (_smaller_key,_d,translated_smaller_key) = triple in 
                (triple,seek_non_translated_obvious_access helper translated_smaller_key)
          ) candidates in 
          let (bad_leaves,good_leaves) = 
              List.partition (fun (_,opt) -> opt = None ) candidates2 in 
          (Image.image (fun ( 
            (smaller_key,d,_translated_smaller_key),opt) -> 
                let (_,translated_mold) = Option.get opt in 
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
                P_Success(Ch_fork(i,j,k),M(sols4,[[]]))
          else let (max_idx,_) = List.hd(List.rev max_indices) in 
                let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
                let ext5 = Image.image (fun (k,_)->List.nth cstr (k-1)) min_indices in 
                P_Success(Ch_fork(i,j,k),M(sols5,[ext5]));;    
         
          
        let peek_for_hook helper key = function 
           Ch_import -> peek_for_import_case helper key 
           |Ch_cumulative(pivot) ->  peek_for_cumulative_case helper key pivot 
           |Ch_fork(i,j,k) ->  peek_for_fork_case helper key (i,j,k) ;; 
  
  end ;;   
    
  module Compute = struct 

    exception Pusher_for_needed_subcomputations_exn_1 ;; 
    exception Pusher_for_needed_subcomputations_exn_2 ;; 
    exception Pusher_for_needed_subcomputations_exn_3 of crude_hook * key ;; 

    let suboptimal_hook_finder key =
       match Kay.largest_constraint_with_predecessor_opt key with 
       None -> None 
       |Some(cstr,_) ->
         let nth = (fun k->List.nth cstr (k-1)) in 
         Some(Ch_fork(nth 1,nth 2,nth 3)) ;; 

    let pusher_for_needed_subcomputations (helper,to_be_treated) =
        match to_be_treated with 
         [] -> raise (Pusher_for_needed_subcomputations_exn_1) 
        |key :: others ->
          let (peek_res1,hook_opt) = Peek_and_seek.peek_for_easy_case helper key in 
          (
            match peek_res1 with 
          | Peek_and_seek.P_Unfinished_computation (new_to_be_treated) -> 
               (helper,new_to_be_treated@to_be_treated)
          | Peek_and_seek.P_Success (_,answer) -> 
              let new_helper =(
                 match hook_opt with 
                  Some(hook) -> (key,(hook,answer)) :: helper 
                 |None -> helper 
              ) in 
              (new_helper,others)
          | Peek_and_seek.P_Failure -> 
            let hook_opt2 = suboptimal_hook_finder key in 
            (
              match hook_opt2 with 
                 None -> raise (Pusher_for_needed_subcomputations_exn_2)
                |Some hook2 -> 
                  let peek_res2 = Peek_and_seek.peek_for_hook helper key hook2 in 
                  match peek_res2 with 
                  | Peek_and_seek.P_Unfinished_computation (new_to_be_treated) -> 
                       (helper,new_to_be_treated@to_be_treated)
                  | Peek_and_seek.P_Success (_,answer) -> 
                      ((key,(hook2,answer)) :: helper ,others)
                  | Peek_and_seek.P_Failure ->  raise (Pusher_for_needed_subcomputations_exn_3(hook2,key))
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
        Some(hook_opt,mold) -> (hook_opt,mold)
        |None ->
           let subcomps =  needed_subcomputations [key] in 
           let _ = List.iter (fun (key,answer)->
            Hashtbl_here.add_to_greedy_only key answer
          ) subcomps in 
           let (hook,mold) = List.assoc key subcomps in
           (Some hook,mold) ;;                           
      
  end ;;  

  let compute = Compute.compute_recursively_and_remember ;; 

end ;;   



module Medium = struct 

  exception Key_is_too_easy of key ;; 
  exception Fork_or_select_exn of key ;; 
  exception Improved_crude_hook_finder_exn_1 of key ;; 
  exception Improved_crude_hook_finder_exn_2 of key ;; 

  module Private = struct   

 let measure key =
   let (_,M(sols,_)) = Crude.compute key in 
   List.length(List.hd sols) ;;  
 
  let rigorous_test_for_import_case old_key = 
    let (W w)=Kay.width(old_key) in 
    if w<2 then false else
    let simpler_key = Kay.decrement old_key in 
    (measure simpler_key)=(measure old_key) ;; 


let rigorous_quest_for_individual_cumulative_case old_key pivot = 
  let simpler_key = Kay.remove_one_element old_key pivot in 
  let res1 = Crude.compute simpler_key 
  and res2 = Crude.compute old_key in 
  let (_,M(sols1,_ext1)) = res1 
  and (_,M(sols2,_ext2)) = res2 in 
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

  let improved_crude_hook_finder =Memoized.recursive( fun old_f key -> 
    match Kay.largest_constraint_with_predecessor_opt key with 
    None ->  raise(Improved_crude_hook_finder_exn_1(key))
    | Some (_,predecessor_opt) ->
        if rigorous_test_for_import_case key then Ch_import else
        (match medium_hook_finder key with 
        None -> raise(Improved_crude_hook_finder_exn_2(key))
        | Some hook -> 
        (match hook with 
         Mh_cumulative(pivot) -> Ch_cumulative(pivot) 
        |Mh_fork(i,j,k) -> Ch_fork(i,j,k) 
        |Mh_select (_,_,_) ->
             old_f(Option.get predecessor_opt) 
        ))     
    );;


    let compute key = 
      let (opt,sol) = Crude.compute key in 
      match opt with 
      None -> (None,None,sol)
      | Some _ -> 
        (
          match rigorous_quest_for_cumulative_case key with 
           Some(hook1,opt1) -> (Some hook1,opt1,sol)
           | None ->
              let (hook2,opt2)= rigorous_quest_for_fork_or_select key in
               (Some hook2,opt2,sol)
            
        ) ;; 
        
      
     let all_solutions =Memoized.recursive(fun old_f key -> 
       let (Key(fis,ub_on_constraint)) = key in 
       let domain = Finite_int_set.to_usual_int_list fis 
       and is_ok = Upper_bound_on_constraint.list_is_admissible ub_on_constraint in 
       if is_ok domain 
       then [domain]
       else
       let compute_below = (fun t->
          old_f (Kay.remove_one_element key t)
       ) in 
       match improved_crude_hook_finder key with 
       Ch_cumulative(m)->
          List.filter_map (
             fun sol->
               let new_sol = i_insert m sol in 
               if is_ok new_sol then Some new_sol else None
          )(compute_below m) 
       |Ch_fork(i,j,k)->
         il_fold_merge(Image.image compute_below [i;j;k])
       |Ch_import ->  
         let smaller_key = Kay.decrement key in 
         List.filter is_ok (old_f smaller_key)
    );;        

end ;;   

let all_solutions = Private.all_solutions ;; 
let compute = Private.compute ;;

end ;;


module Partially_polished = struct 

  exception Compute_naively_exn of simplified_key ;; 
  exception Unregistered_solutions of ( (solution * key) list) * key * medium_hook ;;
  exception Untreated_cases of ( (extension_data * key) list) * key * medium_hook ;;

  module Check = struct

  let compute_naively_without_translating_opt (PP l) key = 
    match List.assoc_opt (Kay.deconstructor key) l with 
    Some (_hook1,mold1) -> Some(mold1)
  | None -> 
    let (Key(fis,upper_bound)) = key in 
    let domain = Finite_int_set.to_usual_int_list fis in 
    if Upper_bound_on_constraint.list_is_admissible upper_bound domain 
    then  Some(M([domain],[domain]))
    else Extra_tools.compute_opt key ;; 
 
  let compute_naively_opt pp key =
      let (d,translated_key) = Kay.decompose_wrt_translation key in 
      match compute_naively_without_translating_opt pp translated_key with 
          None -> None 
          |Some (translated_mold) -> 
             Some(Mold.translate (-d) translated_mold);;

  let compute_naively pp key = match compute_naively_opt pp key with 
      Some answer -> answer 
      | None -> raise(Compute_naively_exn(Kay.deconstructor key)) ;;            

  let check_fork pp (i,j,k) key (M(sols,l_ext)) = 
      let parts = Image.image (
        fun t->
            let subkey = Kay.remove_one_element key t in 
            (t,(subkey,compute_naively pp key)) 
      ) [i;j;k] in 
      let sols_with_pivots = Image.image (fun 
        sol->(sol,List.find (fun t->not(List.mem t sol)) [k;j;i])) sols in
      let unregistered_sols = List.filter_map (
        fun (sol,t) -> 
           let (key2,M(sols2,_l_ext2)) = List.assoc t parts in 
           if not(List.mem sol sols2)
           then Some(sol,key2)
           else None 
      ) sols_with_pivots in 
      if unregistered_sols<>[]
      then raise(Unregistered_solutions(unregistered_sols,key,Mh_fork(i,j,k)))  
      else 
      let untreated_cases = List.flatten(Image.image (fun (_t,(subkey,M(_sols3,l_ext3)))->
        List.filter_map (fun ext3->
          if List.for_all (fun ext->not(i_is_included_in ext ext3)) l_ext 
          then Some(ext3,subkey)
          else None   
        ) l_ext3 
      ) parts) in   
      if untreated_cases<>[]
      then raise(Untreated_cases(untreated_cases,key,Mh_fork(i,j,k)))  
      else () ;; 
      
    let check_select pp (i,j,k) key (M(sols,l_ext)) = 
      let (_,opt) = Option.get(Kay.largest_constraint_with_predecessor_opt key) in 
      let preceding_key = Option.get opt in 
      let (M(sols2,l_ext2)) = compute_naively pp preceding_key in 
      let unregistered_sols = List.filter_map (
          fun sol -> 
             if not(List.mem sol sols2)
             then Some(sol,preceding_key)
            else None 
      ) sols in 
      if unregistered_sols<>[]
      then raise(Unregistered_solutions(unregistered_sols,key,Mh_select(i,j,k)))  
      else 
      let untreated_cases = 
          List.filter_map (fun ext2->
            if List.for_all (fun ext->not(i_is_included_in ext ext2)) ([i;j;k]::l_ext) 
            then Some(ext2,preceding_key)
            else None   
      ) l_ext2  in   
      if untreated_cases<>[]
      then raise(Untreated_cases(untreated_cases,key,Mh_select(i,j,k)))  
      else () ;; 

     
    let check_cumulative pp pivot key (M(sols,l_ext)) = 
        let smaller_key = Kay.remove_one_element key pivot in 
        let (M(sols2,l_ext2)) = compute_naively pp smaller_key in 
        let unregistered_sols = List.filter_map (
            fun sol -> 
               let ssol = i_setminus sol [pivot] in  
               if not(List.mem ssol sols2)
               then Some(ssol,smaller_key)
              else None 
        ) sols in 
        if unregistered_sols<>[]
        then raise(Unregistered_solutions(unregistered_sols,key,Mh_cumulative(pivot)))  
        else 
        let (Key(_,ub_on_constraint)) = key in 
        let is_ok = Upper_bound_on_constraint.list_is_admissible ub_on_constraint in   
        let untreated_cases = 
            List.filter_map (fun ext2->
              let eext2 = i_insert pivot ext2 in 
              if (List.for_all (fun ext->not(i_is_included_in ext eext2)) l_ext)
                 &&
                 (is_ok eext2) 
              then Some(ext2,smaller_key)
              else None   
        ) l_ext2  in   
        if untreated_cases<>[]
        then raise(Untreated_cases(untreated_cases,key,Mh_cumulative(pivot)))  
        else () ;; 
  
    let check_item pp (skey,(hook,mold)) = 
        let key = Kay.constructor skey in 
        match hook with 
         Mh_cumulative(pivot) -> check_cumulative pp pivot key mold   
        |Mh_select(i,j,k) -> check_select pp (i,j,k) key mold  
        |Mh_fork(i,j,k) -> check_fork pp (i,j,k) key mold  ;;

    let check_all pp = 
        let (PP l) = pp in 
        List.iter (check_item pp) (List.rev l) ;;

    end ;;    
    
end ;; 



