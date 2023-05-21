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

type small_step = Sz3_types.small_step = St_cumulative of int | St_fork of int * int *int | St_import ;; 

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

  let untranslate d = function  
    Unrestricted -> Unrestricted 
   |Up_to(B b) -> Up_to(B(b-d)) ;; 
  
  
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
  
      let two_steps_back fis ub_on_constraint =
         let (UBC(W w,_)) = ub_on_constraint in 
         let bmax = a_priori_bound_on_breadth fis ub_on_constraint in 
         match attained_upper_bound_opt_for_dissociated_data fis (W w) bmax with 
         None -> None  
        |Some(UBC(W w1,ub_on_breadth1)) ->
            let (B b1) = Upper_bound_on_breadth.get ub_on_breadth1 in 
            let (preceding_width,preceding_breadth) =
             (
              if b1>1 
              then (W w1, B (b1-1))  
              else let (FIS(n,_)) = fis in 
                    (W (w1-1), B (n-2*(w1-1))) 
             ) in 
              match attained_upper_bound_opt_for_dissociated_data fis preceding_width preceding_breadth with 
              None -> Some(UBC(W 1,Unrestricted),[b1;b1+w1;b1+2*w1]) 
             |Some(UBC(W w2,ub_on_breadth2)) -> 
                 let adjusted_breadth_bound = 
                   (if w2<w1
                    then Unrestricted 
                    else ub_on_breadth2
                    ) in 
               Some(UBC(W w2,adjusted_breadth_bound),[b1;b1+w1;b1+2*w1])
             ;; 
  
  
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

 let two_steps_back = Private.two_steps_back ;;   

  let untranslate d (UBC(w,b)) = UBC(w,Upper_bound_on_breadth.untranslate d b) ;;  
     
  
  
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
       (d,Key(new_fis,Upper_bound_on_constraint.untranslate d ubc)) ;;

    exception Predecessor_exn of key ;; 

    let predecessor key =
        let (Key(fis,ub_on_constraint)) = key in 
        match Upper_bound_on_constraint.two_steps_back fis ub_on_constraint with 
        None -> raise(Predecessor_exn(key))
        |Some(new_ub,cstr)->(Key(fis,new_ub),cstr) ;; 

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
    M([List.flatten sol_components],List.flatten forced_elements);;

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
    M(Image.image (fun sol->sol@extra) sols2,ext2@extra);; 

   let compute_opt key = Some(compute key) ;; 

end ;;   

let compute_opt key =  
    match List.assoc_opt (Kay.width key) [
        W 1, Width_one.compute_opt
    ] with 
    None -> None 
    | Some f -> f key ;;  

end ;;  

module Peek = struct 

  exception For_fork_case_should_never_happen_1_exn of key ;;
  exception Multiple_exn of key ;; 
  exception Simplified_multiple_exn of key ;; 

  module Private = struct 

    let for_obvious_accesses hashtbl helper key = 
      match List.assoc_opt key helper with 
        Some answer1 -> P_Success(answer1)
      | None ->
         (
            match  Hashtbl.find_opt hashtbl key with 
            Some answer2 -> P_Success(answer2)
          | None -> 
            let (Key(fis,upper_bound)) = key in 
            let domain = Finite_int_set.to_usual_int_list fis in 
            if Upper_bound_on_constraint.list_is_admissible upper_bound domain 
            then  P_Success(M([domain],domain))
             else 
              (
                match Extra_tools.compute_opt key with 
                Some answer2 -> P_Success(answer2)
                |None ->P_Failure         
              ) 
           ) ;; 
    
    (*
    
    Note that the peek function below will not detect all
    cumulative cases.
    
    *)
    
    let for_cumulative_case hashtbl helper old_key pivot= 
        let new_key = Kay.remove_one_element old_key pivot in 
        let peek_res = for_obvious_accesses hashtbl helper new_key in 
          match peek_res with 
         P_Unfinished_computation(subcomp)  -> P_Unfinished_computation(subcomp@[new_key])
        |P_Failure -> P_Unfinished_computation([new_key]) 
        |P_Success(M(sols2,ext2)) ->
          let (Key(_,old_ub)) = old_key in 
          if not(Upper_bound_on_constraint.list_is_admissible old_ub (i_insert pivot ext2))
          then P_Success(M(sols2,[]))
          else
          let sols3 = List.filter_map (fun sol->
                      let increased_sol = i_insert pivot sol in 
                      if Upper_bound_on_constraint.list_is_admissible old_ub increased_sol 
                      then Some(increased_sol) 
                      else None    
          ) sols2 in 
          if sols3 <> [] 
          then P_Success(M(sols3,i_insert pivot ext2))  
          else P_Failure
      ;;
  
   let usual_cumulative_case hashtbl helper key =
        let (Key(fis,_)) = key in 
        let n =Finite_int_set.max fis in
        for_cumulative_case  hashtbl helper key n;; 

  (*
     
    We use translations as little as possible. Most of the functions
    of this module are supposed to work on arguments where translation
    does not apply. The function below is an exception.
    
    *)
  
    
  
    let seek_obvious_accesses_using_translation hashtbl helper original_key = 
      let (d,translated_key) = Kay.decompose_wrt_translation original_key in 
          match for_obvious_accesses hashtbl helper translated_key with 
         P_Unfinished_computation(_)  -> (None,Some translated_key)
        |P_Failure -> (None,Some translated_key)
        |P_Success(translated_answer) ->
           let answer_to_original = Mold.translate d translated_answer in 
           (Some(answer_to_original),None);; 

  let partition_leaves_in_fork_case hashtbl helper leaves =
    let leaves2 = Image.image (
        fun cand ->
          (cand,seek_obvious_accesses_using_translation hashtbl helper cand)
    ) leaves in 
    let (good_leaves,bad_leaves) = 
        List.partition (fun (_,(_,opt_bad)) -> opt_bad = None ) leaves2 in 
    (Image.image (fun ( cand,( opt_good,_opt_bad)) -> (cand,Option.get opt_good)) good_leaves,
     Image.image (fun (_cand,(_opt_good, opt_bad)) -> Option.get opt_bad        ) bad_leaves) ;; 
  
  let for_fork_case hashtbl helper old_key (i,j,k)= 
    let cstr = [i;j;k] in 
    let candidates = Image.image (
           fun i-> Kay.remove_one_element old_key i
    ) cstr in 
    let (candidates2,bad_ones) = partition_leaves_in_fork_case hashtbl helper candidates in 
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
          let ext5 = Image.image (fun (k,_)->List.nth cstr (k-1)) min_indices in 
          P_Success(M(sols5,ext5));;    
   
    
    
    let usual_fork_case hashtbl helper key =
            let (Key(fis,upper_bound)) = key in 
            let opt1 = Upper_bound_on_constraint.attained_upper_bound_opt fis upper_bound in 
            if opt1=None  
            then raise(For_fork_case_should_never_happen_1_exn(key))
            else    
            let UBC(W w,ub_on_breadth) = Option.get opt1 in   
            let (B b)=Upper_bound_on_breadth.get ub_on_breadth in  
            for_fork_case  hashtbl helper key (b,b+w,b+2*w);;       
  
    let multiple hashtbl helper old_key = 
      let peek_res1 = for_obvious_accesses hashtbl helper old_key in 
        match peek_res1 with 
        P_Success (_) 
      | P_Unfinished_computation (_) -> (peek_res1,false)  
      | P_Failure ->
      let peek_res2= usual_cumulative_case hashtbl helper old_key in 
        match peek_res2 with 
        P_Success (_) -> (peek_res2,true)
      | P_Unfinished_computation (_) -> (peek_res2,false)  
      | P_Failure -> 
      let peek_res3= usual_fork_case hashtbl helper old_key in 
        match peek_res3 with 
        P_Success (_) -> (peek_res3,true)
      | P_Unfinished_computation (_) -> (peek_res3,false)  
      | P_Failure -> raise(Multiple_exn(old_key)) ;; 
          
    let simplified_multiple hashtbl helper key =   
      let (peek_res,_)= multiple hashtbl helper key in 
      match peek_res with 
            P_Failure -> raise (Simplified_multiple_exn(key))
          | P_Unfinished_computation (new_to_be_treated) -> 
               (None,Some new_to_be_treated)
          | P_Success (answer) -> 
              (Some answer,None) ;; 

    


    end ;;           

let cumulative_case = Private.for_cumulative_case ;;

let fork_case = Private.for_fork_case ;;
let multiple = Private.multiple ;; 
let simplified_multiple = Private.simplified_multiple ;; 
let usual_cumulative_case = Private.usual_cumulative_case ;;
let usual_fork_case = Private.usual_fork_case ;;

end ;;  

module Compute = struct 

    exception Pusher_for_needed_subcomputations_exn_1 ;; 
    exception Pusher_for_needed_subcomputations_exn_2 ;; 

    let impatient_opt hashtbl key = 
        fst(Peek.simplified_multiple hashtbl [] key);;

    let pusher_for_needed_subcomputations hashtbl (helper,to_be_treated) =
        match to_be_treated with 
         [] -> raise (Pusher_for_needed_subcomputations_exn_1) 
        |key :: others ->
          let (peek_res,to_be_remembered)= Peek.multiple hashtbl helper key in
          (
            match peek_res with 
            P_Failure -> raise (Pusher_for_needed_subcomputations_exn_2)
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
  
     let rec iterator_for_needed_subcomputations hashtbl walker = 
        if snd walker = [] then List.rev(fst walker) else 
        let new_walker = pusher_for_needed_subcomputations hashtbl walker in      
        iterator_for_needed_subcomputations hashtbl new_walker ;;
  
     let needed_subcomputations hashtbl items = 
      iterator_for_needed_subcomputations hashtbl ([],items) ;;  
      
     
     
  
  end ;;  


module Hashtbl_here = struct 

  let greedy = ((Hashtbl.create 50) : (key, mold) Hashtbl.t) ;; 
  let cautious = ((Hashtbl.create 50) : (key, mold) Hashtbl.t) ;;   
  
  let add_to_all key answer =
      (
        Hashtbl.replace cautious key answer;
        Hashtbl.replace greedy key answer;
      ) ;;

  let add_to_greedy_only key answer =
      Hashtbl.replace greedy key answer;;    

end ;;
  
module High_level = struct 

  module Private = struct 

  let compute_recursively_and_remember key = 
      let hashtbl = Hashtbl_here.greedy in 
      match Hashtbl.find_opt hashtbl key with 
      Some(old_answer) -> old_answer
      | None ->
      let subcomps = Compute.needed_subcomputations hashtbl [key] in 
      if subcomps = []
      then Option.get(Compute.impatient_opt hashtbl key)
      else    
      let _ = List.iter (fun (key,answer)->
        Hashtbl_here.add_to_greedy_only key answer
      ) subcomps in 
      List.assoc key subcomps ;;      

  let measure key =
     let (M(sols,_)) = compute_recursively_and_remember key in 
     List.length(List.hd sols) ;;  
      
  let rec find_threshhold_triple triple =
      let ((_,ms),(predecessor,mp),cstr) = triple in 
       if ms<>mp 
       then (predecessor,cstr)
       else let (predecessor2,cstr2) = Kay.predecessor predecessor in 
            let mp2 = measure predecessor2 in 
            find_threshhold_triple ((predecessor,mp),(predecessor2,mp2),cstr2) ;;  

  let unchecked_threshhold_decomposition key =
     let mkey = measure key 
     and (key2,cstr) = Kay.predecessor key in 
     let mkey2 = measure key2 in 
     find_threshhold_triple ((key,mkey),(key2,mkey2),cstr) ;;  

  let threshhold_decomposition_opt key =
      try Some(unchecked_threshhold_decomposition key) with 
      _ -> None ;; 

  let rigorous_quest_for_cumulative_case old_key = 
    let (n,simpler_key) = Kay.vertex_decomposition old_key in 
    let res1 = compute_recursively_and_remember simpler_key 
    and res2 = compute_recursively_and_remember old_key in 
    let M(sols1,_ext1) = res1 
    and M(sols2,_ext2) = res2 in 
    if List.length(List.hd sols2)=List.length(List.hd sols1)+1 
    then Some([n],(res1,res2),[simpler_key])  
    else None ;;

  let rigorous_quest_for_fork_case initial_key = 
      match threshhold_decomposition_opt initial_key with  
      None -> None 
      |Some(_,cstr) -> 
      let bare_candidates = Image.image 
      (Kay.remove_one_element initial_key) cstr in 
      let candidates = Image.image (
             fun cand-> (cand,compute_recursively_and_remember cand)
      ) bare_candidates 
      and translated_candidates = Image.image (
        fun cand-> snd(Kay.decompose_wrt_translation cand)
      ) bare_candidates in 
      let sizes = Image.image (fun (_,M(sol,_ext))->List.length(List.hd sol)) candidates in 
      let first_size = List.hd sizes in 
      if List.for_all (fun size->size=first_size) sizes 
      then Some(cstr,candidates,translated_candidates)
      else None;;   
  
  exception Assess_exn of key ;; 

  let assess key = 
    let selector = List.filter (fun cand->
      (Compute.impatient_opt Hashtbl_here.cautious cand)=None
    ) in
    match rigorous_quest_for_cumulative_case key with 
    Some(single,_,candidates)-> (St_cumulative(List.hd single), selector candidates)
    | None ->
      (
       match rigorous_quest_for_fork_case key with 
       Some(cstr,_,candidates)-> 
             let elt = (fun k->List.nth cstr (k-1)) in 
             ((St_fork(elt 1,elt 2,elt 3)), selector candidates)
       | None -> raise(Assess_exn(key))
      );;

    let needed_nodes key =
        let temp1 = Stabilize.explore_enhanced_tree assess [key] in 
        let temp2 = Image.image (
          fun (key,(small_step,_)) -> 
             (Kay.deconstructor key,small_step)
        ) temp1 in 
        temp2;; 

    end ;;
    
    let assess = Private.assess ;; 
    let compute = Private.compute_recursively_and_remember ;; 
    let needed_nodes = Private.needed_nodes ;; 

end ;;   
  


module Small_step = struct 
  
  exception Compute_easy_cumulative_exn of key ;;
  exception Compute_easy_fork_exn of key ;;
  exception Import_exn1 of key ;;    
  exception Import_exn2 of key ;;  

  module Private = struct

  let compute_easy_cumulative pivot key =
     match Peek.cumulative_case Hashtbl_here.cautious [] key pivot with 
     P_Success(answer) -> answer 
   | P_Unfinished_computation(_)
   | P_Failure -> raise(Compute_easy_cumulative_exn(key)) ;; 
    
  let add_easy_cumulative pivot key =
      let answer = compute_easy_cumulative pivot key in 
      Hashtbl_here.add_to_all key answer ;; 
 
  let compute_easy_fork (i,j,k) key =
      match Peek.fork_case Hashtbl_here.cautious [] key (i,j,k) with 
      P_Success(answer) -> answer 
    | P_Unfinished_computation(_)
    | P_Failure -> raise(Compute_easy_fork_exn(key)) ;; 

  let add_easy_fork (i,j,k) key =
      let answer = compute_easy_fork (i,j,k) key in 
      Hashtbl_here.add_to_all key answer ;;  

  let import key = 
     let lower_key = Kay.decrement key 
     and (Key(_,ub_on_constraints))= key in 
     match Compute.impatient_opt Hashtbl_here.cautious lower_key with 
     None -> raise(Import_exn1(key))
     |Some(M(sols,ext)) ->    
     let sols2 = List.filter (Upper_bound_on_constraint.list_is_admissible ub_on_constraints) sols in 
      if sols2 = []
      then raise(Import_exn2(key))
      else 
      let answer = M(sols2,ext) in   
      Hashtbl_here.add_to_all key answer;;
   
    end ;;

   let apply = function 
       St_cumulative pivot -> Private.add_easy_cumulative pivot
      |St_fork(i,j,k) -> Private.add_easy_fork (i,j,k)
      |St_import -> Private.import ;;      

end ;;   


module Fill = struct 

  module Private = struct 

  let bound = 40 ;; 
  let apply ((n,scr,w,b),small_step)=
    Small_step.apply small_step (Kay.constructor(n,scr,w,b)) ;;


   let for_level2 = [
        ((7,[4],2,0),St_import);
   ] ;; 
   
   let for_level3 = [
        ((5,[],3,0),St_import);
        ((6,[],3,0),St_import);
        ((7,[],3,0),St_fork(1,3,7));
   ] ;; 

   end ;;

   let fill () =
    let _ = Int_range.scale (fun k->Private.apply ((k,[],2,0),St_import)) 1 Private.bound in 
    (
    List.iter Private.apply Private.for_level2;
    List.iter Private.apply Private.for_level3;
    )
  ;;    

  let reset () = 
    (
       Hashtbl.clear Hashtbl_here.cautious ;
       Hashtbl.clear Hashtbl_here.cautious ;
       fill ();
    ) ;;

end ;;
