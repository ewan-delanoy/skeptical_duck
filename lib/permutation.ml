(*

#use"lib/permutation.ml";;

*)

module Private = struct

   let rec helper_for_first_decomposition (treated,border_val,others) =
      match others with 
      [] -> (None,border_val::treated)
      |next_border_val :: remaining_others ->
        if border_val < next_border_val 
        then helper_for_first_decomposition (border_val::treated,next_border_val,remaining_others)
        else (Some(List.rev remaining_others,next_border_val),border_val::treated)
        ;;    
   
   let first_decomposition l= 
      let (a,others) = Listennou.ht (List.rev l) in 
      helper_for_first_decomposition ([],a,others) ;;     
   
   let rec helper_for_second_decomposition (above_pivot,pivot,not_treated_yet) =
      match not_treated_yet with 
      [] -> (List.rev above_pivot,not_treated_yet) 
      |a :: others ->
         if a > pivot 
         then helper_for_second_decomposition (a::above_pivot,pivot,others)
         else (List.rev above_pivot,a :: others) ;;
   
   let second_decomposition (pivot,decreasing_sequence) =
      helper_for_second_decomposition ([],pivot,decreasing_sequence) ;;
   
   
   
   let opt_next_permutation l=
      let (opt,decreasing_sequence) = first_decomposition l in 
      match opt with 
      None -> None
      |Some(before_pivot,pivot) ->
         let (above_pivot,below_pivot) = second_decomposition (pivot,decreasing_sequence)  in 
         let (successor,above_successor) = Listennou.ht (List.rev above_pivot) in 
         Some(before_pivot@(successor::(List.rev_append below_pivot (pivot::above_successor))));;
         
   
   (*
   first_decomposition [3;1;4;6;8;7;5;4;2];;
   second_decomposition (6,[8;7;5;4;2]);; 
   opt_next_permutation [3;1;4;6;8;7;5;4;2];;
   *)
   
   let rec helper_for_enumeration (item,already_treated) = 
     match   opt_next_permutation item with 
     None -> List.rev (item :: already_treated) 
     |Some (next_item) -> helper_for_enumeration (next_item,item :: already_treated) ;;
   
     let rec helper_for_signature (sign,perm) =
      let n = List.length perm in 
      if n<2 then sign else 
      let j = Listennou.find_index n perm 
      and shorter_perm = Listennou.big_head (n-1) perm in 
      if j = n 
      then helper_for_signature (sign,shorter_perm) 
      else 
      let m = List.nth perm (n-1) in 
      let new_perm = Image.image (fun x->if x=n then m else x) shorter_perm in 
      helper_for_signature (-sign,new_perm) ;;
      
   let signature perm = helper_for_signature (1,perm) ;;     
   
   let integer_initial_interval  n = 
      helper_for_enumeration (Int_range.range 1 n,[]);;   
 
   let rewrite_cycle_with_min_at_the_beginning cycle =
       let m = Min.list cycle in 
       let j = Listennou.find_index  m cycle in 
       let (before,after) = Listennou.big_rht (j-1) cycle in 
       after @ (List.rev before) ;;

   (*
      
   rewrite_cycle_with_min_at_the_beginning [1;5;4];;
   rewrite_cycle_with_min_at_the_beginning [3;2;1];;
   rewrite_cycle_with_min_at_the_beginning [3;5;7;1;2;4;6];;

   *)    

   let rec helper_for_subcycle_computation 
       (perm,initial_seed,to_be_treated,treated) = 
        if to_be_treated = initial_seed 
        then rewrite_cycle_with_min_at_the_beginning(List.rev treated) 
        else 
        let new_val = List.nth perm (to_be_treated-1) in 
        helper_for_subcycle_computation 
       (perm,initial_seed,new_val,to_be_treated::treated)
      ;;   

   let compute_full_subcycle_from_point perm initial_seed = 
      let first_computed_val = List.nth perm (initial_seed-1) in 
      if first_computed_val = initial_seed 
      then [initial_seed]      
      else helper_for_subcycle_computation (perm,initial_seed,first_computed_val,[initial_seed]) ;;

   (*
   
   compute_full_subcycle_from_point [2; 3; 4; 1; 5; 7; 6; 8; 10; 11; 9] 11 ;;

   *) 

   let find_cycle_inside perm =
       let n = List.length perm in 
       match Option.seek (fun d->
         let t = n+1-d in List.nth perm (t-1) <> t   
      ) (Int_range.range 1 n) with 
       None -> None 
       | Some d0 ->
         let t0 = n+1-d0 in 
         let cycle = compute_full_subcycle_from_point perm t0 in 
         let cleaned_perm = Int_range.scale (fun x->
            if List.mem x cycle then x else List.nth perm (x-1)
         ) 1 n in 
         Some(cycle,cleaned_perm) ;; 

   (*
      
   find_cycle_inside [2; 3; 4; 1; 5; 7; 6; 8; 10; 11; 9]  ;;

   *)

   let reorder_cycles_by_min_element cycles = 
       let order_for_pairs =
           Total_ordering.product 
              Total_ordering.standard Total_ordering.standard 
       and temp1 = Image.image (fun cycle->(List.hd cycle,cycle)) cycles in 
       let temp2 = Ordered.sort order_for_pairs temp1 in 
       Image.image snd temp2 ;;

   let rec helper_for_cycle_decomposition (walker_perm,treated)=
      match find_cycle_inside walker_perm with 
      None -> (reorder_cycles_by_min_element treated)
      |Some(new_cycle,new_perm) -> 
         helper_for_cycle_decomposition (new_perm,new_cycle ::treated) ;;

   let decompose_into_disjoint_cycles perm =
      helper_for_cycle_decomposition (perm,[]) ;;  

   (*
      
   decompose_into_disjoint_cycles [2; 3; 4; 1; 5; 7; 6; 8; 10; 11; 9] ;;

   *)   


   let evaluate_cycle_at_point cycle k=
     let m = List.length cycle 
     and j = Listennou.find_index k cycle in 
     if j = m 
     then List.hd cycle 
     else List.nth cycle j ;;    

   let product_of_cycles cycles = 
       let n = Max.list (List.flatten cycles) in 
       Int_range.scale (fun k->
         match Option.seek (List.mem k) cycles with 
         None -> k
         | Some cycle ->
            evaluate_cycle_at_point cycle k
       ) 1 n;;
 
   (*
      
   let z0 = [[1;2;3;4];[6;7];[9;10;11]]
   let z1 = product_of_cycles z0 ;;
   let z2 = decompose_into_disjoint_cycles z1 ;;
   let check = (z2 = z0) ;;

   *)    
 
   let order perm =
       let cycles = decompose_into_disjoint_cycles perm in 
       if cycles = [] then 1 else 
       Gcd.lcm_for_many (Image.image List.length cycles) ;;  

   (*
   
   order [2;3;4;5;1] ;;
   
   *)

   let product sigma1 sigma2 =
      Image.image (fun s2->List.nth sigma1 (s2-1)) sigma2 ;;


   let cyclic_subgroup perm =
       let w = order perm in 
       if w=1 then [perm] else
       let walker = ref (perm,[]) in 
       let _ =for k=1 to w do
         let (to_be_treated,treated) = (!walker) in 
         let new_val = product perm to_be_treated in 
         walker:=(new_val,to_be_treated::treated)
       done in 
       let (last_perm,other_perms) = (!walker) in 
       Ordered.sort Total_ordering.silex_for_intlists (last_perm::other_perms)
      ;;  

   (*
   
   cyclic_subgroup [2;3;4;5;1] ;;
   
   *)


   end ;; 
   
let alternating_group  = Memoized.make(fun n->
      List.filter (fun perm->Private.signature perm=1) 
      (Private.integer_initial_interval n)  );;   
   
let cyclic_subgroup = Private.cyclic_subgroup ;;

let decompose_into_disjoint_cycles =  Private.decompose_into_disjoint_cycles ;;    
   
let iii (* meaning, integer initial interval *) 
   = Memoized.make(fun n->
      Private.integer_initial_interval n);;
   
let permutations l =
      let initial_item = Ordered.sort Total_ordering.standard l in 
      Private.helper_for_enumeration (initial_item,[]);;   
   
let product = Private.product ;;
   
let product_of_cycles = Private.product_of_cycles ;;     
       
let inverse sigma = 
      let n = List.length sigma in 
      Int_range.scale (fun y->Listennou.find_index y sigma) 1 n ;;
   
let order = Private.order ;;      

let signature = Private.signature ;;   
         
   
   