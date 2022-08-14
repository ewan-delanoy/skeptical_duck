(*

#use"permutation.ml";;

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

   end ;; 
   
let alternating_group  = Memoized.make(fun n->
      List.filter (fun perm->Private.signature perm=1) 
       (Private.helper_for_enumeration (Int_range.range 1 n,[]))  );;   
   
   
let iii (* meaning, integer initial interval *) 
   = Memoized.make(fun n->
      Private.integer_initial_interval n);;
   
let permutations l =
      let initial_item = Ordered.sort Total_ordering.standard l in 
      Private.helper_for_enumeration (initial_item,[]);;   
   
let product sigma1 sigma2 =
       Image.image (fun s2->List.nth sigma1 (s2-1)) sigma2 ;;
   
let inverse sigma = 
      let n = List.length sigma in 
      Int_range.scale (fun y->Listennou.find_index y sigma) 1 n ;;
   
let signature = Private.signature ;;   
         
   
   