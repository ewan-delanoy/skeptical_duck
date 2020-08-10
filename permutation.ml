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

end ;; 

let permutations l =
   let initial_item = Ordered.sort Total_ordering.standard l in 
   Private.helper_for_enumeration (initial_item,[]);;   

let iii (* meaning, integer initial interval *) n = 
   Private.helper_for_enumeration (Ennig.ennig 1 n,[]);;

   
      

