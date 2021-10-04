(* 

#use"Ordered_Lists/ordered_misc.ml";;

*)

module Private = struct 

  let oi = Total_ordering.for_integers ;;

  let rec helper_for_minimal_elts_wrt_inclusion (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
       if List.exists (fun b->
         (b<>a) && ( Ordered.is_included_in oi b a) ) others
       then helper_for_minimal_elts_wrt_inclusion (already_treated,others)
       else 
        let temp1 = List.filter (fun b->
           not(Ordered.is_included_in oi a b)) others in
        helper_for_minimal_elts_wrt_inclusion (a :: already_treated,temp1) ;;  

  let rec helper_for_minimal_transversals (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
      let temp1 = Cartesian.product a already_treated in 
      let temp2 = Image.image (fun (x,y)->Ordered.insert oi x y) temp1 in 
      let temp3 = helper_for_minimal_elts_wrt_inclusion ([],temp2) in 
      helper_for_minimal_transversals (temp3,others) ;;        

  let power_set_for_intset x =
     Ordered.sort (Total_ordering.silex_compare oi) (Listennou.power_set x) ;; 

  let rec helper_for_int_upwards_filter (f,treated,to_be_treated) = 
     match to_be_treated with 
     [] -> List.rev treated 
     | a::others ->
         if f a 
         then let remaining_ones = List.filter (
                fun b->not(Ordered.is_included_in oi a b)
              ) to_be_treated in 
              helper_for_int_upwards_filter (f,a::treated,remaining_ones)
         else helper_for_int_upwards_filter (f,treated,others) ;;    

  let naive_minimal_elts_in_int_upwards_filter f base =
    let pb = power_set_for_intset base in 
    helper_for_int_upwards_filter (f,[],pb) ;;     

  let minimal_elts_in_int_upwards_filter f base =
     let indispensable_ones = List.filter (
       fun x->(not(f(Ordered.outsert oi x base)))
     ) base in 
     let dispensable_ones = Ordered.setminus oi base indispensable_ones in 
     let pullbacked_f = (fun y->f(Ordered.merge oi indispensable_ones y)) in 
     let temp1 = naive_minimal_elts_in_int_upwards_filter pullbacked_f dispensable_ones in 
     Image.image (Ordered.merge oi indispensable_ones) temp1 ;;

  (*   
  let base0 = Ennig.ennig 1 5 ;;
  let f0 y= List.exists (fun x->Ordered.is_included_in oi x y) [[1;2;3];[3;4;5]] ;;
  let res0 = minimal_elts_in_int_upwards_filter f0 base0 ;;
  *)

  let minimal_elts_in_upwards_filter f base =
     let get_one = (fun k->List.nth base (k-1)) in 
     let get_several = Image.image get_one in 
     let n = List.length base 
     and normalized_f = (fun indices ->f(get_several indices)) in 
     let temp1 = minimal_elts_in_int_upwards_filter normalized_f (Ennig.ennig 1 n) in 
     Image.image get_several temp1 ;;


end ;;        

let minimal_elts_in_upwards_filter = Private.minimal_elts_in_upwards_filter ;;

let minimal_elts_wrt_inclusion l= 
  Private.helper_for_minimal_elts_wrt_inclusion ([],l) ;;

  
let minimal_transversals l= 
  match l with 
  [] -> []
  | a:: others ->
    let starter = Image.image (fun x->[x]) a in    
  Private.helper_for_minimal_transversals (starter,others) ;;  
  

let reorder_list_of_pairs_using_list_of_singles pairs singles =
  let idx=Memoized.make(fun x->
     Listennou.find_index x singles   
  ) in 
  let ordr = (fun x1 x2 -> Total_ordering.for_integers (idx x1) (idx x2)) in 
  let ordr2 = Total_ordering.product ordr Total_ordering.standard in 
  Ordered.sort ordr2 pairs ;;

(*

reorder_list_of_pairs_using_list_of_singles (Ennig.doyle( fun t->(t,t+100)) 1 8)
[2;5;3;6;4;7;1;8] ;;
*)


let translate_at_level_two ll translation=
  Image.image (
    fun l->Ordered.merge Total_ordering.for_integers l translation
  ) ll ;;
