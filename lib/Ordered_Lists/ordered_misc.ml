(* 

#use"lib/Ordered_Lists/ordered_misc.ml";;

*)

module Private = struct 

  let oi = Total_ordering.for_integers ;;

  let rec helper_for_maximal_elts_wrt_inclusion (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
       if List.exists (fun b->
         (b<>a) && ( Ordered.is_included_in oi a b) ) others
       then helper_for_maximal_elts_wrt_inclusion (already_treated,others)
       else 
        let temp1 = List.filter (fun b->
           not(Ordered.is_included_in oi b a)) others in
        helper_for_maximal_elts_wrt_inclusion (a :: already_treated,temp1) ;;  

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
     Ordered.sort (Total_ordering.silex_compare oi) (List_again.power_set x) ;; 

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
     let temp1 = minimal_elts_in_int_upwards_filter normalized_f (Int_range.range 1 n) in 
     Image.image get_several temp1 ;;

let commonest_elements ord ll = 
      let temp1 = Ordered.fold_merge ord ll in 
      let (_,temp2) = Max.maximize_it_with_care (
        fun y->List.length(List.filter (Ordered.mem ord y) ll)
      ) temp1 in 
      temp2;; 


let rec helper_for_greedy_transversal (history,treated,to_be_treated) =
   if to_be_treated = [] 
   then (List.rev history,treated)
   else
   let temp1 = commonest_elements oi to_be_treated in 
   let a = List.hd temp1 in 
   helper_for_greedy_transversal(temp1::history,Ordered.insert oi a treated,
     List.filter (fun x->not(Ordered.mem oi a x)) to_be_treated) ;;  

 let greedy_transversal ll =
  helper_for_greedy_transversal ([],[],ll) ;;
   
let generated_algebra_for_two ord x y = 
  let setminus = Ordered.setminus ord 
  and intersect = Ordered.intersect ord in 
  List.filter (fun z->z<>[])
    [setminus x y;intersect x y;setminus y x] ;;

let rec find_atom intersect candidate testers = 
  match List.find_map ( 
    fun x -> 
      let y = intersect x candidate in 
      if (y<>[])&&(y<>candidate)
      then Some y 
      else None  
  ) testers with
  None -> candidate 
  | Some better_candidate ->
     find_atom intersect better_candidate testers ;;


let rec helper_for_generated_algebra 
     (intersect,setminus) (treated,to_be_treated) = 
  match to_be_treated with 
  [] -> treated 
  |elt :: _ ->
    let new_atom = find_atom intersect elt to_be_treated in 
    let to_be_treated2 = List.filter_map (
      fun x -> 
        let y = setminus x new_atom in 
        if y<>[]
        then Some y 
        else None  
    ) to_be_treated in 
    helper_for_generated_algebra 
    (intersect,setminus) (new_atom::treated,to_be_treated2) ;;

let generated_algebra ord l = 
  let intersect = Ordered.intersect ord 
  and setminus = Ordered.setminus ord in 
  Ordered.sort (Total_ordering.silex_compare ord)(
  helper_for_generated_algebra 
    (intersect,setminus) ([],l)) ;; 

(*

generated_algebra Total_ordering.for_integers 
 [[1;3;5;72;73];[2;3;6;72;73];[4;5;6;72;73]] ;; 

*)

end ;;        


let commonest_elements = Private.commonest_elements ;;

let generated_algebra = Private.generated_algebra ;;

let greedy_transversal = Private.greedy_transversal ;;

let maximal_elts_wrt_inclusion l= 
  Private.helper_for_maximal_elts_wrt_inclusion ([],l) ;;

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
     List_again.find_index_of_in x singles   
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


let underline_new_elements ord old_set possibly_new_elts =
  let new_elts = Ordered.setminus ord possibly_new_elts old_set in 
  let new_whole = Ordered.merge ord old_set new_elts in 
  let temp1 = Image.image (fun x->(x,List.mem x new_elts)) new_whole in 
  let temp2 = Hurried.connected_components snd temp1 in 
  Image.image (fun part -> (Image.image fst part,snd(List.hd part)) ) temp2;;

(*

underline_new_elements Total_ordering.standard 
 [1; 2;  5; 7; 8;  11; 13; 14; 16; 17; 19; 20]
 [3;4; 6; 9;10; 12; 15; 18; 21] ;;

*)


