(* 

#use"Ordered_Lists/ordered_misc.ml";;

*)

module Private = struct 

  let rec helper_for_minimal_elts_wrt_inclusion (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
       if List.exists (fun b->
         (b<>a) && ( Set_of_integers.is_included_in b a) ) others
       then helper_for_minimal_elts_wrt_inclusion (already_treated,others)
       else 
        let temp1 = List.filter (fun b->
           not(Set_of_integers.is_included_in a b)) others in
        helper_for_minimal_elts_wrt_inclusion (a :: already_treated,temp1) ;;  

  let rec helper_for_minimal_transversals (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
      let temp1 = Cartesian.product (Set_of_integers.forget_order a) already_treated in 
      let temp2 = Image.image (fun (x,y)->Set_of_integers.insert x y) temp1 in 
      let temp3 = helper_for_minimal_elts_wrt_inclusion ([],temp2) in 
      helper_for_minimal_transversals (temp3,others) ;;        

end ;;        

let minimal_elts_wrt_inclusion l= 
  Private.helper_for_minimal_elts_wrt_inclusion ([],l) ;;

  
let minimal_transversals l= 
  match l with 
  [] -> []
  | a:: others ->
    let starter = Image.image (fun x->Set_of_integers.singleton x) 
     (Set_of_integers.forget_order a) in    
  Private.helper_for_minimal_transversals (starter,others) ;;  
  

let translate_at_level_two ll translation=
  Image.image (
    fun l->Ordered.merge Total_ordering.for_integers l translation
  ) ll ;;
