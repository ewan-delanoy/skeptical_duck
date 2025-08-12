(*

#use"lib/Linear_programming_with_Ocaml/lpwo_affine_form.ml";;

*)

module Private = struct 

let constructor l =
   let unordered_vars = Image.image snd l in 
   let ordered_vars = Ordered.sort Lpwo_variable.order unordered_vars in 
   Lpwo_affine_form_t.F( List.filter_map (
     fun v -> 
      let coeffs = List.filter_map (fun (lamb,v2)->
        if v2 = v then Some lamb else None
      ) l in 
      let final_coeff = Zirath.Q.fold_sum coeffs in 
      if Zirath.Q.equals final_coeff Zirath.Q.zero 
      then None 
      else Some(final_coeff,v)  
   ) ordered_vars ) ;; 

   let rec helper_for_sum (l1,l2,accu)=
     match l1 with 
      [] -> List.rev_append(accu)(l2)
      |(lamb1,v1) :: others1 ->
        match l2 with 
      [] -> List.rev_append(accu)(l1)
      |(lamb2,v2) :: others2 ->
      match Lpwo_variable.order(v1)(v2) with
        Total_ordering_result_t.Lower->helper_for_sum(others1,l2,(lamb1,v1)::accu)
      |Total_ordering_result_t.Greater->helper_for_sum(l1,others2,(lamb2,v2)::accu)
      |Total_ordering_result_t.Equal->
        let lamb = Zirath.Q.add lamb1 lamb2 in
        if Zirath.Q.equals lamb Zirath.Q.zero 
        then helper_for_sum(others1,others2,(lamb,v1)::accu) 
        else helper_for_sum(others1,others2,(lamb,v1)::accu)
      ;;

  let sum (Lpwo_affine_form_t.F l1) (Lpwo_affine_form_t.F l2) =
    Lpwo_affine_form_t.F(helper_for_sum (l1,l2,[]));;

  let zero = Lpwo_affine_form_t.F [] ;;

  let dot lamb (Lpwo_affine_form_t.F l) =
    if Zirath.Q.equals lamb Zirath.Q.zero 
    then zero 
    else Lpwo_affine_form_t.F (Image.image (
      fun (lamb2,v) -> (Zirath.Q.mul lamb lamb2,v) 
    )l);;

  let fold_sum l = List.fold_left sum zero l ;;  

  let linear_combination l=
    let temp = Image.image (fun (lamb,form) ->dot lamb form) l in  
    fold_sum temp ;; 

  let subst (Lpwo_affine_form_t.F l) vaar vaal =
    let temp = Image.image (
      fun (lamb,v) ->
        if v = vaar 
        then dot lamb vaal 
        else Lpwo_affine_form_t.F [lamb,v] 
    )  l in 
    fold_sum temp ;; 

  let uncurried_susbt form (vaar,vaal) =
    subst form vaar vaal ;;

end ;;  

let dot = Private.dot ;;

let subst = Private.subst;;
let sum = Private.sum ;;