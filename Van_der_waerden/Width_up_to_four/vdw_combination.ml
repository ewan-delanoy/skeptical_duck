(*

#use"Van_der_Waerden/Width_up_to_four/vdw_combination.ml";;

*)

module Private = struct 

   let oord = 
      Total_ordering.silex_compare 
         Total_ordering.for_integers ;;

   let merge = Ordered.merge Total_ordering.for_integers ;;   
   
   let order_for_pairs =
      Total_ordering.product 
      Vdw_nonempty_index.order
       (Total_ordering.silex_compare Total_ordering.for_integers)
          ;;
   
   let intlist_to_string l=
     "["^
      (String.concat "," (Image.image string_of_int l))^
     "]" ;; 

   let to_string (Vdw_combination_t.C l) = 
      let temp1 = Image.image (fun (core,translation)->
       (Vdw_nonempty_index.to_string core)^"."^
       (intlist_to_string translation)   
      ) l in 
      String.concat " + " temp1 ;;

   end ;;   
   
   let constructor l =
      Vdw_combination_t.C (Ordered.sort Private.order_for_pairs l) ;;
        
   
   let expand_fully (Vdw_combination_t.C l)= 
     let temp1 = Image.image (
        fun (core,translation) ->
          Vdw_variable.homogeneous_translation core translation 
     ) l in 
     let temp2 = Option.filter_and_unpack (
        function 
        Vdw_homogeneous_translation_result_t.Nothing_taken -> None 
        |Vdw_homogeneous_translation_result_t.All_taken(ll) -> Some ll
     ) temp1 in 
     Ordered.fold_merge Private.oord temp2 ;;
   
   exception Homogeneous_translation_exn of Vdw_nonempty_index_t.t * (int list) * ( (int list) * (int list) );;
   
   let homogeneous_translation 
    (Vdw_combination_t.C l) translation =
    let temp1 = Image.image (fun (core1,translation1) ->
       (core1,Private.merge translation1 translation)   
   ) l  in 
   let tempf = (fun (core1,translation1)->
      try ((core1,translation1),
      Vdw_variable.homogeneous_translation core1 (translation1))   with 
      Vdw_common.Homogeneous_translation_exn(tr,(l1,l2)) ->
         raise(Homogeneous_translation_exn(core1,tr,(l1,l2)) )
   ) in 
   let temp2 = Image.image tempf temp1 in 
   let temp3 = Option.filter_and_unpack (
       fun ((core1,translation1),res)-> match res with 
       Vdw_homogeneous_translation_result_t.Nothing_taken -> None 
       |Vdw_homogeneous_translation_result_t.All_taken(ll) ->
        Some((core1,translation1),ll)
   ) temp2 in
    (constructor (Image.image fst temp3),
    Ordered.fold_merge Private.oord (Image.image snd temp3));;
   
   
   
   let replace_with_in (x,combination_for_x) combination_for_y =
     let (Vdw_combination_t.C content_for_y) = combination_for_y in
     let (before,opt,after) = 
        Three_parts.select_center_element_and_reverse_left 
        (fun (core,translation)->core = x) content_for_y in 
     match opt with 
      None -> combination_for_y  
     |Some(_,translation) -> 
        let (Vdw_combination_t.C content_for_x) = combination_for_x in 
       let new_center = Image.image (
        fun (core1,translation1)->
              (core1,Ordered.merge Total_ordering.for_integers 
              translation translation1)
       ) content_for_x in 
       constructor(List.rev_append before (new_center@after)) ;;
   
   let to_string = Private.to_string ;;

   let union (Vdw_combination_t.C l1) (Vdw_combination_t.C l2) =
        constructor (l1@l2) ;; 



        