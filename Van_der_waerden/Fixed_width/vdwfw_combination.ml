(*

#use"Van_der_Waerden/Fixed_width/vdwfw_combination.ml";;

*)

exception Empty_union ;;

module Private = struct 

   let oord = 
      Total_ordering.silex_compare 
         Total_ordering.for_integers ;;

   let merge = Ordered.merge Total_ordering.for_integers ;;   
   
   let order_for_pairs =
      Total_ordering.product 
      Vdwfw_nonempty_index.order
       (Total_ordering.silex_compare Total_ordering.for_integers)
          ;;

   let to_string (Vdwfw_combination_t.C l) = 
      let temp1 = Image.image (fun (core,translation)->
       (Vdwfw_nonempty_index.to_string core)^"."^
       (Strung.of_intlist translation)   
      ) l in 
      String.concat " + " temp1 ;;

   let constructor l =
         Vdwfw_combination_t.C (Ordered.sort order_for_pairs l) ;;

   let union (Vdwfw_combination_t.C l1) (Vdwfw_combination_t.C l2) =
      constructor (l1@l2) ;;    

   end ;;   
   
   let constructor  = Private.constructor ;;
        
   
   let expand_fully (Vdwfw_combination_t.C l)= 
     let temp1 = Image.image (
        fun (core,translation) ->
          Vdwfw_variable.homogeneous_translation core translation 
     ) l in 
     let temp2 = Option.filter_and_unpack (
        function 
        Vdw_homogeneous_translation_result_t.Nothing_taken -> None 
        |Vdw_homogeneous_translation_result_t.All_taken(ll) -> Some ll
     ) temp1 in 
     Ordered.fold_merge Private.oord temp2 ;;
   
   let fold_union l = match l with 
       [] -> raise(Empty_union)
      |comb :: others -> List.fold_left Private.union comb others ;;

   exception Homogeneous_translation_exn of Vdwfw_nonempty_index_t.t * (int list) * ( (int list) * (int list) );;
   
   let homogeneous_translation 
    (Vdwfw_combination_t.C l) translation =
    let temp1 = Image.image (fun (core1,translation1) ->
       (core1,Private.merge translation1 translation)   
   ) l  in 
   let tempf = (fun (core1,translation1)->
      try ((core1,translation1),
      Vdwfw_variable.homogeneous_translation core1 (translation1))   with 
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
     let (Vdwfw_combination_t.C content_for_y) = combination_for_y in
     let (before,opt,after) = 
        Three_parts.select_center_element_and_reverse_left 
        (fun (core,translation)->core = x) content_for_y in 
     match opt with 
      None -> combination_for_y  
     |Some(_,translation) -> 
        let (Vdwfw_combination_t.C content_for_x) = combination_for_x in 
       let new_center = Image.image (
        fun (core1,translation1)->
              (core1,Ordered.merge Total_ordering.for_integers 
              translation translation1)
       ) content_for_x in 
       constructor(List.rev_append before (new_center@after)) ;;
   
   let to_string = Private.to_string ;;

   let union = Private.union ;; 



        