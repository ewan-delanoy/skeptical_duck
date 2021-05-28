(*

#use"Van_der_Waerden/vdw_fan.ml";;

*)

module Private = struct 

  let order_for_translations = 
      Total_ordering.silex_compare Total_ordering.for_integers ;;

  let apply_change 
    (Vdw_fan_t.F components) (old_idx,_,idx1,idx2) =
    let new_components = List.flatten (Image.image 
      (fun (idx,translations)->
         if idx = old_idx 
         then [(idx1,translations);(idx2,translations)]
        else  [(idx,translations)]
        )
    components) in 
    Vdw_fan_t.F new_components;;
  
  end ;;  
  
  let apply_changes changes fan = 
    List.fold_left  Private.apply_change  fan changes ;; 
  
  
  let expand rp (Vdw_fan_t.F components) =
    List.flatten( Image.image 
      (fun (idx,translations)->
         let temp1 = Image.image (fun translation ->
          Ordered_misc.translate_at_level_two 
          (Vdw_repeatedly_partitionable.expand rp idx) translation
         ) translations in 
         List.flatten temp1
        )
    components) ;; 
  
  let merge (Vdw_fan_t.F components1) (Vdw_fan_t.F components2)=
      let pr1 = Image.image fst components1 
      and unchecked_pr2 = Image.image fst components2 in 
      let pr2 = List.filter (
        fun x->not(List.mem x pr1)
      ) unchecked_pr2 in 
      let opt_to_list = (function None -> [] |Some ll-> ll) in
      let components = Image.image (
          fun idx ->
            let part1 = opt_to_list (List.assoc_opt idx components1)
            and part2 = opt_to_list (List.assoc_opt idx components2) in 
            (idx,Ordered.merge Private.order_for_translations part1 part2)
      ) (pr1@pr2) in 
      Vdw_fan_t.F components
    ;;   
  

  let prepare_partition (rp,fan) criterion=
     let (Vdw_fan_t.F components) = fan in 
     let temp1 = Image.image (fun (idx,translations)->
        let full_criteria = Image.image (
          fun translation -> Vdw_translated_criterion_t.C(criterion,translation)
        ) translations in
        (idx,full_criteria)) components in 
     let (new_rp,changes) = 
       Vdw_repeatedly_partitionable.partition rp temp1 in 
     (new_rp,changes);;  
      
  let remember_partition (rp,fan) criterion=
     let (Vdw_fan_t.F components) = fan in 
     let flattened_components = List.flatten(Image.image (
        fun (idx,translations) ->
          Image.image (fun translation ->(idx,translation)) translations
     ) components) in
     let both = Image.image (fun (idx,translation)->
      let (opt_part1,opt_part2) = 
      let full_criterion = Vdw_translated_criterion_t.C(criterion,translation) in 
      Vdw_repeatedly_partitionable.remember_partition rp idx full_criterion in
      (opt_part1,opt_part2,translation)
     ) flattened_components in 
    let flattened_memory1 = Option.filter_and_unpack (
      fun (opt_part1,opt_part2,translation)->
        match opt_part1 with 
        None -> None 
        |Some part1 -> Some(part1,translation)
    ) both 
    and flattened_memory2 = Option.filter_and_unpack (
      fun (opt_part1,opt_part2,translation)->
        match opt_part2 with 
        None -> None 
        |Some part2 -> Some(part2,translation)
    ) both in 
    let unflatten = (fun flattened_memory ->
      let temp = Listennou.partition_according_to_fst flattened_memory in 
       Image.image (fun (idx,translations)->
          (idx,Ordered.sort Private.order_for_translations translations)
        ) temp 
    ) in 
    let memory1 = unflatten flattened_memory1 
    and memory2 = unflatten flattened_memory2 in 
    (Vdw_fan_t.F  memory1,Vdw_fan_t.F  memory2) ;;
  
  let translate 
     (Vdw_fan_t.F components) translation =
     let new_components = Image.image 
       (fun (idx,translations)->
          (idx,
          Image.image (Ordered.merge Total_ordering.for_integers translation) translations)
         )
     components in 
     Vdw_fan_t.F new_components;;