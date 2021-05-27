(*

#use"Van_der_Waerden/vdw_fan.ml";;

*)

module Private = struct 

  let apply_change 
    (Vdw_fan_t.F components) (old_idx,_,idx1,idx2) =
    let new_components = List.flatten (Image.image 
      (fun (idx,translation)->
         if idx = old_idx 
         then [(idx1,translation);(idx2,translation)]
        else  [(idx,translation)]
        )
    components) in 
    Vdw_fan_t.F new_components;;
  
  end ;;  
  
  let apply_changes changes fan = 
    List.fold_left  Private.apply_change  fan changes ;; 
  
  
  let expand rp (Vdw_fan_t.F components) =
    List.flatten( Image.image 
      (fun (idx,translation)->
         Ordered_misc.translate_at_level_two 
          (Vdw_repeatedly_partitionable.expand rp idx) translation
        )
    components) ;; 
  
  let merge (Vdw_fan_t.F components1) (Vdw_fan_t.F unchecked_components2)=
      let components2 = List.filter (
        fun x->not(List.mem x components1)
      ) unchecked_components2 in 
      Vdw_fan_t.F (components1@components2)
    ;;   
  
  let prepare_partition (rp,fan) criterion=
     let (Vdw_fan_t.F components) = fan in 
     let temp1 = Image.image (fun (idx,l)->(idx,criterion)) components in 
     let (new_rp,changes) = 
       Vdw_repeatedly_partitionable.partition rp temp1 in 
     (new_rp,changes);;  
      
  let remember_partition (rp,fan) criterion=
     let (Vdw_fan_t.F components) = fan in 
     let both = Image.image (fun (idx,l)->
      let (opt_part1,opt_part2) = 
      Vdw_repeatedly_partitionable.remember_partition rp idx criterion in
      (opt_part1,opt_part2,l)
     ) components in 
    let memory1 = Option.filter_and_unpack (
      fun (opt_part1,opt_part2,l)->
        match opt_part1 with 
        None -> None 
        |Some part1 -> Some(part1,l)
    ) both 
    and memory2 = Option.filter_and_unpack (
      fun (opt_part1,opt_part2,l)->
        match opt_part2 with 
        None -> None 
        |Some part2 -> Some(part2,l)
    ) both in 
    (Vdw_fan_t.F  memory1,Vdw_fan_t.F  memory2) ;;
  
  let translate 
     (Vdw_fan_t.F components) translation =
     let new_components = Image.image 
       (fun (idx,translation2)->
          (idx,Ordered.merge Total_ordering.for_integers translation translation2)
         )
     components in 
     Vdw_fan_t.F new_components;;