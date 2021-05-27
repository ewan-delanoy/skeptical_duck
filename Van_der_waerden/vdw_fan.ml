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
  
  
  let prepare_partition (rp,fan) criterion=
     let (Vdw_fan_t.F components) = fan in 
     let temp1 = Image.image (fun (idx,l)->(idx,criterion)) components in 
     let (new_rp,changes) = 
       Vdw_repeatedly_partitionable.partition rp temp1 in 
     (new_rp,changes);;  
      
  let remember_partition (rp,fan) criterion=
     let (Vdw_fan_t.F components) = fan in 
     let both = Image.image (fun (idx,l)->
      let (part1,part2) = Vdw_repeatedly_partitionable.remember_partition rp idx criterion in
      (part1,part2,l)
     ) components in 
    let memory1 = Option.filter_and_unpack (
      fun (part1,part2,l)->
        if part1 < 1 then None else Some(part1,l)
    ) both 
    and memory2 = Option.filter_and_unpack (
      fun (part1,part2,l)->
        if part2 < 1 then None else Some(part2,l)
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