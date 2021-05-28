(*

#use"Van_der_Waerden/vdw_repeatedly_partitionable.ml";;

*)

exception Remember_partition_exn of Vdw_part_t.t * Vdw_translated_criterion_t.t ;;


module Private = struct 

let add_if_needed uple gains =
   if List.mem uple gains 
   then gains 
   else uple :: gains ;; 



let atomic_partition rp (part_idx,criterion) =
    let old_parts = rp.Vdw_repeatedly_partitionable_t.parts 
    and old_history = rp.Vdw_repeatedly_partitionable_t.history in
    let part = List.assoc part_idx old_parts in 
    let (part1,part2) = Vdw_translated_criterion.partition criterion part in
    let old_gains = rp.Vdw_repeatedly_partitionable_t.gains in 
    if part1 = []
    then ({rp with 
        Vdw_repeatedly_partitionable_t.gains = 
          add_if_needed (part_idx,criterion,None,Some(part_idx))  old_gains},None)  
    else 
    if part2 = []
    then ({rp with 
        Vdw_repeatedly_partitionable_t.gains = 
          add_if_needed (part_idx,criterion,Some(part_idx),None) old_gains},None)  
    else   
    let n = List.length old_parts in 
    let new_part_index1 = Vdw_part_t.P (n+1) 
    and new_part_index2 = Vdw_part_t.P (n+2) in  
    let summary = (part_idx,criterion,new_part_index1,new_part_index2) in 
    ({
      Vdw_repeatedly_partitionable_t.parts = old_parts @ 
         [(new_part_index1,part1);(new_part_index2,part2)];
       history = summary :: old_history; 
       gains = 
        (new_part_index2,criterion,None,Some new_part_index2) :: 
        (new_part_index1,criterion,Some new_part_index1,None) ::old_gains;
     },Some(summary)) ;;

let rec partition_at_criterion_level 
  (changes_so_far,rp,treated_parts,parts_to_be_treated,criterion) =
  match parts_to_be_treated with 
  [] -> (changes_so_far,rp,List.rev treated_parts)
  |part_idx :: other_parts ->
     let (new_rp,opt_enhancement) = atomic_partition rp (part_idx,criterion) in 
     let (more_changes,treated,to_be_treated) = (match opt_enhancement with
          None -> (changes_so_far,part_idx::treated_parts,other_parts) 
         |Some change -> 
            let (_,_,new_part1,new_part2) = change in 
            (change :: changes_so_far,new_part2::new_part1::treated_parts,other_parts) 
     ) in 
     partition_at_criterion_level (more_changes,new_rp,treated,to_be_treated,criterion) ;;    

let rec partition_at_cartesian_level (changes_so_far,rp,part_indices,criteria) =   
   match criteria with 
   [] -> (rp,changes_so_far)
   | criterion ::other_criteria ->
      let (more_changes_so_far,new_rp,new_part_indices) =
        partition_at_criterion_level (changes_so_far,rp,[],part_indices,criterion) in
     partition_at_cartesian_level (more_changes_so_far,new_rp,new_part_indices,other_criteria);;  

let partition_at_part_level rp (part_index,criteria) =
  partition_at_cartesian_level ([],rp,[part_index],criteria) ;;


end ;;  

let expand rp k = List.assoc k (rp.Vdw_repeatedly_partitionable_t.parts);;


let partition rp requirements = 
   let rec iterator =(
    fun (already_treated,walker,to_be_treated) ->
    match to_be_treated with 
    [] ->  (walker,List.rev already_treated)
    | item :: others -> 
        let (new_walker,new_changes) = Private.partition_at_part_level  walker item in 
        let already_treated2 = List.rev_append new_changes already_treated in 
        iterator (already_treated2,new_walker,others)
   ) in 
   iterator ([],rp,requirements) ;;

let remember_partition rp part_idx criterion=
    let history_made_vague = Image.image (fun 
     (part_idx,criterion2,part_idx1,part_idx2)->
      (part_idx,criterion2,Some part_idx1,Some part_idx2)
    ) rp.Vdw_repeatedly_partitionable_t.history in 
    match Option.seek (fun (idx,criterion3,_,_)->
      (idx,criterion3)=(part_idx,criterion)
      ) 
     (history_made_vague @ 
      rp.Vdw_repeatedly_partitionable_t.gains) with 
    None -> raise (Remember_partition_exn(part_idx,criterion))
   |Some(_,_,opt_idx1,opt_idx2) -> (opt_idx1,opt_idx2);;

let start ll =
   {
    Vdw_repeatedly_partitionable_t.parts = [Vdw_part_t.P(1),ll];
     history = []; 
     gains = [];
   } ;;

  