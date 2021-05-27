(*

#use"Van_der_Waerden/vdw_repeatedly_partitionable.ml";;

*)

exception Remember_partition_exn of Vdw_part_t.t * Vdw_criterion_t.t ;;

module Private = struct 

let add_if_needed uple gains =
   if List.mem uple gains 
   then gains 
   else uple :: gains ;; 

let enhance rp (part_idx,criterion) =
    let old_parts = rp.Vdw_repeatedly_partitionable_t.parts 
    and old_history = rp.Vdw_repeatedly_partitionable_t.history in
    let part = List.assoc part_idx old_parts in 
    let (part1,part2) = Vdw_criterion.partition criterion part in
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
      
let rec iterator_for_multiple_enhancer (already_treated,walker,to_be_treated) =
    match to_be_treated with 
    [] ->  (walker,List.rev already_treated)
    | item :: others -> 
        let (new_walker,opt_enhancement) = enhance  walker item in 
        let already_treated2 = (match  opt_enhancement with 
        None ->  already_treated
        |Some(enhancement) -> enhancement::already_treated
        ) in 
        iterator_for_multiple_enhancer (already_treated2,new_walker,others) ;;
end ;;  

let expand rp k = List.assoc k (rp.Vdw_repeatedly_partitionable_t.parts);;


let partition rp enhancements =
   Private.iterator_for_multiple_enhancer ([],rp,enhancements) ;;

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

  