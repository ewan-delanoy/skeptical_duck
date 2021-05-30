(*

#use"Van_der_Waerden/vdw_repeatedly_partitionable.ml";;

*)


exception Remember_partition_exn of Vdw_part_t.t * Vdw_translated_criterion_t.t ;;
exception Cartesian_partition_exn of  (Vdw_part_t.t * Vdw_translated_criterion_t.t * Vdw_part_t.t * Vdw_part_t.t) list ;;

module Private = struct 

let add_if_needed uple gains =
   if List.mem uple gains 
   then gains 
   else uple :: gains ;; 

let remember_partition_opt rp part_idx criterion=
   let history_made_vague = Image.image (fun 
    (part_idx,criterion2,part_idx1,part_idx2)->
     (part_idx,criterion2,Some part_idx1,Some part_idx2)
   ) rp.Vdw_repeatedly_partitionable_t.history in 
   match Option.seek (fun (idx,criterion3,_,_)->
     (idx,criterion3)=(part_idx,criterion)
     ) 
    (history_made_vague @ 
     rp.Vdw_repeatedly_partitionable_t.gains) with 
   None -> None
  |Some(_,_,opt_idx1,opt_idx2) -> Some(opt_idx1,opt_idx2);;

let atomic_partition rp (part_idx,criterion) =
    if (remember_partition_opt rp part_idx criterion) <> None 
    then (rp,None)
    else  
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

let rec almost_atomic_partition walker items =
      match items with 
      [] ->  (walker,None)
      | item :: others -> 
          let (new_walker,opt_enhancement) = atomic_partition  walker item in 
          match opt_enhancement with 
          Some(enhancement) -> (new_walker,opt_enhancement)
          |None -> almost_atomic_partition new_walker others ;;

let rec partition_at_slow_cartesian_level 
   (changes_so_far,walker,old_indices,criteria) =
   let items = Cartesian.product old_indices criteria in 
   let (new_walker,opt_enhancement) = almost_atomic_partition walker items in 
   match opt_enhancement with 
   None -> (new_walker,changes_so_far)
   |Some enhancement ->
     let (_,_,idx1,idx2) = enhancement in 
     partition_at_slow_cartesian_level 
      (enhancement::changes_so_far,new_walker,idx1::idx2::old_indices,criteria) ;;

let partition_at_part_level rp (part_index,criteria) =
  partition_at_slow_cartesian_level ([],rp,[part_index],criteria) ;;

let rec molecular_partition (changes,walker,items) =
    match items with 
    [] ->  (walker,List.rev changes)
    | item :: others -> 
        let (new_walker,opt_enhancement) = atomic_partition  walker item in 
        let new_changes= (
        match opt_enhancement with 
        Some(enhancement) -> enhancement :: changes
        |None -> changes ) in 
        molecular_partition (new_changes,new_walker,others);;  

let rec iterator_for_cartesian_partition  (walker,constituents,criteria,accu) =
    match criteria with 
    [] -> (walker,constituents,accu)
    | criterion :: other_criteria ->
      let temp1 = Image.image (fun constituent->(constituent,criterion)) constituents in 
      let (new_walker,changes) = molecular_partition ([],walker,temp1) in 
      let new_constituents = List.flatten(Image.image (
         fun c -> match Option.seek (fun (part,_,_,_)->part = c) changes with 
         None -> [c]
         |Some(_,_,c1,c2) -> [c1;c2]
      ) constituents) in 
      iterator_for_cartesian_partition  (new_walker,new_constituents,other_criteria,accu@changes) ;;


let cartesian_partition walker constituents criteria =
    let (walker2,final_constituents,changes_made) = iterator_for_cartesian_partition  (walker,constituents,criteria,[])  in 
    let temp1 = Cartesian.product final_constituents criteria in
    let (walker3,no_changes) =
      molecular_partition ([],walker2,temp1) in 
    if no_changes <> []
    then raise(Cartesian_partition_exn no_changes)
    else (walker3,final_constituents,changes_made);;      

end ;;  

let cartesian_partition = Private.cartesian_partition ;;

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
    match Private.remember_partition_opt rp part_idx criterion with 
    None -> raise (Remember_partition_exn(part_idx,criterion))
   |Some(half1,half2) -> (half1,half2);;

let start ll =
   {
    Vdw_repeatedly_partitionable_t.parts = [Vdw_part_t.P(1),ll];
     history = []; 
     gains = [];
   } ;;

  