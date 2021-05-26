(*

#use"Van_der_Waerden/vdw_repeatedly_partitionable.ml";;

*)

module Private = struct 

let enhance rp (part_idx,criterion) =
    let old_parts = rp.Vdw_repeatedly_partitionable_t.parts 
    and old_main = rp.Vdw_repeatedly_partitionable_t.main 
    and old_history = rp.Vdw_repeatedly_partitionable_t.history in
    let part = List.assoc part_idx old_parts in 
    let (part1,part2) = Vdw_criterion.partition criterion part in
    let old_gains = rp.Vdw_repeatedly_partitionable_t.gains in 
    if part1 = []
    then ({rp with 
        Vdw_repeatedly_partitionable_t.gains = (part_idx,criterion,0,part_idx):: old_gains},None)  
    else 
    if part2 = []
    then ({rp with 
        Vdw_repeatedly_partitionable_t.gains = (part_idx,criterion,part_idx,0):: old_gains},None)  
    else   
    let n = List.length old_parts in 
    let summary = (part_idx,criterion,n+1,n+2) in 
    ({
      rp with
      Vdw_repeatedly_partitionable_t.parts = old_parts @ [(n+1,part1);(n+2,part2)];
       main = List.flatten (Image.image (fun idx->
              if idx=part_idx then [n+1;n+2] else [idx]
              ) old_main) ;
       history = (part_idx,criterion,n+1,n+2) :: old_history; 
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

let start ll =
   {
    Vdw_repeatedly_partitionable_t.parts = [1,ll];
     main = [1];
     history = []; 
     gains = [];
   } ;;

  