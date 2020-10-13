(* 

#use"Hex_analysis/hex_recorder_for_minimal_connecting_paths.ml";;

*)



module Mapper = struct 

let order_for_pochro_labels = 
   ((fun (Hex_polychrome_label_t.L(a)) (Hex_polychrome_label_t.L(b))->
        Total_ordering.standard a b
    ) : Hex_polychrome_label_t.t Total_ordering.t);;

let order_for_pochro_label_pairs = 
    Total_ordering.product order_for_pochro_labels order_for_pochro_labels;;

let order_for_mappings =
   Total_ordering.product order_for_pochro_label_pairs Total_ordering.standard;;

let rec helper_for_evaluation (pivot,already_seen,to_be_seen) =
    match to_be_seen with 
    [] -> (already_seen,None,[])
    | item :: other_items ->
        let (key,vaal) = item in 
        match  order_for_pochro_label_pairs pivot key with 
        Total_ordering.Lower ->   helper_for_evaluation (pivot,item::already_seen,other_items) 
        |Equal -> (already_seen,Some vaal,other_items)
        |Greater ->  (already_seen,None,to_be_seen) ;;

let evaluate (m:Hex_recorder_for_minimal_connecting_paths_t.mapper) key=
    let (_,opt,_) = helper_for_evaluation (key,[],m) in opt ;;

let add (m:Hex_recorder_for_minimal_connecting_paths_t.mapper) key vaal =
     let (before,opt,after) = helper_for_evaluation (key,[],m) in 
     match opt with 
     None -> List.rev_append before ((key,vaal)::after)
     |Some (_) -> m;;

let rec helper_for_selection (already_selected,to_be_compared,to_be_seen) =
      match to_be_seen with 
       [] -> List.rev already_selected 
       |pair :: other_pairs->
    (
      match to_be_compared with 
      [] -> List.rev_append already_selected to_be_seen 
      | (key,vaal) :: other_associations ->
        match  order_for_pochro_label_pairs pair key with 
        Total_ordering.Lower ->   helper_for_selection (pair::already_selected,to_be_compared,other_pairs) 
        |Equal -> helper_for_selection (already_selected,other_associations,other_pairs)
        |Greater ->  helper_for_selection (already_selected,other_associations,to_be_seen)
    );;

let select_nonregistered_pairs (m:Hex_recorder_for_minimal_connecting_paths_t.mapper) to_be_tested =
   helper_for_selection ([],m,Ordered.safe_set order_for_pochro_label_pairs to_be_tested);;

end ;;



module Private = struct 


let content recorder li =
   let (Hex_polychrome_label_t.L i)=li  in 
   if i > recorder.Hex_recorder_for_minimal_connecting_paths_t.number_of_old_labels 
   then List.assoc li recorder.Hex_recorder_for_minimal_connecting_paths_t.contents_for_new_labels
   else [li];;

let add_merger recorder triple=
  let (li,gc,lj)=triple in 
  let vi = content recorder li 
  and vj = content recorder lj in 
  let vij = Cartesian.product vi vj in 
  let old_mapper = recorder.Hex_recorder_for_minimal_connecting_paths_t.mapper in 
  let cleaned_vij = Mapper.select_nonregistered_pairs old_mapper vij in 
  let old_bridges = recorder.Hex_recorder_for_minimal_connecting_paths_t.bridges_for_new_labels  in 
  let new_mappings = Image.image (fun (xi,xj)->
     let bxi = List.assoc xi old_bridges
     and bxj = List.assoc xj old_bridges in 
     ((xi,xj),gc::(bxi@bxj))  
  ) cleaned_vij in 
  let new_mapper = Ordered.merge Mapper.order_for_mappings new_mappings old_mapper in 
  let old_contents = recorder.Hex_recorder_for_minimal_connecting_paths_t.contents_for_new_labels in 
  let (Hex_polychrome_label_t.L m,_)= List.hd(List.rev(old_contents)) in 
  let new_label = Hex_polychrome_label_t.L(m+1) in 
  let uij = Ordered.merge Mapper.order_for_pochro_labels vi vj in 
  let old_defs = recorder.Hex_recorder_for_minimal_connecting_paths_t.definitions_for_new_labels in 
  let bi = List.assoc li old_bridges 
  and bj = List.assoc lj old_bridges  in 
  {
     recorder with 
     Hex_recorder_for_minimal_connecting_paths_t.mapper = new_mapper ;
     bridges_for_new_labels = (new_label,gc::(bi@bj)) :: old_bridges ;
     contents_for_new_labels = (new_label,uij) :: old_contents  ;
     definitions_for_new_labels = (new_label,triple) :: old_defs ;
   } ;;



   
let empty_one n=
    {
    Hex_recorder_for_minimal_connecting_paths_t.number_of_old_labels = n;
    mapper = [] ; 
    bridges_for_new_labels = [];
    contents_for_new_labels = [];  
    definitions_for_new_labels = [];  
  };;    


end ;;    

let add_merger = Private.add_merger ;;
let empty_one = Private.empty_one ;;
