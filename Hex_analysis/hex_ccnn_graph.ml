(* 

#use"Hex_analysis/hex_ccnn_graph.ml";;

*) 

let neighbors (Hex_ccnn_report_t.R l) i =
   Option.filter_and_unpack (
      fun ((j,k),data) ->
        if j=i then Some(k,data) else 
        if k=i then Some(j,data) else None
   ) l ;;
  
let initializer_for_connected_region_computation ccnn origin =
   let germ = [origin,Image.image (fun (k,data)->(k,[[data]])) (neighbors ccnn origin)] in 
  (Set_of_integers.singleton origin,
   germ,
   germ
  );;

let pusher_for_connected_region_computation ccnn
   (all_indices_so_far,all_paths_so_far,recent_paths) =
  let temp1 =List.flatten(Image.image (
    fun (i,paths_for_i)->
     let ttemp2= neighbors ccnn i in 
     Image.image (
         fun (j,data) -> (j,
         Image.image (fun l->data::l) paths_for_i
         )
     ) ttemp2
  )recent_paths) in 
  let temp2 = List.filter (
    fun (j,paths_for_j) -> 
      not(Set_of_integers.mem j all_indices_so_far)
  )  temp1  in 
  let new_indices = Set_of_integers.safe_set(Image.image fst temp2) in 
  let new_paths = Set_of_integers.image (
     fun j->(j,List.flatten(Option.filter_and_unpack (
         fun (k,paths_for_k) ->  
           if k = j 
           then Some(paths_for_k)
           else None
     ) temp2))
  ) new_indices in 
  let new_path_catalogue = all_paths_so_far @ new_paths in 
  let new_index_catalogue = Set_of_integers.merge all_indices_so_far new_indices in 
  (new_index_catalogue,new_path_catalogue,new_paths);; 