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

let rec helper (pivot,already_seen,to_be_seen) =
    match to_be_seen with 
    [] -> (already_seen,None,[])
    | item :: other_items ->
        let (key,vaal) = item in 
        match  order_for_pochro_label_pairs pivot key with 
        Total_ordering.Lower ->   helper (pivot,item::already_seen,other_items) 
        |Equal -> (already_seen,Some vaal,other_items)
        |Greater ->  (already_seen,None,to_be_seen) ;;

let evaluate (m:Hex_recorder_for_minimal_connecting_paths_t.mapper) key=
    let (_,opt,_) = helper (key,[],m) in opt ;;

let add (m:Hex_recorder_for_minimal_connecting_paths_t.mapper) key vaal =
     let (before,opt,after) = helper (key,[],m) in 
     match opt with 
     None -> List.rev_append before ((key,vaal)::after)
     |Some (_) -> m;;

end ;;



module Private = struct 

let size recorder =
  let l = recorder.Hex_recorder_for_minimal_connecting_paths_t.paths in 
  let ((_,Hex_polychrome_label_t.L(m)),_) = List.hd (List.rev l) in 
  m;;

let neighbors_below recorder lx =
  Option.filter_and_unpack (
    fun ((a,b),opt_link) -> 
      match opt_link with 
      None -> None 
      |Some(link) -> 
        if b=lx then Some(a,link) else 
        None
  ) recorder.Hex_recorder_for_minimal_connecting_paths_t.paths;;

let neighbors_above recorder lx =
  Option.filter_and_unpack (
    fun ((a,b),opt_link) -> 
      match opt_link with 
      None -> None 
      |Some(link) -> 
        if a=lx then Some(b,link) else 
        None
  ) recorder.Hex_recorder_for_minimal_connecting_paths_t.paths;;

let all_neighbors recorder lx =
  (neighbors_below recorder lx)@[lx,[]]@(neighbors_above recorder lx);;


let minmax (Hex_polychrome_label_t.L i1) (Hex_polychrome_label_t.L i2)=
  (Hex_polychrome_label_t.L(min(i1)(i2)),Hex_polychrome_label_t.L(max(i1)(i2)));;   

(**** New code starts here ****)



let add_merger recorder (li,gc,lj)=
  let (Hex_polychrome_label_t.L i)=li 
  and (Hex_polychrome_label_t.L j)=lj   in 
  


  and paths=recorder.Hex_recorder_for_minimal_connecting_paths_t.paths in 
  let m = size recorder in 
  let new_label = Hex_polychrome_label_t.L(m+1) in 
  let vi = all_neighbors recorder li 
  and vj = all_neighbors recorder lj in 
  let vij = Image.image (fun ((lab1,path1),(lab2,path2))->
       (minmax lab1 lab2,path1@path2)
  ) (Cartesian.product vi vj) in 
  let paths1 = Image.image (
    fun pair ->
     let (key,opt_link) = pair in 
     if opt_link<>None then pair else 
     match Option.seek (fun (key2,vaal)->key2=key) vij with 
     None -> pair 
     |Some(_,link) -> (key,Some link)
  ) paths in 
  let new_records = Ennig.doyle (
     fun k-> 
       let lk = (Hex_polychrome_label_t.L k) in 
       let answer = (
       if (k=i)||(k=j) then Some [gc] else 
       match List.assoc (minmax li lk) paths1 with 
        Some(path) -> Some(path)
        |None -> List.assoc (minmax lj lk) paths1 
       ) in 
       ((lk,new_label),answer)
  ) 1 m in 
  {
    Hex_recorder_for_minimal_connecting_paths_t.paths = paths1 @ new_records ; 
    connections = gc :: (recorder.Hex_recorder_for_minimal_connecting_paths_t.connections);  
  };; 

   
let empty_one =
    {
    Hex_recorder_for_minimal_connecting_paths_t.mapper = [] ; 
    contents_for_new_labels = [];  
    definitions_for_new_labels = [];  
  };;    


end ;;    

let add_merger = Private.add_merger ;;
let empty_one = Private.empty_one ;;
