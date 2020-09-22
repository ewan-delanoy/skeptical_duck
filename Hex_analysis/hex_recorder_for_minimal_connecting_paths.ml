(* 

#use"Hex_analysis/hex_recorder_for_minimal_connecting_paths.ml";;

*)

module Private = struct 

let size (Hex_recorder_for_minimal_connecting_paths_t.R(l)) =
  let ((_,Hex_polychrome_label_t.L(m)),_) = List.hd (List.rev l) in 
  m;;

let neighbors_below (Hex_recorder_for_minimal_connecting_paths_t.R(l)) lx =
  Option.filter_and_unpack (
    fun ((a,b),opt_link) -> 
      match opt_link with 
      None -> None 
      |Some(link) -> 
        if b=lx then Some(a,link) else 
        None
  ) l;;

let neighbors_above (Hex_recorder_for_minimal_connecting_paths_t.R(l)) lx =
  Option.filter_and_unpack (
    fun ((a,b),opt_link) -> 
      match opt_link with 
      None -> None 
      |Some(link) -> 
        if a=lx then Some(b,link) else 
        None
  ) l;;

let all_neighbors recorder lx =
  (neighbors_below recorder lx)@[lx,[]]@(neighbors_above recorder lx);;


let minmax (Hex_polychrome_label_t.L i1) (Hex_polychrome_label_t.L i2)=
  (Hex_polychrome_label_t.L(min(i1)(i2)),Hex_polychrome_label_t.L(max(i1)(i2)));;   

let add_merger recorder (li,gc,lj)=
  let (Hex_polychrome_label_t.L i)=li 
  and (Hex_polychrome_label_t.L j)=lj   
  and (Hex_recorder_for_minimal_connecting_paths_t.R(l)) = recorder in 
  let m = size recorder in 
  let new_label = Hex_polychrome_label_t.L(m+1) in 
  let vi = all_neighbors recorder li 
  and vj = all_neighbors recorder lj in 
  let vij = Image.image (fun ((lab1,path1),(lab2,path2))->
       (minmax lab1 lab2,path1@path2)
  ) (Cartesian.product vi vj) in 
  let l1 = Image.image (
    fun pair ->
     let (key,opt_link) = pair in 
     if opt_link<>None then pair else 
     match Option.seek (fun (key2,vaal)->key2=key) vij with 
     None -> pair 
     |Some(_,link) -> (key,Some link)
  ) l in 
  let new_records = Ennig.doyle (
     fun k-> 
       let lk = (Hex_polychrome_label_t.L k) in 
       let answer = (
       if (k=i)||(k=j) then Some [gc] else 
       match List.assoc (minmax li lk) l1 with 
        Some(path) -> Some(path)
        |None -> List.assoc (minmax lj lk) l1 
       ) in 
       ((lk,new_label),answer)
  ) 1 m in 
  Hex_recorder_for_minimal_connecting_paths_t.R(l1 @ new_records);;

   
let empty_one (Hex_ctct_report_t.R l_ctct_report)=
   let n = List.length l_ctct_report in 
   let temp1 = Ennig.doyle (
         fun j ->
            let lj =  Hex_polychrome_label_t.L j in 
              Ennig.doyle (
                  fun i -> 
                  let li =  Hex_polychrome_label_t.L i in 
                    ((li,lj),None)
              ) 1 (j-1)
      ) 2 n  in 
   Hex_recorder_for_minimal_connecting_paths_t.R(List.flatten temp1);;

end ;;    

let add_merger = Private.add_merger ;;
let empty_one = Private.empty_one ;;
