(*

The most used function in all those modules !

#use"maximal_paths_in_acyclic_graph.ml";;

*)



module Private = struct 

let cross_edge v ((x,y),lbl) =
    if v=x then Some(lbl,y) else 
    if v=y then Some(lbl,x) else 
    None ;;        


exception Cycle of int list;;



let extend_path (start,wedges) (lbl,x) =
     let (before,opt,after) = 
        Three_parts.select_center_element_and_reverse_left (fun (c,y)->y=x) (List.rev wedges) in 
     match opt with 
     Some(pair)-> raise(Cycle(x::(Image.image snd before))) 
      |None -> if x=start 
               then raise(Cycle(x::(Image.image snd wedges)))
               else (start,wedges@[lbl,x]);;   

let final_vertex_in_path (start,wedges) = match List.rev wedges with 
  [] -> start 
  |(_,x) :: _-> x ;;



(*
extend_path
 (1,[("a",2);("b",3);("",4);("d",5)]) ("e",6);;

extend_path
(1,[("a",2);("b",3);("",4);("d",5)]) ("e",5);;

extend_path
 (1,[("a",2);("b",3);("",4);("d",5)]) ("e",3);;

extend_path
(1,[("a",2);("b",3);("",4);("d",5)]) ("e",1);;
*)





let node_extensions (path,edges) =
    let temp1 = Three_parts.complemented_points edges in 
    let y = final_vertex_in_path path in 
    Option.filter_and_unpack (
      fun (edge,other_edges) -> 
          match cross_edge y edge with 
          None -> None 
          |Some(lbl,z) -> Some(extend_path path (lbl,z),other_edges)
    ) temp1 ;;

let rec helper_for_maximal_paths (complete,incomplete) =
    match incomplete with 
    [] ->List.rev complete 
    |node :: other_nodes ->
        let temp1 = node_extensions node in 
        if temp1=[]
        then let (path,_) = node in 
             helper_for_maximal_paths (path::complete,other_nodes)
        else helper_for_maximal_paths (complete,temp1@other_nodes) ;; 

end ;;

let maximal_paths edges start =
  Private.helper_for_maximal_paths ([],[((start,[]),edges)]);;


(*

maximal_paths 
(
  [
    ((1,2),"a");((4,2),"b");((4,6),"c");((5,2),"d");((5,7),"e");
    ((3,1),"f");((3,8),"g");((9,8),"h");((8,10),"i");((11,10),"j");
  ]
) 1;;

maximal_paths 
(
  [
    ((1,2),"a");((4,2),"b");((4,6),"c");((5,2),"d");((5,7),"e");
    ((3,1),"f");((3,8),"g");((9,8),"h");((8,10),"i");((11,10),"j");
    ((9,2),"k")
  ]
) 1;;

*)

