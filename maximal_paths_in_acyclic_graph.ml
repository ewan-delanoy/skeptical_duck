(*

The most used function in all those modules !

#use"maximal_paths_in_acyclic_graph.ml";;

*)

(*

type color = C of string ;;

module Edge = struct 

type t = E of  (int*int) * color ;; 
let cross_edge v (E((x,y),lbl)) =
    if v=x then Some(lbl,y) else 
    if v=y then Some(lbl,x) else 
    None ;;        
end ;;  

exception Cycle of int * ((color*int) list) * color;;

module Path = struct 

type t = P of int * ((color*int) list)  ;;

let extend_path (P(start,wedges)) (lbl,x) =
     let (before,opt,after) = 
        Three_parts.select_center_element_and_reverse_left (fun (c,y)->y=x) (List.rev wedges) in 
     match opt with 
     Some(pair)-> raise(Cycle(x,before,lbl)) 
      |None -> if x=start 
               then raise(Cycle(x,wedges,lbl))
               else P(start,wedges@[lbl,x]);;   

let end_vertex_in_path (P(start,wedges)) = match List.rev wedges with 
  [] -> start 
  |(_,x) :: _-> x ;;

end ;;  

(*
Path.extend_path
(Path.P (1,[(C"a",2);(C"b",3);(C"c",4);(C"d",5)])) (C"e",6);;

Path.extend_path
(Path.P (1,[(C"a",2);(C"b",3);(C"c",4);(C"d",5)])) (C"e",5);;

Path.extend_path
(Path.P (1,[(C"a",2);(C"b",3);(C"c",4);(C"d",5)])) (C"e",3);;

Path.extend_path
(Path.P (1,[(C"a",2);(C"b",3);(C"c",4);(C"d",5)])) (C"e",1);;
*)



module Node = struct 

type t =  Path.t * (Edge.t list);; 

let node_extensions (path,edges) =
    let temp1 = Three_parts.complemented_points edges in 
    let y = Path.end_vertex_in_path path in 
    Option.filter_and_unpack (
      fun (edge,other_edges) -> 
          match Edge.cross_edge y edge with 
          None -> None 
          |Some(lbl,z) -> Some(Path.extend_path path (lbl,z),other_edges)
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


let maximal_paths start edges =
  helper_for_maximal_paths ([],[(Path.P(start,[]),edges)]);;

end ;;  


Node.maximal_paths 1 
(Image.image (fun ((i,j),c)->Edge.E((i,j),C(c)) )
  [
    ((1,2),"a");((4,2),"b");((4,6),"c");((5,2),"d");((5,7),"e");
    ((3,1),"f");((3,8),"g");((9,8),"h");((8,10),"i");((11,10),"j");
  ]
);;

Node.maximal_paths 1 
(Image.image (fun ((i,j),c)->Edge.E((i,j),C(c)) )
  [
    ((1,2),"a");((4,2),"b");((4,6),"c");((5,2),"d");((5,7),"e");
    ((3,1),"f");((3,8),"g");((9,8),"h");((8,10),"i");((11,10),"j");
    ((9,2),"k")
  ]
);;

*)

