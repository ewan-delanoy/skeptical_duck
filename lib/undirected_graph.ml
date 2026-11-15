(*

#use"lib/undirected_graph.ml";;

*)

(*
module Private =struct 

module type INTERFACE = sig 

    type 'a t 

    val all_vertices : 'a t -> 'a list
    val connected_components : 'a t -> 'a list list
    val edges : 'a t -> ('a * 'a) list
    val extract_cycle : 'a t -> 'a list -> 'a list
    val make : 'a list -> ('a * 'a) list -> 'a t
    val make_from_indexation_and_neighbors :
      (int * 'a) list -> (int * int list) list -> (int * 'a) t
    val merge_group_of_vertices : 'a t -> 'a list -> 'a -> 'a t  
    val neighbors : 'a t -> 'a -> 'a list
    val rename_vertices : 'a t -> ('a -> 'b) -> 'b t
    val remove_edges : 'a t -> ('a * 'a) list -> 'a t
    val remove_vertices : 'a t -> 'a list -> 'a t  
    val restrict : 'a t -> 'a list -> 'a t
    val set_level_neighborhood : 'a t -> 'a list -> 'a list
    val split_vertex : 'a t -> original:'a -> 'a -> 'a -> 'a t
    val vertices_with_degree_at_least_three :
      'a t -> 'a list -> ('a * 'a list) list


end ;;  

module Unconstrained = struct 

type ug_index = UGI of int ;;

type 'a t = G of ug_index * ('a list) * (('a * 'a) list) * ((int * 'a) list);; 

 


module Inner = struct 

type inner_t = IG of ug_index * (int list) * ((int * int) list) ;;

let repository = ref ([]: inner_t list) ;;

let i_order = Total_ordering.for_integers ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_insert = Ordered.insert i_order ;;
let i_intersect = Ordered.intersect i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let memoize0 hashtbl f ug =
  let (IG(UGI(idx),_,_)) = ug in 
  match Hashtbl.find_opt hashtbl idx with 
  Some(old_answer)->old_answer 
  |None ->
    let answer = f ug in 
    let _ = Hashtbl.add hashtbl idx answer in 
    answer ;;

let memoize1 hashtbl f ug x=
  let (IG(UGI(idx),_,_)) = ug in 
  match Hashtbl.find_opt hashtbl (idx,x) with 
  Some(old_answer)->old_answer 
  |None ->
    let answer = f ug x in 
    let _ = Hashtbl.add hashtbl (idx,x) answer in 
    answer ;;
      

let hashtbl_for_neighbors = (Hashtbl.create 100:(int*int, int list) Hashtbl.t) ;;

let unmemoized_neighbors (IG(_,_vertices,edges)) v =
   let temp = List.filter_map (fun (x,y)->
    if x=v then Some y else 
    if y=v then Some x else None   
    ) edges in 
    i_sort temp ;;

let neighbors = memoize1 hashtbl_for_neighbors unmemoized_neighbors ;; 

let neighbors_for_several iug vertices = i_fold_merge (Image.image (neighbors iug) vertices) ;;

let rec helper_for_connected_component iug (treated,to_be_treated) =
   if to_be_treated = []
   then treated 
   else let potential_novelties = neighbors_for_several iug to_be_treated in
        let effective_novelties = i_setminus potential_novelties treated in 
        helper_for_connected_component iug (i_merge treated to_be_treated,effective_novelties) ;;

let connected_component_at_point iug vertex = helper_for_connected_component iug ([],[vertex]) ;; 

let rec helper_for_connected_components iug (treated,to_be_treated) =
  match to_be_treated with 
  [] -> List.rev treated 
  |v::others ->
    let component = connected_component_at_point iug v in
    helper_for_connected_components iug (component::treated,i_setminus others component) ;;
        
let hashtbl_for_connected_components = (Hashtbl.create 100:(int, int list list) Hashtbl.t) ;;

let unmemoized_connected_components iug =
  let (IG(_idx,vertices,_edges))= iug in 
  helper_for_connected_components iug ([],vertices) ;;  

let connected_components = memoize0 hashtbl_for_connected_components unmemoized_connected_components ;; 

let touches iug v1 v2 = i_mem v2 (neighbors iug v1);;

let hashtbl_for_induced_neighborhoods = (Hashtbl.create 100:(int*(int list), (int*(int list)) list) Hashtbl.t) ;;

let unmemoized_induced_neighborhoods iug vertices =
   let sorted_vertices = i_sort vertices in 
   Image.image (fun x->(x,i_intersect (neighbors iug x) sorted_vertices)) vertices ;;


let induced_neighborhoods = memoize1 hashtbl_for_induced_neighborhoods unmemoized_induced_neighborhoods ;; 

let vertices_with_degree_at_least_three iug vertices =
   List.filter (fun (_x,nx)->List.length(nx)>2) (induced_neighborhoods iug vertices) ;; 

let degree_one_vertices iug vertices =
   List.filter (fun (_x,nx)->List.length(nx)=1) (induced_neighborhoods iug vertices) ;; 

let rec helper_for_cycle_extraction indneigh (treated,ordered_treated,cursor) =
   let temp = i_setminus (List.assoc cursor indneigh) ordered_treated in 
   if List.length(temp)<>1
   then List.rev (cursor::treated)
   else let new_cursor = List.hd temp in 
        helper_for_cycle_extraction indneigh (cursor::treated,i_insert cursor ordered_treated,new_cursor) ;; 

let extract_cycle iug vertices = 
  let indneigh = induced_neighborhoods iug vertices in 
  let (starter,_)=List.find (fun (_x,nx)->List.length(nx)=1) indneigh in 
  helper_for_cycle_extraction indneigh ([],[],starter) ;;   

let register (vertices,edges) =
   
   let idx = List.length(!repository)+1 in 
   let new_inner_graph = IG(UGI(idx),i_sort vertices,edges) in 
   let _ = (repository:=(!repository)@[new_inner_graph]) in 
   idx ;;

let image (G(UGI(idx),_,_,_)) = List.nth (!repository) (idx-1) ;;   

let set_level_neighborhood iug domain = i_setminus (neighbors_for_several iug domain) domain ;;

end ;;  

let make vertices edges = 
  let m = List.length vertices in 
  let get_idx = (fun x->List_again.index_of_in x vertices) in 
  let renamed_edges = Image.image (fun (a,b)->(get_idx a,get_idx b)) edges in 
  let new_idx = Inner.register (Int_range.range 1 m,renamed_edges) in 
  G(UGI(new_idx),vertices,edges,Int_range.index_everything vertices) ;; 
  
let make_from_indexation_and_neighbors indexed_vertices i_neighbors = 
  let i_vertices = Image.image fst indexed_vertices 
  and i_edges = List.flatten(Image.image(fun (x,nx)->Image.image(fun y->(x,y)) nx) i_neighbors) in  
  let new_idx = Inner.register (i_vertices,i_edges) in 
  let indexed_form = (fun idx->(idx,List.assoc idx indexed_vertices)) in 
  let reconstructed_edges = Image.image (fun (a,b)->(indexed_form a,indexed_form b)) i_edges in 
  G(UGI(new_idx),indexed_vertices,reconstructed_edges,Image.image (fun (idx,v)->(idx,(idx,v))) indexed_vertices) ;;   

let vertex_of_inner_index (G(_,_,_,associator)) k = List.assoc k associator ;;

let vertex_to_inner_index (G(_,_,_,associator)) v = Option.get(List_again.assoc_right_opt v associator) ;;

let connected_components ug = 
   let iug = Inner.image ug in 
   let i_connected_components = Inner.connected_components iug in 
   Image.image (Image.image (vertex_of_inner_index ug)) i_connected_components;;

let vertices_with_degree_at_least_three ug domain= 
   let iug = Inner.image ug in 
   let i_domain = Image.image (vertex_to_inner_index ug) domain in 
   let i_solutions = Inner.vertices_with_degree_at_least_three iug i_domain in 
   let g = vertex_of_inner_index ug  in  
   (Image.image (fun (k,nk)->(g k,Image.image g nk))) i_solutions;;

let extract_cycle ug domain= 
   let iug = Inner.image ug in 
   let i_domain = Image.image (vertex_to_inner_index ug) domain in 
   let i_solutions = Inner.extract_cycle iug i_domain in 
   let g = vertex_of_inner_index ug  in  
   Image.image g i_solutions;;

let restrict old_ug domain= 
   let (G(_,old_vertices,old_edges,_)) = old_ug in
   let tester = (fun x->List.mem x domain) in 
   let new_vertices = List.filter tester old_vertices
   and new_edges =  List.filter (fun (x,y)->(tester x)&&(tester y)) old_edges in 
   make new_vertices new_edges;;

let remove_vertices old_ug domain= 
   let (G(_,old_vertices,old_edges,_)) = old_ug in
   let tester = (fun x->not(List.mem x domain)) in 
   let new_vertices = List.filter tester old_vertices
   and new_edges =  List.filter (fun (x,y)->(tester x)&&(tester y)) old_edges in 
   make new_vertices new_edges;;   

let split_vertex old_ug ~original:x x1 x2= 
   let (G(_,old_vertices,old_edges,_)) = old_ug in
   let new_vertices = List.flatten(Image.image (fun t->if t=x then [x1;x2] else [t]) old_vertices)
   and new_edges =  (x1,x2)::(List.flatten(Image.image (fun (a,b)->
     if a=x then [x1,b;x2,b] else 
     if b=x then [a,x1;a,x2] else 
     [a,b]) old_edges)) in 
   make new_vertices new_edges;;   

let remove_edges old_ug edges_to_be_removed= 
   let (G(_,vertices,old_edges,_)) = old_ug in
   let new_edges =  List.filter (fun (x,y)->
      not((List.mem (x,y) edges_to_be_removed)||(List.mem (y,x) edges_to_be_removed))
    ) old_edges in 
   make vertices new_edges;;      

let rename_vertices old_ug f= 
   let (G(_,old_vertices,old_edges,_)) = old_ug in
   let new_vertices = Image.image f old_vertices
   and new_edges =  Image.image (fun (x,y)->(f x,f y)) old_edges in 
   make new_vertices new_edges;;      

let set_level_neighborhood ug domain= 
   let iug = Inner.image ug  in 
   let i_domain = Image.image (vertex_to_inner_index ug) domain in 
   let i_solutions = Inner.set_level_neighborhood iug i_domain in 
   let g = vertex_of_inner_index ug  in 
   Image.image g i_solutions;;


let all_vertices (G(_,vs,_,_)) = vs ;;

let merge_group_of_vertices ug group representative= 
   let (G(_,old_vertices,old_edges,_)) = ug in 
   let transform = (fun x-> if List.mem x group then representative else x) in 
   let new_vertices = List.filter (
     fun x->if List.mem x group then x=representative else true
   ) old_vertices 
   and new_edges = List.filter_map (
     fun (a,b)->
       let new_a=transform a and new_b=transform b in 
       if new_a=new_b then None else Some(new_a,new_b)
   ) old_edges in 
  make new_vertices new_edges;;


let neighbors ug v= 
   let iug = Inner.image ug in 
   let v_idx = vertex_to_inner_index ug v in
   let i_neighbors = Inner.neighbors iug v_idx in 
   let g = vertex_of_inner_index ug  in 
   Image.image g i_neighbors ;;
   
let edges (G(_,_,old_edges,_))= old_edges ;; 

end ;;   

module Constrained = (Unconstrained:INTERFACE) ;;

end ;;

include Private.Constrained ;;

*)

let make_from_indexation_and_neighbors _x _y= () ;;