(*

#use"lib/undirected_graph.ml";;

*)


module Private =struct 

module type INTERFACE = sig 

    type 'a t 

    val all_vertices : 'a t -> 'a list
    val connected_components : 'a t -> 'a list list
    val decompose_into_cycles : 'a t -> 'a list -> 'a list list
    val edges : 'a t -> ('a * 'a) list
    val extract_cycle_from_starting_at : 'a t -> 'a list -> 'a -> 'a list
    val local_connected_components : 'a t -> 'a list -> 'a list list
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

type inner_t = IG of ug_index * (Set_of_integers_t.t) * ((int * int) list) ;;

let repository = ref ([]: inner_t list) ;;

let i_order = Total_ordering.for_integers ;;

let i_fold_merge = Set_of_integers.fold_merge ;;
let i_insert = Set_of_integers.insert ;;
let i_intersect = Set_of_integers.intersect ;;
let i_mem = Set_of_integers.mem ;;
let i_merge = Set_of_integers.merge ;;
let i_setminus = Set_of_integers.setminus ;;
let i_sort = Set_of_integers.sort ;;


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
      

let hashtbl_for_neighbors = (Hashtbl.create 100:(int*int, Set_of_integers_t.t) Hashtbl.t) ;;

let unmemoized_neighbors (IG(_,_vertices,edges)) v =
   let temp = List.filter_map (fun (x,y)->
    if x=v then Some y else 
    if y=v then Some x else None   
    ) edges in 
    i_sort temp ;;

let neighbors = memoize1 hashtbl_for_neighbors unmemoized_neighbors ;; 

let neighbors_for_several iug vertices = i_fold_merge (Image.image (neighbors iug) vertices) ;;

let rec generic_helper_for_connected_component neighbors_map (treated,to_be_treated) =
   let l_to_be_treated = Set_of_integers.forget_order to_be_treated in 
   if l_to_be_treated = []
   then treated 
   else let potential_novelties = i_fold_merge (Image.image neighbors_map l_to_be_treated) in
        let effective_novelties = i_setminus potential_novelties treated in 
        generic_helper_for_connected_component neighbors_map (i_merge treated to_be_treated,effective_novelties) ;;

let generic_connected_component_at_point neighbors_map vertex = generic_helper_for_connected_component neighbors_map 
 (Set_of_integers.empty_set,Set_of_integers.singleton vertex) ;;

let rec generic_helper_for_connected_components neighbors_map (treated,to_be_treated) =
  match to_be_treated with 
  [] -> List.rev treated 
  |v::others ->
    let component = generic_connected_component_at_point neighbors_map v in
    generic_helper_for_connected_components neighbors_map (component::treated,
      List.filter(fun v->not(Set_of_integers.mem v component)) others ) ;;


let hashtbl_for_connected_components = (Hashtbl.create 100:(int, Set_of_integers_t.t list) Hashtbl.t) ;;

let unmemoized_connected_components iug =
  let (IG(_idx,vertices,_edges))= iug in  
  generic_helper_for_connected_components (neighbors iug) ([],Set_of_integers.forget_order vertices) ;;  

let connected_components = memoize0 hashtbl_for_connected_components unmemoized_connected_components ;; 

let local_connected_components iug domain_in_imposed_order = 
   let domain = Set_of_integers.sort domain_in_imposed_order in 
   let local_neighbors = (fun v-> i_intersect (neighbors iug v) domain) in 
   generic_helper_for_connected_components local_neighbors ([],domain_in_imposed_order) ;;   

let touches iug v1 v2 = i_mem v2 (neighbors iug v1);;

let hashtbl_for_induced_neighborhoods = (Hashtbl.create 100:(int*(int list), (int*Set_of_integers_t.t) list) Hashtbl.t) ;;

let unmemoized_induced_neighborhoods iug vertices =
   let sorted_vertices = i_sort vertices in 
   Image.image (fun x->(x,i_intersect (neighbors iug x) sorted_vertices)) vertices ;;


let induced_neighborhoods = memoize1 hashtbl_for_induced_neighborhoods unmemoized_induced_neighborhoods ;; 

let vertices_with_degree_at_least_three iug vertices =
   List.filter (fun (_x,nx)->Set_of_integers.length(nx)>2) (induced_neighborhoods iug vertices) ;; 

let degree_one_vertices iug vertices =
   List.filter (fun (_x,nx)->Set_of_integers.length(nx)=1) (induced_neighborhoods iug vertices) ;; 

let rec helper_for_cycle_extraction indneigh (treated,ordered_treated,cursor) =
   let temp = i_setminus (List.assoc cursor indneigh) ordered_treated in 
   if Set_of_integers.length(temp)<>1
   then List.rev (cursor::treated)
   else let new_cursor = List.hd (Set_of_integers.forget_order temp) in 
        helper_for_cycle_extraction indneigh (cursor::treated,i_insert cursor ordered_treated,new_cursor) ;; 

let extract_cycle_from_starting_at iug vertices starter= 
  let indneigh = induced_neighborhoods iug vertices in 
  helper_for_cycle_extraction indneigh ([],Set_of_integers.empty_set,starter) ;;   

let register (vertices,edges) =
   
   let idx = List.length(!repository)+1 in 
   let new_inner_graph = IG(UGI(idx),i_sort vertices,edges) in 
   let _ = (repository:=(!repository)@[new_inner_graph]) in 
   idx ;;

let image (G(UGI(idx),_,_,_)) = List.nth (!repository) (idx-1) ;;   

let set_level_neighborhood iug domain = i_setminus (neighbors_for_several iug domain) (i_sort domain) ;;

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
   Image.image (Set_of_integers.image (vertex_of_inner_index ug)) i_connected_components;;

let vertices_with_degree_at_least_three ug domain= 
   let iug = Inner.image ug in 
   let i_domain = Image.image (vertex_to_inner_index ug) domain in 
   let i_solutions = Inner.vertices_with_degree_at_least_three iug i_domain in 
   let g = vertex_of_inner_index ug  in  
   (Image.image (fun (k,nk)->(g k,Set_of_integers.image g nk))) i_solutions;;

let extract_cycle_from_starting_at ug domain starter= 
   let iug = Inner.image ug in 
   let i_domain = Image.image (vertex_to_inner_index ug) domain 
   and i_starter = vertex_to_inner_index ug starter in 
   let i_solutions = Inner.extract_cycle_from_starting_at iug i_domain i_starter in 
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
   if not(List.mem x old_vertices) then failwith("Splitting input is incorrect") else 
   if (List.mem x1 old_vertices)||(List.mem x2 old_vertices) then failwith("Splitting output is incorrect") else   
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
   Set_of_integers.image g i_solutions;;


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
   Set_of_integers.image g i_neighbors ;;
   
let edges (G(_,_,old_edges,_))= old_edges ;; 

let local_connected_components ug domain_in_imposed_order= 
   let iug = Inner.image ug in 
   let i_imposed_order = Image.image (vertex_to_inner_index ug) domain_in_imposed_order in
   let i_local_comps = Inner.local_connected_components iug i_imposed_order in 
   let g = vertex_of_inner_index ug  in 
   Image.image (Set_of_integers.image g) i_local_comps ;;

let induced_neighborhoods ug domain =
   let iug = Inner.image ug  in 
   let i_domain = Image.image (vertex_to_inner_index ug) domain in 
   let i_pairs = Inner.induced_neighborhoods iug i_domain in 
   let g = vertex_of_inner_index ug  in 
   Image.image (fun (k,z)->(g k,Set_of_integers.image g z))  i_pairs;;

let extract_some_cycle ug domain =
   if List.length(domain)=1 then domain else
   let indnei = induced_neighborhoods ug domain in 
   match List.find_opt (fun (_,l)->List.length(l)=1) indnei with 
   None -> failwith("extract_some_cycle")
   |Some(v0,_) -> extract_cycle_from_starting_at ug domain v0 ;;

let decompose_into_cycles ug domain =
   let comps = local_connected_components ug domain in 
   Image.image (extract_some_cycle ug) comps ;;


end ;;   

module Constrained = (Unconstrained:INTERFACE) ;;

end ;;

include Private.Constrained ;;


