(*

#use"lib/lower_acyclic_subposet.ml";;

Start with an arbitrary relation R on a finite set L. Consider its transitive closure R'; then
for (x,y) in R, we say that x is a coatom of y, and we denote by coat(y) the set of all coatoms of y.

There are three kind of elements in L :
- The cylic elements, i.e. those elements x such that there are x2,x3,...,xr with xRx2Rx3Rx4...xrRx.
- The "cyclic accomplice" elements, i.e. those elements x not in any cycle but that have cyclic elements below them.
- Finally, the "completely acyclic" elements, i.e. those elements x such that x and the elements below are not cyclic.

The main function here computes the pair (cycles_found_so_far,completely_acyclic_elements) where 
cycles_found_so_far is a (non-canonical,non-exhaustive) set of cycles_found_so_far, and 
completely_acyclic_elements lists the c.a. elements in an order compatible with the initial relation R.
Those c.a. elements are accompanied by the corresponding lower intervals.

*)

module Private = struct

type 'a walker = {
   data_for_completely_acyclic_elements : ('a * (('a list) * ('a Set_of_polys_t.t))) list ;
   ordered_list_of_completely_acyclic_elements_found_so_far : 'a Set_of_polys_t.t ;
   cycles_found_so_far : 'a list list ;
   ordered_list_of_cyclic_elements_found_so_far : 'a Set_of_polys_t.t ;
   between : 'a list ;
   to_be_treated : 'a list ;
   final_answer_opt : ( ('a list list) * ( ('a * 'a list) list) ) option ; 
} ;; 

let extreme_state l opt = {
   data_for_completely_acyclic_elements = [] ;
   ordered_list_of_completely_acyclic_elements_found_so_far = Set_of_polys.empty_set ;
   cycles_found_so_far = [] ;
   ordered_list_of_cyclic_elements_found_so_far = Set_of_polys.empty_set ;
   between = [] ;
   to_be_treated = l ;
   final_answer_opt = opt ; 
} ;; 

let final_state w = extreme_state [] (Some(w.cycles_found_so_far,List_again.rev_map (fun (z,p)->(z,fst p)) w.data_for_completely_acyclic_elements)) ;;

let initial_state l = extreme_state l None ;; 


let completely_acyclic_case w current_head coatoms unordered_coatoms = 
  let several_coatoms=coatoms::(Image.image (fun z->snd(List.assoc z w.data_for_completely_acyclic_elements)) 
                      unordered_coatoms) in
  let lower_interval_in_global_order=Set_of_polys.fold_merge(several_coatoms) in
  let lower_interval_in_reversed_global_order=List.filter_map (
           fun (b,_)->if Set_of_polys.mem b lower_interval_in_global_order
             then Some(b)
             else None
  ) w.data_for_completely_acyclic_elements in
  let lower_interval_in_local_order=List.rev(lower_interval_in_reversed_global_order) in
  let data_for_head=(lower_interval_in_local_order,lower_interval_in_global_order) in
  {
    w with 
    data_for_completely_acyclic_elements = (current_head,data_for_head):: w.data_for_completely_acyclic_elements ;
    ordered_list_of_completely_acyclic_elements_found_so_far = Set_of_polys.insert current_head w.ordered_list_of_completely_acyclic_elements_found_so_far ;
    between = [] ;
    to_be_treated = List.filter (fun z->z<>current_head) w.to_be_treated ;
  } ;;

let cyclic_or_cyclic_accomplice_case w current_head = 
  {
    w with 
    ordered_list_of_cyclic_elements_found_so_far = Set_of_polys.insert current_head w.ordered_list_of_cyclic_elements_found_so_far ;
    between = [] ;
    to_be_treated = List.filter (fun z->z<>current_head) w.to_be_treated ;
  } ;;

let step coat w=
  if w.final_answer_opt<>None 
  then w 
  else
  if (w.between,w.to_be_treated)=([],[]) 
  then final_state w 
  else
  let current_head=
    	  (if w.between=[] 
    	   then List.hd(w.to_be_treated)
           else List.hd(w.between)
          ) in
    let unordered_coatoms=coat(current_head) in
    let coatoms=Set_of_polys.safe_set(unordered_coatoms) in
    let unclear_coatoms=Set_of_polys.setminus coatoms w.ordered_list_of_completely_acyclic_elements_found_so_far in
    if Set_of_polys.length(unclear_coatoms)=0
    then (* if we get here, all the coatoms are completely acyclic ; so is the head *)
          completely_acyclic_case w current_head coatoms unordered_coatoms
    else
    if (Set_of_polys.mem current_head unclear_coatoms) || (not(Set_of_polys.does_not_intersect unclear_coatoms w.ordered_list_of_cyclic_elements_found_so_far))
    then (* if we get here the head is a cycle by itself, or a cyclic accomplice *)
         cyclic_or_cyclic_accomplice_case w current_head
    else 
    (*see if we can close the cycle *)
    match List.find_opt(fun (_rev_left,x,_right)->Set_of_polys.mem x unclear_coatoms) 
     (List.rev(Three_parts.generic w.between)) with
     None->{
           w with 
          between = (
            if w.between=[]
            then [(Set_of_polys.hd unclear_coatoms);current_head]
            else (Set_of_polys.hd unclear_coatoms)::w.between
            ) ;
         }
    |Some(rev_left,x,_right)->
        (* let (before,_,_after)=Three_parts.select_center_element_and_reverse_left (fun x->x=p) w.between in
        let temp2=Image.image fst before in
        let new_cycle=(fst p)::(temp2@[current_head]) in *)

        let new_cycle=x::rev_left in 
        let ordered_cycle=Set_of_polys.sort new_cycle in
        {
           w with 
          cycles_found_so_far = new_cycle:: w.cycles_found_so_far ;
          ordered_list_of_cyclic_elements_found_so_far = Set_of_polys.merge ordered_cycle w.ordered_list_of_cyclic_elements_found_so_far ;
          between = [] ;
          to_be_treated = List.filter (fun z->Set_of_polys.nmem z ordered_cycle) w.to_be_treated ;
        } ;;

let compute coat l=
  let rec tempf=(fun walker->
    if walker.final_answer_opt <>None
    then Option.get walker.final_answer_opt
    else tempf(step coat walker)
    ) in
    tempf(initial_state l);;
    
end ;;

let compute = Private.compute ;;


(*

let sugar i=
   if (i<4)||(i>20) then [] else
   if i=11 then [5] else [i+1];;
    
(reconstruct_linear_poset sugar (Int_range.range 1 30))=(
   ([[5; 6; 7; 8; 9; 10; 11]],
 [(1, []); (2, []); (3, []); (21, []); (20, [21]); (19, [21; 20]);
  (18, [21; 20; 19]); (17, [21; 20; 19; 18]); (16, [21; 20; 19; 18; 17]);
  (15, [21; 20; 19; 18; 17; 16]); (14, [21; 20; 19; 18; 17; 16; 15]);
  (13, [21; 20; 19; 18; 17; 16; 15; 14]);
  (12, [21; 20; 19; 18; 17; 16; 15; 14; 13]); (22, []); (23, []); (24, []);
  (25, []); (26, []); (27, []); (28, []); (29, []); (30, [])])

);;  

let some_edges=
  [
    (1,4);(1,16);(2,6);(2,7);(3,16);(7,11);(7,19);(8,11);(8,15);(9,19);
    (10,1);(10,18);(11,2);(12,16);(13,5);(15,13);(16,17);(17,10);(17,14);(19,20);
    (20,21);(21,9)
  
  ];;

let brown j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

(reconstruct_linear_poset brown (Int_range.range 1 21))=(
   ([[2; 11; 7]; [1; 10; 17; 16]],
 [(3, []); (8, []); (15, [8]); (13, [8; 15]); (5, [8; 15; 13]); (12, [])])

);;  

let some_edges=
  [
    (10, 1); (9, 10); (4, 9); (5, 4); (6, 5); (2, 6); (8, 2); (3, 8); (7, 3);(4, 7)
  ];;

let yellow j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

(reconstruct_linear_poset yellow (Int_range.range 1 10))=(
   [[4; 5; 6; 2; 8; 3; 7]], []
);;  

*)
 
