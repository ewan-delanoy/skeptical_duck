(* 

#use"Hex_analysis/hex_finished_kite.ml";;

*)


module Private = struct 


let to_molecular_linker  pk =
   (* The kite is assumed to be finished *) 
   Hex_molecular_linker.fold_merge 
   (Option.filter_and_unpack 
      Hex_expsv_kite_element.to_molecular_linker 
        (pk.Hex_expsv_partial_kite_t.steps_so_far));;

let helper2_for_removing_redundant_islands treated pending1 pending2  = 
   if (Hex_expsv_kite_springless_element.is_an_island pending1)
          &&
          (Hex_expsv_kite_springless_element.is_an_island pending2)
   then List.rev (pending2::treated)
   else List.rev (pending2::pending1::treated) ;;

let rec helper_for_removing_redundant_islands (treated,pending1,pending2,to_be_treated) = 
   match to_be_treated with 
    [] -> helper2_for_removing_redundant_islands treated pending1 pending2 
   |pending3::others ->
       if (Hex_expsv_kite_springless_element.is_an_island pending1)
          &&
          (Hex_expsv_kite_springless_element.is_an_island pending2)
       then helper_for_removing_redundant_islands (treated,pending2,pending3,others)
       else helper_for_removing_redundant_islands (pending1::treated,pending2,pending3,others) ;;
      
let remove_redundant_islands l=match l with 
  [] -> []
  |pending1::others1 ->
    (
      match others1 with 
      [] -> [pending1]
      |pending2::others2 -> helper_for_removing_redundant_islands ([],pending1,pending2,others2)
    );; 

let trim_for_island_boarding l = 
   (* if the first elt is an island, it comes from a springboard, and 
      all the info it carries is already contained in the place_of_birth field.
      In most cases, we can (and in fact we must) delete it *)
   match l with 
   [] -> (None,l) 
   |elt :: others -> 
        let opt_island = Hex_expsv_kite_springless_element.opt_island elt in 
        let l1 = (match opt_island with None -> l |Some _ ->others) in
        let n1 = List.length l1 in 
        if ((n1 mod 2)=0) && (l1 <> [])
        then (* if we get here the last elt is an island, and 
                all the info it carries is already contained in the place_of_death value. 
               We can (and in fact we must) delete it
             *)
             (opt_island,List.rev(List.tl(List.rev l1)) )
        else (opt_island,l1);; 

let unique_boarded_island island (birth,death)=
   [Hex_island.eviscerate birth,island,Hex_island.eviscerate death];;

exception Deduce_boarded_islands_exn of (Hex_expsv_kite_springless_element_t.t list) * int ;;

let deduce_boarded_islands  untrimmed_l (birth,death) =
    let (opt_island,l) =  trim_for_island_boarding untrimmed_l in
    if l=[] then unique_boarded_island (Option.unpack opt_island) (birth,death) else  
    let n = ((List.length l)-1)/2  in 
    let gl = (fun j->List.nth l (j-1)) in 
    let sea_entry = (fun x->(Hex_expsv_kite_springless_element.claim_sea (x)).Hex_expsv_named_connector_t.entry )
    and sea_exit = (fun x->(Hex_expsv_kite_springless_element.claim_sea (x)).Hex_expsv_named_connector_t.exit ) in 
    let first_in_triple =(fun k->
       if k=1 then Hex_island.eviscerate birth else sea_exit(gl(2*k-3))
    ) 
    and second_in_triple = (fun k->
       if k=1   then birth else 
       if k=n+2 then death else Hex_expsv_kite_springless_element.claim_island(gl (2*k-2))
    )   
    and third_in_triple = (fun k->
       if k=n+2 then Hex_island.eviscerate death else sea_entry(gl(2*k-1))
    ) in  
    let naive_trier=(fun k ->
         (first_in_triple k,second_in_triple k,third_in_triple k) ) in 
    let  trier=(fun k->try naive_trier k with _->raise(Deduce_boarded_islands_exn(l,k))) in     
    Ennig.doyle trier 1 (n+2);;

let contribution_from_island_in_active_part pk prepared_list = 
   let birth = pk.Hex_expsv_partial_kite_t.place_of_birth 
   and death = Hex_expsv_partial_kite_field.place_of_death pk   in 
   if birth = death 
   then Hex_island.minimal_version_for_two_edged death
   else 
    let boarded_islands = deduce_boarded_islands prepared_list (birth,death)  in 
    Hex_cell_set.fold_merge(Image.vorstellung 
    (function (island1,island,island2)-> 
         Hex_island.minimal_connection (island1,island2) island
    ) boarded_islands) ;;

let possibly_too_large_active_part  pk =
    let unprepared_l= List.rev pk.Hex_expsv_partial_kite_t.steps_so_far in 
    let prepared_l = remove_redundant_islands (Image.vorstellung Hex_expsv_kite_element.compress_to_springless  unprepared_l)  in 
    let contribution_from_seas = Hex_cell_set.fold_merge(Option.filter_and_unpack (
       function (Hex_expsv_kite_springless_element_t.Sea(nc)) -> Some(Hex_expsv_named_connector.active_part nc)
       |_->None
    ) prepared_l) 
    and contribution_from_islands = contribution_from_island_in_active_part pk prepared_l
    and contribution_from_springboards = Option.filter_and_unpack (
       function (Hex_expsv_kite_element_t.Springboard(spr))-> Some(Hex_expsv_springboard.active_part spr)
        | _ -> None 
   ) unprepared_l  in 
   Hex_cell_set.fold_merge 
     (contribution_from_islands::contribution_from_seas::contribution_from_springboards) ;;


let active_part pk =
    let molecular_part = Hex_molecular_linker.support(to_molecular_linker pk) in 
   Hex_cell_set.setminus (possibly_too_large_active_part pk) molecular_part;;

let solution_details pk = 
    let steps_in_order=List.rev(pk.Hex_expsv_partial_kite_t.steps_so_far) in
    let (fst_step,other_steps)=Listennou.ht steps_in_order in     
    let birth = pk.Hex_expsv_partial_kite_t.place_of_birth in 
    (birth,fst_step,other_steps,to_molecular_linker pk,active_part pk);;





end ;; 

let active_part = Private.active_part ;;
let solution_details = Private.solution_details ;;
let to_molecular_linker = Private.to_molecular_linker;;
