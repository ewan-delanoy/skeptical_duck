(* 

#use"Hex_analysis/hex_finished_kite.ml";;

*)


module Private = struct 


let to_molecular_linker  pk =
   (* The kite is assumed to be finished *) 
   Hex_molecular_linker.fold_merge 
   (Option.filter_and_unpack 
      Hex_kite_element.to_molecular_linker 
        (pk.Hex_partial_kite_t.steps_so_far));;

let helper2_for_removing_redundant_islands treated pending1 pending2  = 
   if (Hex_kite_springless_element.is_an_island pending1)
          &&
          (Hex_kite_springless_element.is_an_island pending2)
   then List.rev (pending2::treated)
   else List.rev (pending2::pending1::treated) ;;

let rec helper_for_removing_redundant_islands (treated,pending1,pending2,to_be_treated) = 
   match to_be_treated with 
    [] -> helper2_for_removing_redundant_islands treated pending1 pending2 
   |pending3::others ->
       if (Hex_kite_springless_element.is_an_island pending1)
          &&
          (Hex_kite_springless_element.is_an_island pending2)
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


exception Deduce_boarded_islands_exn of (Hex_kite_springless_element_t.t list) * int ;;

let deduce_boarded_islands  l (birth,death) = 
    let n = ((List.length l)-1)/2  in 
    let gl = (fun j->List.nth l (j-1)) in 
    let sea_entry = (fun x->(Hex_kite_springless_element.claim_sea (x)).Hex_named_connector_t.entry )
    and sea_exit = (fun x->(Hex_kite_springless_element.claim_sea (x)).Hex_named_connector_t.exit ) in 
    let first_in_triple =(fun k->
       if k=1 then Hex_island.eviscerate birth else sea_exit(gl(2*k-3))
    ) 
    and second_in_triple = (fun k->
       if k=1   then birth else 
       if k=n+2 then death else Hex_kite_springless_element.claim_island(gl (2*k-2))
    )   
    and third_in_triple = (fun k->
       if k=n+2 then Hex_island.eviscerate death else sea_entry(gl(2*k-1))
    ) in  
    let naive_trier=(fun k ->
         (first_in_triple k,second_in_triple k,third_in_triple k) ) in 
    let  trier=(fun k->try naive_trier k with _->raise(Deduce_boarded_islands_exn(l,k))) in     
    Ennig.doyle trier 1 (n+2);;

let active_part  pk =
   (* The kite is assumed to be finished *)  
   let birth = pk.Hex_partial_kite_t.place_of_birth 
   and death = Hex_partial_kite_field.place_of_death pk   in 
    let unfiltered_l= List.rev pk.Hex_partial_kite_t.steps_so_far in 
    let l = remove_redundant_islands (Image.image Hex_kite_element.compress_to_springless  unfiltered_l)  in 
    let boarded_islands = deduce_boarded_islands l (birth,death)  in 
    let contribution_from_seas = Hex_cell_set.fold_merge(Option.filter_and_unpack (
       function (Hex_kite_springless_element_t.Sea(nc)) -> Some(Hex_named_connector.outer_earth nc)
       |_->None
    ) l) 
    and contribution_from_islands = Hex_cell_set.fold_merge(Image.image 
    (function (island1,island,island2)-> 
         Hex_island.minimal_connection (island1,island2) island
    ) boarded_islands) 
    and contribution_from_springboards = Option.filter_and_unpack (
       function (Hex_kite_element_t.Springboard(spr))-> Some(Hex_springboard.active_part spr)
        | _ -> None 
   ) unfiltered_l  in 
   let possibly_too_large = Hex_cell_set.fold_merge 
     (contribution_from_islands::contribution_from_seas::contribution_from_springboards) in 
   let molecular_part = Hex_molecular_linker.support(to_molecular_linker pk) in 
   Hex_cell_set.setminus possibly_too_large molecular_part;;



let solution_details pk = 
    let steps_in_order=List.rev(pk.Hex_partial_kite_t.steps_so_far) in
    let (fst_step,other_steps)=Listennou.ht steps_in_order in     
    let birth = pk.Hex_partial_kite_t.place_of_birth in 
    (birth,fst_step,other_steps,to_molecular_linker pk,active_part pk);;





end ;; 

let active_part = Private.active_part ;;
let solution_details = Private.solution_details ;;
let to_molecular_linker = Private.to_molecular_linker;;
