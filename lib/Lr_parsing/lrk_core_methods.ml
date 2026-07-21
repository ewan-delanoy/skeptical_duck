(*

Lrp is short for "LR Parsing"
Lrk here means LR(k), with k =0 or 1


#use"lib/Lr_parsing/lrk_core_methods.ml";;

*)

open Lrp_types ;;

module Private = struct 

let molecule l = St(l) ;;

let item_component (Atom(item)) = item ;; 

let atm_order gram = ((fun (Atom item1) (Atom item2) ->
    Lrp_item.order gram item1 item2 
): lr0_atom Total_ordering_t.t);;

let atm_sort gram = Ordered.sort (atm_order gram) ;;

let immediate_closure gram (Atom(Item(_p,l)))= 
  let (BG productions)=gram in 
  let n = List.length l 
  and j = List_again.index_of_in "." l in
  if j=n then [] else 
  let symb = List.nth l j in 
  atm_sort gram (List.filter_map ( fun pr->let (Prod(p2,_))=pr in 
  if p2=symb then Some(Atom(Lrp_item.first_item_from_production pr)) else None) productions);;

let push_dot_one_symbol symb (Atom item) = 
   Option.map (fun item->Atom item) (
     Lrp_item.push_dot_one_symbol symb item 
   );;

(*
let item_list_component (St items) = items ;;

let small_closure () = () ;;

let push () _symb = () ;;

let allowed_candidates_for_reduce () = () ;;

let initial_state = () ;;

let final_state = () ;; 
*)


end ;;

let immediate_closure = Private.immediate_closure ;;

let item_component  = Private.item_component ;; 

let molecule = Private.molecule ;;

let order_on_atoms = Private.atm_order ;;

let push_dot_one_symbol = Private.push_dot_one_symbol ;;