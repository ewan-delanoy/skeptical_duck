(*

Lrp is short for "LR Parsing"
Lrk here means LR(k), with k =0 or 1


#use"lib/Lr_parsing/lr1_seed.ml";;

*)



open Lrp_types ;;
open Lrp_constant ;;

type atom = Atom of item * string ;;
type molecule = Molecule of atom list ;;

module Private = struct 

let atoms_inside (Molecule atoms)= atoms ;; 

let molecule l = Molecule(l) ;;

let item_component (Atom (item,_lah)) = item ;; 

let make_atom item lah = Atom (item,lah) ;;

let lookahead_component (Atom (_item,lah)) = lah ;; 

let atm_order gram = ((fun atom1 atom2 ->
    let trial1 = Lrp_grammar.order_on_items gram (item_component atom1) (item_component atom2) in 
    if trial1<>Total_ordering_result_t.Equal 
    then trial1 
    else Total_ordering.lex_for_strings (lookahead_component atom1)  (lookahead_component atom2)  
): atom Total_ordering_t.t);;

let atm_sort gram = Ordered.sort (atm_order gram) ;;

let immediate_closure gram atom =
  let (Item(_p,l))= item_component atom in 
  let productions =Lrp_grammar.productions gram in 
  let n = List.length l 
  and j = List_again.index_of_in "." l in
  if j=n then [] else 
  let symb = List.nth l j in 
  let incomplete_ender = List_again.long_tail (j+1) l 
  and a = lookahead_component atom in
  let ender = incomplete_ender@[a] in 
  let usable_lookaheads = Lrp_grammar.furst_set_for_form gram ender in 
  let usable_items = List.filter_map ( fun pr->let (Prod(p2,_))=pr in 
  if p2=symb then Some(Lrp_item.first_item_from_production pr) else None) productions  in 
  let product = Cartesian.product usable_items usable_lookaheads in 
  atm_sort gram (Image.image (fun (item,lah)->make_atom item lah) product);;

let push_dot_one_symbol symb atom =
  let item= item_component atom 
  and lah = lookahead_component atom in 
   Option.map (fun new_item ->make_atom new_item lah) (
     Lrp_item.push_dot_one_symbol symb item 
   );;

let starter_atom bare_grammar = 
  let productions =Lrp_grammar.productions bare_grammar in 
  make_atom(Lrp_item.first_item_from_production  (List.hd productions)) end_marker;;

let ender_atom bare_grammar = 
  let productions =Lrp_grammar.productions bare_grammar in 
  make_atom(Lrp_item.last_item_from_production  (List.hd productions)) end_marker;;  

 let test_for_allowing_reduction (_gram:grammar) atom ~head_of_production ~terminal =
  let comb = ref(String.length head_of_production) in 
  let _ = (comb:=0) in 
  terminal = lookahead_component atom ;;

let visualize_atom (Atom(item,lah)) = (Lrp_item.visualize item)^" , "^lah ;;

end ;;

let atoms_inside = Private.atoms_inside ;; 

let empty_one = Private.molecule [] ;;

let ender_atom = Private.ender_atom ;;

let immediate_closure = Private.immediate_closure ;;

let item_component  = Private.item_component ;; 

let lookahead_component  = Private.lookahead_component ;; 

let molecule = Private.molecule ;;

let name = "LR(1)" ;;

let order_on_atoms = Private.atm_order ;;

let push_dot_one_symbol = Private.push_dot_one_symbol ;;

let starter_atom = Private.starter_atom ;;

let test_for_allowing_reduction = Private.test_for_allowing_reduction ;;

let visualize_atom = Private.visualize_atom ;;

