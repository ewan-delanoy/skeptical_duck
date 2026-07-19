(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_bare_grammar.ml";;

*)

open Lrp_types ;;

module Private = struct 

let str_order = Total_ordering.lex_for_strings ;;
let str_merge = Ordered.merge str_order ;; 
let str_setminus = Ordered.setminus str_order ;; 
let str_sort = Ordered.sort str_order ;; 

let nonterminals (BG l)= str_sort (Image.image (fun (Prod(a,_))->a) l) ;;

let terminals (BG l) =
   let temp = str_sort (List.flatten(Image.image (fun (Prod(_,b))->b) l)) in 
   str_setminus temp (nonterminals (BG l)) ;;
   

 
let descendants_for_one gram (Item(_p,l))= 
  let (BG productions)=gram in 
  let n = List.length l 
  and j = List_again.index_of_in "." l in
  if j=n then [] else 
  let symb = List.nth l j in 
  Lrp_item.sort gram (List.filter_map ( fun pr->let (Prod(p2,_))=pr in 
  if p2=symb then Some(Lrp_item.first_item_from_production pr) else None) productions);;

let descendants_for_several gram items = Lrp_item.fold_merge gram 
   (Image.image (descendants_for_one gram) items) ;; 

let rec towards_closure gram (whole,_treated,to_be_treated) = 
  if to_be_treated = [] then St(whole) else 
  let temp = descendants_for_several gram to_be_treated in 
  let new_whole = Lrp_item.merge gram temp whole 
  and yet_untreated = Lrp_item.setminus gram temp whole  in 
  towards_closure gram (new_whole,whole,yet_untreated) ;;

let closure gram items = towards_closure gram (items,[],items) ;;   

let rename_in_symbol (old_name,new_name) symb = if symb = old_name then new_name else symb ;;

let rename_in_production rep (Prod(a,b)) = 
  let r = rename_in_symbol rep in (Prod(r a,Image.image r b)) ;;

let rename_in_grammar rep (BG l) = BG(Image.image (rename_in_production rep) l) ;; 

let start_symbol (BG l)= let (Prod(s,_)) = List.hd l in s ;; 

let augment ~earlier_start ~new_name_for_old_start old_bg=
  let old_start = start_symbol old_bg in 
  let (BG l1) = rename_in_grammar (old_start,new_name_for_old_start) old_bg in 
  BG((Prod(earlier_start,[new_name_for_old_start]))::l1) ;;

    

end ;;

let augment = Private.augment ;;

let closure = Private.closure ;;

let items gram =
  let (BG productions)=gram in 
  let unordered = List.flatten (Image.image Lrp_item.items_from_production productions) in 
  Lrp_item.sort gram unordered ;;


let nonterminals = Private.nonterminals ;;
let start_symbol = Private.start_symbol ;; 

let starter_lr0_molecule gram =
  let (BG productions)=gram in 
  closure gram [Lrp_item.first_item_from_production  (List.hd productions)];;

let symbols gram = Private.str_merge (Private.terminals gram)  (Private.nonterminals gram) ;;
let terminals = Private.terminals ;;