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

let rename_in_symbol (old_name,new_name) symb = if symb = old_name then new_name else symb ;;

let rename_in_production rep (Prod(a,b)) = 
  let r = rename_in_symbol rep in (Prod(r a,Image.image r b)) ;;

let rename_in_grammar rep (BG l) = BG(Image.image (rename_in_production rep) l) ;; 

let start_symbol (BG l)= let (Prod(s,_)) = List.hd l in s ;; 

let augment ~earlier_start ~new_name_for_old_start old_bg=
  let old_start = start_symbol old_bg in 
  let (BG l1) = rename_in_grammar (old_start,new_name_for_old_start) old_bg in 
  BG((Prod(earlier_start,[new_name_for_old_start]))::l1) ;;

let make prods = BG(Image.image (fun (a,b)->Prod(a,b)) prods) ;;       

end ;;

let augment = Private.augment ;;

let items gram =
  let (BG productions)=gram in 
  let unordered = List.flatten (Image.image Lrp_item.items_from_production productions) in 
  Lrp_item.sort gram unordered ;;


let make = Private.make ;;   
let nonterminals = Private.nonterminals ;;

let productions (BG(prods)) = prods ;;
let start_symbol = Private.start_symbol ;; 

let symbols gram = Private.str_merge (Private.terminals gram)  (Private.nonterminals gram) ;;
let terminals = Private.terminals ;;