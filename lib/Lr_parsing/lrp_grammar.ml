(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

*)

open Lrp_types ;;

module Private = struct 

let str_order = Total_ordering.lex_for_strings ;;
let str_merge = Ordered.merge str_order ;; 
let str_setminus = Ordered.setminus str_order ;; 
let str_sort = Ordered.sort str_order ;; 

let nonterminals (G l)= str_sort (Image.image (fun (Prod(a,_))->a) l) ;;

let terminals (G l) =
   let temp = str_sort (List.flatten(Image.image (fun (Prod(_,b))->b) l)) in 
   str_setminus temp (nonterminals (G l)) ;;
   



end ;;

let items gram =
  let (G productions)=gram in 
  let unordered = List.flatten (Image.image Lrp_item.items_from_production productions) in 
  Lrp_item.sort gram unordered ;;


let nonterminals = Private.nonterminals ;;
let start_symbol (G l)= let (Prod(s,_)) = List.hd l in s ;; 
let symbols gram = Private.str_merge (Private.terminals gram)  (Private.nonterminals gram) ;;
let terminals = Private.terminals ;;