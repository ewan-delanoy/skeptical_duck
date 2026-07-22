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

let productions gram = gram.productions ;;

let counter = ref(0) ;;

let make_from_prods prods = 
  let new_val = (!counter)+1 in 
  let _ = (counter:=new_val) in 
  {
     grammar_serial_number = new_val ;
     productions =  prods ; 
  } ;;

let make prods = 
  make_from_prods (Image.image (fun (a,b)->Prod(a,b)) prods) ;;


let memoize hashtbl f gram =
   match Hashtbl.find_opt hashtbl gram.grammar_serial_number with 
   Some old_answer -> old_answer 
   | None -> 
     let answer = f gram in 
     let _ = Hashtbl.replace hashtbl gram.grammar_serial_number answer in 
     answer ;;

let compute_nonterminals_naively gram= 
  str_sort (Image.image (fun (Prod(a,_))->a) (productions gram)) ;;

let hashtbl_for_nonterminals = Hashtbl.create 100;;

let nonterminals = memoize hashtbl_for_nonterminals compute_nonterminals_naively ;;

let compute_terminals_naively gram= 
   let l = productions gram in 
   let temp = str_sort (List.flatten(Image.image (fun (Prod(_,b))->b) l)) in 
   str_setminus temp (nonterminals gram) ;; ;;

let hashtbl_for_terminals = Hashtbl.create 100;;

let terminals = memoize hashtbl_for_terminals compute_terminals_naively ;; 

let compute_all_symbols_naively gram= str_merge (terminals gram)  (nonterminals gram);; 

let hashtbl_for_all_symbols = Hashtbl.create 100;;

let all_symbols = memoize hashtbl_for_all_symbols compute_all_symbols_naively ;; 

let rename_in_symbol (old_name,new_name) symb = if symb = old_name then new_name else symb ;;

let rename_in_production rep (Prod(a,b)) = 
  let r = rename_in_symbol rep in (Prod(r a,Image.image r b)) ;;

let rename_in_grammar rep gram =
  let old_prods = productions gram in 
  let new_prods = Image.image (rename_in_production rep) old_prods in 
  make_from_prods new_prods ;; 

let start_symbol gram= let (Prod(s,_)) = List.hd (productions gram) in s ;; 

let augment ~earlier_start ~new_name_for_old_start old_gram=
  let old_start = start_symbol old_gram in 
  let new_gram = rename_in_grammar (old_start,new_name_for_old_start) old_gram in 
  let prods1 = productions new_gram in 
  make_from_prods((Prod(earlier_start,[new_name_for_old_start]))::prods1) ;;

let index_of_production_in_grammar item gram =
  let prods = productions gram in 
  List_again.index_of_in (Lrp_item.production_from_item item) prods ;;

let order_on_items gram = ((fun item1 item2 ->
  let trial1 = Total_ordering.for_integers (Lrp_item.index_of_dot item1) (Lrp_item.index_of_dot item2) in 
  if trial1<> Total_ordering_result_t.Equal then trial1 else 
  Total_ordering.for_integers 
    (index_of_production_in_grammar item1 gram) 
      (index_of_production_in_grammar item2 gram)  
): item Total_ordering_t.t);;       

end ;;

let all_symbols = Private.all_symbols ;;
let augment = Private.augment ;;

let items gram =
  let prods = Private.productions gram in 
  let unordered = List.flatten (Image.image Lrp_item.items_from_production prods) in 
  Ordered.sort (Private.order_on_items gram) unordered ;;


let make = Private.make ;;  
let make_from_prods = Private.make_from_prods ;;  
let nonterminals = Private.nonterminals ;;

let order_on_items = Private.order_on_items ;;
let productions = Private.productions ;;
let start_symbol = Private.start_symbol ;; 
let terminals = Private.terminals ;;