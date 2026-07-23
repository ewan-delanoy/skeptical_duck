(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

*)

open Lrp_types ;;

module Private = struct 

let str_order = Total_ordering.lex_for_strings ;;
let str_fold_merge = Ordered.fold_merge str_order ;;
let str_insert = Ordered.insert str_order ;; 
let str_mem = Ordered.mem str_order ;; 
let str_merge = Ordered.merge str_order ;; 
let str_setminus = Ordered.setminus str_order ;; 
let str_sort = Ordered.sort str_order ;; 

let end_marker = "Endmarker" ;;

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


let memoize_constant hashtbl f gram =
   match Hashtbl.find_opt hashtbl gram.grammar_serial_number with 
   Some old_answer -> old_answer 
   | None -> 
     let answer = f gram in 
     let _ = Hashtbl.replace hashtbl gram.grammar_serial_number answer in 
     answer ;;

let memoize_univar hashtbl f gram var1=
   match Hashtbl.find_opt hashtbl (gram.grammar_serial_number,var1) with 
   Some old_answer -> old_answer 
   | None -> 
     let answer = f gram var1 in 
     let _ = Hashtbl.replace hashtbl (gram.grammar_serial_number,var1) answer in 
     answer ;;



let compute_nonterminals_naively gram= 
  str_sort (Image.image (fun (Prod(a,_))->a) (productions gram)) ;;

let hashtbl_for_nonterminals = Hashtbl.create 100;;

let nonterminals = memoize_constant hashtbl_for_nonterminals compute_nonterminals_naively ;;

let compute_terminals_naively gram= 
   let l = productions gram in 
   let temp = str_sort (List.flatten(Image.image (fun (Prod(_,b))->b) l)) in 
   str_setminus temp (nonterminals gram) ;; ;;

let hashtbl_for_terminals = Hashtbl.create 100;;

let terminals = memoize_constant hashtbl_for_terminals compute_terminals_naively ;; 

let compute_all_symbols_naively gram= str_merge (terminals gram)  (nonterminals gram);; 

let hashtbl_for_all_symbols = Hashtbl.create 100;;

let all_symbols = memoize_constant hashtbl_for_all_symbols compute_all_symbols_naively ;; 

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

module Emptiable_nonterminals = struct

let initial_data gram =
   let prods1 = productions gram in 
   let prods2= Image.image (fun (Prod(h,l))->(h,str_sort l)) prods1 in
   let unordered_nonterminals = Image.image fst prods2 in 
   let nonterminals = str_sort unordered_nonterminals in 
   Image.image (fun h->(h,List.assoc h prods2)) nonterminals ;; 


let pusher (simplified_productions,level) =
    let next_simplified_productions = Image.image (fun (h,l)->
        (h,str_setminus l level)
      ) simplified_productions in 
    let next_level = List.filter_map(fun (h,l)->if l=[] then Some h else None) next_simplified_productions in 
    (next_simplified_productions,next_level)  ;;

let rec iterator (preceding_pair,pair) =
   let (_,preceding_level) = preceding_pair 
   and (_,level) = pair in 
   if List.length preceding_level = List.length level 
   then level 
   else iterator(pair,pusher pair) ;;   

let compute_emptiable_nonterminals_naively gram = 
   let first_pair = (initial_data gram,[]) in 
   let final_level = iterator (first_pair,pusher first_pair) in 
   final_level ;;

let hashtbl_for_emptiable_nonterminals = Hashtbl.create 100;;

let emptiable_nonterminals = memoize_constant hashtbl_for_emptiable_nonterminals compute_emptiable_nonterminals_naively ;; 

let compute_is_emptiable_naively gram symb=str_mem symb (emptiable_nonterminals gram);; 
    

let hashtbl_for_is_emptiable = Hashtbl.create 100;;

let is_emptiable = memoize_univar hashtbl_for_is_emptiable compute_is_emptiable_naively ;; 

end ;;

module Furst_set = struct 

(*

We compute so-called "FIRST" sets (here renamed "Furst" sets for convenience) 

*)

let elements_having_a_wholly_emptiable_left gram form =
   let rec tempf = (
     fun (treated,to_be_treated) -> 
      match to_be_treated with 
      [] -> List.rev(treated)
      |symb::other_symbs ->
         if Emptiable_nonterminals.is_emptiable gram symb 
         then tempf(symb::treated,other_symbs)
         else List.rev(symb::treated)  
   ) in 
   tempf([],form) ;; 

let hashtbl_for_furst_set = Hashtbl.create 100;;

let expand gram already_found_prefixes (older_heads,current_head) = 
   let termies = terminals gram in 
   let updated_heads = str_insert current_head older_heads in 
   let productions =productions gram in 
   let temp1 = List.flatten(List.filter_map (fun (Prod(a,b))->
      if a<>current_head then None else Some( elements_having_a_wholly_emptiable_left gram b)) productions) in 
   let candidates =  str_setminus (str_sort temp1) updated_heads in 
   let (new_prefixes1,nonterminal_candidates) = List.partition (fun symb->str_mem symb termies) candidates in 
   let using_precedent_computations = Image.image (fun symb->
      (symb,Hashtbl.find_opt hashtbl_for_furst_set (gram.grammar_serial_number,symb))) nonterminal_candidates in 
   let (to_be_treated_next,already_treated)  = List.partition (fun (_,opt)->opt=None) 
       using_precedent_computations in 
   let new_prefixes2 = str_fold_merge  (Image.image (fun (_,opt)->Option.get opt) already_treated) in 
   let new_prefixes = str_merge new_prefixes1 new_prefixes2 in 
   (str_merge new_prefixes already_found_prefixes,updated_heads,Image.image fst to_be_treated_next) ;;

let rec iterator gram (already_found_prefixes,to_be_treated) = 
   match to_be_treated with 
   [] -> already_found_prefixes 
   |pair :: other_pairs -> 
     let (new_set_of_prefixes,new_pairs) = (
        match Hashtbl.find_opt hashtbl_for_furst_set (gram.grammar_serial_number,snd pair) with 
        Some old_answer -> (str_merge old_answer already_found_prefixes,[])
        |None -> 
         let (new_set_of_prefixes2,updated_heads,to_be_treated_next) = expand gram already_found_prefixes pair in 
         let new_pairs2 = Image.image (fun candidate ->(updated_heads,candidate)) to_be_treated_next  in 
         (new_set_of_prefixes2,new_pairs2)
     )  in 
     iterator gram (new_set_of_prefixes,new_pairs@other_pairs) ;;
  
let compute_furst_set_naively gram symb= iterator  gram ([],[[],symb]) ;;

let furst_set_for_symbol = memoize_univar hashtbl_for_furst_set compute_furst_set_naively ;; 

let furst_set_for_form gram form = 
    let symbols = elements_having_a_wholly_emptiable_left gram form in 
    iterator  gram ([],Image.image(fun symb -> ([],symb)) symbols) ;;



end ;;  

module Rightmost_ancestors = struct 

let direct_rightmost_ancestors gram symb = 
   let productions = productions gram in 
   str_sort(List.filter_map (fun (Prod(a,b))->if List.hd(List.rev b)=symb then Some a else None)  productions) ;;

let direct_rightmost_ancestors_for_several gram symbs =
  str_fold_merge (Image.image (direct_rightmost_ancestors gram) symbs) ;;

let rec helper gram (current_whole,to_be_treated) =
   let possibly_new = direct_rightmost_ancestors_for_several gram to_be_treated in 
   let really_new = str_setminus possibly_new current_whole in 
   if really_new = []
   then current_whole 
   else helper gram (str_merge current_whole really_new,really_new) ;;   

let compute_rightmost_ancestors_naively gram symb = helper gram ([],[symb]) ;;

let hashtbl_for_rightmost_ancestors = Hashtbl.create 100;;

let rightmost_ancestors = memoize_univar hashtbl_for_rightmost_ancestors compute_rightmost_ancestors_naively ;; 

end ;;   

module Follow_set = struct 

   let completions_on_the_right_for_in_production symb (Prod(_a,b)) =
      let temp1 = Two_winged_bird_on_plank.generic b in 
      List.filter_map (
        fun (_rev_left,center,right) ->
          if center <> symb then None else 
          Some right   
      ) temp1 ;;

    let completions_on_the_right_for_in_productions symb productions =
     List.flatten (Image.image (completions_on_the_right_for_in_production symb) productions) ;;   


let direct_follow_set gram symb =
   let prods = productions gram in 
   let completions = completions_on_the_right_for_in_productions symb prods in 
   let (empty_completions,nonempty_completions) = List.partition (fun x->x=[]) completions in
   let rightmost_contribution = (if empty_completions=[] then [] else [end_marker]) in 
   str_fold_merge (rightmost_contribution::
   (Image.image (Furst_set.furst_set_for_form gram) nonempty_completions)) ;;

let compute_follow_set_naively gram symb =
   str_fold_merge (Image.image (direct_follow_set gram)
    (symb::(Rightmost_ancestors.rightmost_ancestors gram symb))) ;;

let hashtbl_for_follow_set = Hashtbl.create 100;;

let follow_set = memoize_univar hashtbl_for_follow_set compute_follow_set_naively ;; 

end ;;   


end ;;

let all_symbols = Private.all_symbols ;;
let augment = Private.augment ;;

let follow_set = Private.Follow_set.follow_set ;;

let furst_set_for_form = Private.Furst_set.furst_set_for_form ;;

let furst_set_for_symbol = Private.Furst_set.furst_set_for_symbol ;;

let is_emptiable = Private.Emptiable_nonterminals.is_emptiable ;;

let items gram =
  let prods = Private.productions gram in 
  let unordered = List.flatten (Image.image Lrp_item.items_from_production prods) in 
  Ordered.sort (Private.order_on_items gram) unordered ;;


let make = Private.make ;;  
let make_from_prods = Private.make_from_prods ;;  
let nonterminals = Private.nonterminals ;;

let order_on_items = Private.order_on_items ;;
let productions = Private.productions ;;

let rightmost_ancestors = Private.Rightmost_ancestors.rightmost_ancestors ;;
let start_symbol = Private.start_symbol ;; 
let terminals = Private.terminals ;;