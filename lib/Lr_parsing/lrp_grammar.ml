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

   let add_new_paths_to_lr0_state gram lr0_state paths_to_be_added =
   let new_registry = Lrp_registry.add_new_paths_to_lr0_state gram.registry lr0_state paths_to_be_added in 
   (gram.registry<- new_registry) ;;

   let register_lr0_state gram lr0_state = 
   let (new_registry,registered_state) = Lrp_registry.register_lr0_state gram.registry lr0_state in 
   let _ = (gram.registry<- new_registry) in  
   registered_state;; 

   let ghetto_for_jterm gram (St items) symb = Lrp_bare_grammar.closure gram.core 
(Lrp_item.push_dots_one_symbol gram.core symb items);;

let compute_ghetto_naively gram (RSt (_idx,items)) symb = 
  let old_lr0_state = St items in 
  let new_lr0_state = ghetto_for_jterm gram old_lr0_state symb in 
  let new_rlr0_state = register_lr0_state gram new_lr0_state in 
  let (Rg regy) = gram.registry in 
  let older_paths = List.assoc old_lr0_state regy in 
  let paths_to_be_added= Image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lr0_state gram new_lr0_state paths_to_be_added in 
  new_rlr0_state  
  ;;


let compute_ghetto gram rlr_state symb =
  let (RSt (idx,_items)) = rlr_state in  
  match Hashtbl.find_opt gram.hashtbl_for_ghettoes (idx,symb) with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_ghetto_naively gram rlr_state symb in 
   let _ = Hashtbl.add gram.hashtbl_for_ghettoes (idx,symb) new_answer in 
   new_answer
  ;;

let rlr0_state_order = ((fun (RSt (i1,_))  (RSt (i2,_))->Total_ordering.for_integers i1 i2): registered_lr0_state Total_ordering_t.t) ;;
let rlr0_state_fold_merge = Ordered.fold_merge rlr0_state_order ;;
let rlr0_state_merge = Ordered.merge rlr0_state_order ;;
let rlr0_state_setminus = Ordered.setminus rlr0_state_order ;;
let rlr0_state_sort = Ordered.sort rlr0_state_order ;;



let ghetto_neighbors_for_one gram rlr0_state = 
   let all_symbols = Lrp_bare_grammar.symbols gram.core in 
   rlr0_state_sort(Image.image (compute_ghetto gram rlr0_state) all_symbols) ;;

let ghetto_neighbors_for_several gram lr0_states = rlr0_state_fold_merge
 (Image.image (ghetto_neighbors_for_one gram) lr0_states) ;;

let rec towards_ghetto_neighborhood gram (whole,_treated,to_be_treated) = 
  if to_be_treated = [] then whole else 
  let temp = ghetto_neighbors_for_several gram to_be_treated in 
  let new_whole = rlr0_state_merge temp whole 
  and yet_untreated = rlr0_state_setminus temp whole  in 
 towards_ghetto_neighborhood gram (new_whole,whole,yet_untreated) ;;

let ghetto_neighborhood gram lr0_states = towards_ghetto_neighborhood gram (lr0_states,[],lr0_states) ;; 

let starter_rlr0_state gram = 
   let starter_lr0_state = Lrp_bare_grammar.starter_lr0_state gram.core in
   let answer = register_lr0_state gram starter_lr0_state in 
   let _ = add_new_paths_to_lr0_state gram starter_lr0_state [[]] in 
   answer;;
;;

let all_lr0_states gram = ghetto_neighborhood gram [starter_rlr0_state gram] ;;

let get_terminals gram = 
   match gram.terminals with 
   Some old_answer -> old_answer 
   | None -> 
      let answer = Lrp_bare_grammar.terminals gram.core in 
      let _ = (gram.terminals <- (Some answer);
      List.iter (fun terminal -> Hashtbl.add gram.hashtbl_for_furst_sets terminal [terminal]) answer
      ) in 
      answer;;


module Emptiable_nonterminals = struct

let initial_data gram =
   let (BG prods1) = gram.core in 
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

let all gram = 
   match gram.emptiable_nonterminals with 
   Some old_answer -> old_answer 
   | None ->
   let first_pair = (initial_data gram,[]) in 
   let final_level = iterator (first_pair,pusher first_pair) in 
   let symbols = Lrp_bare_grammar.symbols gram.core in 
   let _ = (List.iter (fun x->
     Hashtbl.add gram.hashtbl_for_emptiability x (List.mem x final_level)   
   ) symbols;
     gram.emptiable_nonterminals <- (Some final_level);
   ) in 
   final_level ;;



end ;;

module Furst_set = struct 

(*

We compute so-called "FIRST" sets (here renamed "Furst" sets for convenience) 

*)

let elements_having_a_wholly_emptiable_left gram form =
   (* It is assumed that gram.hashtbl_for_emptiability has already been filled *)
   let rec tempf = (
     fun (treated,to_be_treated) -> 
      match to_be_treated with 
      [] -> List.rev(treated)
      |symb::other_symbs ->
         if Hashtbl.find gram.hashtbl_for_emptiability symb 
         then tempf(symb::treated,other_symbs)
         else List.rev(symb::treated)  
   ) in 
   tempf([],form) ;; 

let expand gram already_found_prefixes (older_heads,current_head) = 
   let terminals = get_terminals gram in 
   let updated_heads = str_insert current_head older_heads in 
   let (BG productions) = gram.core in 
   let temp1 = List.flatten(List.filter_map (fun (Prod(a,b))->
      if a<>current_head then None else Some( elements_having_a_wholly_emptiable_left gram b)) productions) in 
   let candidates =  str_setminus (str_sort temp1) updated_heads in 
   let (new_prefixes1,nonterminal_candidates) = List.partition (fun symb->str_mem symb terminals) candidates in 
   let using_precedent_computations = Image.image (fun symb->
      (symb,Hashtbl.find_opt gram.hashtbl_for_furst_sets symb)) nonterminal_candidates in 
   let (to_be_treated_next,already_treated)  = List.partition (fun (_,opt)->opt=None) 
       using_precedent_computations in 
   let new_prefixes2 = str_fold_merge  (Image.image (fun (_,opt)->Option.get opt) already_treated) in 
   let new_prefixes = str_merge new_prefixes1 new_prefixes2 in 
   (str_merge new_prefixes already_found_prefixes,updated_heads,Image.image fst to_be_treated_next) ;;

let rec iterator gram (already_found_prefixes,to_be_treated) = 
   match to_be_treated with 
   [] -> already_found_prefixes 
   |pair :: other_pairs ->
     let (new_set_of_prefixes,updated_heads,to_be_treated_next) = expand gram already_found_prefixes pair in 
     let new_pairs = Image.image (fun candidate ->(updated_heads,candidate)) to_be_treated_next  in 
     iterator gram (new_set_of_prefixes,new_pairs@other_pairs) ;;
  
let compute_furst_set_naively gram symb= iterator  gram ([],[[],symb]) ;;

let furst_set_for_symbol gram symb = 
  match Hashtbl.find_opt gram.hashtbl_for_furst_sets symb with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_furst_set_naively gram symb in 
   let _ = Hashtbl.add gram.hashtbl_for_furst_sets symb new_answer in 
   new_answer
  ;;

let furst_set_for_form gram form = 
    let symbols = elements_having_a_wholly_emptiable_left gram form in 
    iterator  gram ([],Image.image(fun symb -> ([],symb)) symbols) ;;



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


let compute_naively gram symb =
   let (BG productions) = gram.core in 
   let completions = completions_on_the_right_for_in_productions symb productions in 
   let (empty_completions,nonempty_completions) = List.partition (fun x->x=[]) completions in
   let rightmost_contribution = (if empty_completions=[] then [] else [end_marker]) in 
   str_fold_merge (rightmost_contribution::
   (Image.image (Furst_set.furst_set_for_form gram) nonempty_completions)) ;;

let follow_set gram symb = 
  match Hashtbl.find_opt gram.hashtbl_for_follow_sets symb with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_naively gram symb in 
   let _ = Hashtbl.add gram.hashtbl_for_follow_sets symb new_answer in 
   new_answer
  ;;

end ;;   

end ;;   

let add_new_paths_to_lr0_state = Private.add_new_paths_to_lr0_state ;;

let all_lr0_states = Private.all_lr0_states ;;

let emptiable_nonterminals = Private.Emptiable_nonterminals.all ;;

let follow_set = Private.Follow_set.follow_set ;;

let furst_set = Private.Furst_set.furst_set_for_symbol ;;

let items gram = Lrp_bare_grammar.items gram.core ;; 

let make l= {
   core = BG l ;
   registry = Lrp_registry.default ;
   hashtbl_for_ghettoes = Hashtbl.create 100;
   hashtbl_for_emptiability = Hashtbl.create 100;
   emptiable_nonterminals = None ;
   terminals = None ;
   hashtbl_for_furst_sets = Hashtbl.create 100;
   hashtbl_for_follow_sets = Hashtbl.create 100;
} ;;



let nonterminals gram = Lrp_bare_grammar.nonterminals gram.core;;

let register_lr0_state = Private.register_lr0_state ;;

let start_symbol gram = Lrp_bare_grammar.start_symbol gram.core ;; 

let starter_lr0_state gram = Lrp_bare_grammar.starter_lr0_state gram.core ;; 
let symbols gram = Lrp_bare_grammar.symbols gram.core ;; 
let terminals = Private.get_terminals;;