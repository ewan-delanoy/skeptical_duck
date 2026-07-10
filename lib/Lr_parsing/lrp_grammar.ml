(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

*)

open Lrp_types ;;


module Private = struct 

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




end ;;   

let add_new_paths_to_lr0_state = Private.add_new_paths_to_lr0_state ;;

let all_lr0_states = Private.all_lr0_states ;;

let make l= {
   core = BG l ;
   registry = Lrp_registry.default ;
   hashtbl_for_ghettoes = Hashtbl.create 100;
} ;;


let items gram = Lrp_bare_grammar.items gram.core ;; 
let nonterminals gram = Lrp_bare_grammar.nonterminals gram.core;;

let register_lr0_state = Private.register_lr0_state ;;

let start_symbol gram = Lrp_bare_grammar.start_symbol gram.core ;; 

let starter_lr0_state gram = Lrp_bare_grammar.starter_lr0_state gram.core ;; 
let symbols gram = Lrp_bare_grammar.symbols gram.core ;; 
let terminals gram = Lrp_bare_grammar.terminals gram.core;;