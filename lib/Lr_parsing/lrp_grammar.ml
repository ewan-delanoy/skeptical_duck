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

end ;;   

let add_new_paths_to_lr0_state = Private.add_new_paths_to_lr0_state ;;

let make l= {
   core = BG l ;
   registry = Lrp_registry.default ;
   hahstbl_for_ghettoes = Hashtbl.create 100;
} ;;

let initialize gram = 
   let starter_lr0_state = Lrp_bare_grammar.starter_lr0_state gram.core in
   let _ = Private.register_lr0_state gram starter_lr0_state in 
   add_new_paths_to_lr0_state gram starter_lr0_state [[]]
;;


let items gram = Lrp_bare_grammar.items gram.core ;; 
let nonterminals gram = Lrp_bare_grammar.nonterminals gram.core;;

let register_lr0_state = Private.register_lr0_state ;;

let start_symbol gram = Lrp_bare_grammar.start_symbol gram.core ;; 

let starter_lr0_state gram = Lrp_bare_grammar.starter_lr0_state gram.core ;; 
let symbols gram = Lrp_bare_grammar.start_symbol gram.core ;; 
let terminals gram = Lrp_bare_grammar.terminals gram.core;;