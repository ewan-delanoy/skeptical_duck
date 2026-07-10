(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

*)

open Lrp_types ;;

let make l= {
   core = BG l ;
   registry = Lrp_registry.default ;
} ;;

let nonterminals gram = Lrp_bare_grammar.nonterminals gram.core;;
let start_symbol gram = Lrp_bare_grammar.start_symbol gram.core ;; 

let starter_lr0_state gram = Lrp_bare_grammar.starter_lr0_state gram.core ;; 
let symbols gram = Lrp_bare_grammar.start_symbol gram.core ;; 
let terminals gram = Lrp_bare_grammar.terminals gram.core;;