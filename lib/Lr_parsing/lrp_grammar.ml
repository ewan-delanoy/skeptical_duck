(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

*)

open Lrp_types ;;

let make l= {
   core = BG l ;
   registry = Lrp_registry.default ;
} ;;