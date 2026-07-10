(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_types.ml";;

*)

type production = Prod of string * string list ;;

type bare_grammar = BG of production list ;;

type item = Item of string * (string list) ;;

type lr0_state = St of item list ;;

type registered_lr0_state = RSt of int * (item list) ;;

type registry_for_lr0_states = Rg of ( lr0_state * ((string list) list) ) list ;;

type grammar = {
   core : bare_grammar ;
   registry : registry_for_lr0_states ;
} ;;