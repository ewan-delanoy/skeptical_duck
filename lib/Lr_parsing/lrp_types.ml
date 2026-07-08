(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_types.ml";;

*)

type production = Prod of string * string list ;;

type grammar = G of production list ;;

