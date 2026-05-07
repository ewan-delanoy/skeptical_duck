(*

#use"lib/naive_parser_t.ml";;

*)

type 'a t = NP of (string -> int -> ('a *int) option) ;;  