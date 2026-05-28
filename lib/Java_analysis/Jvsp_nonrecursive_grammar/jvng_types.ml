(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_types.ml";;

*)

type name_with_ancestry = NWA of string * (string list) ;;     

type ancestry_manager = AM of string * ((string list) list) ;;