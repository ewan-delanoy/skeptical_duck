(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_stream.ml";;

*)

open Jvng_types ;;

let consume old_stream k = 
  {
   cursor  = old_stream.cursor + k ;
   remaining_list = Jvsp_token_types_list.long_tail k old_stream.remaining_list ;
} ;;
