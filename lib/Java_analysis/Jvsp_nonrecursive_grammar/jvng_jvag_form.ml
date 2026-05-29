(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_form.ml";;

*)

open Jvng_jvag_types ;;

let order = (
   (fun form1 form2 ->Total_ordering.standard form1 form2): 
     form Total_ordering_t.t ) ;; 