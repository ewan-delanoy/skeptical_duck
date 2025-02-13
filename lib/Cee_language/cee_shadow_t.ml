(*

#use"lib/Cee_language/cee_shadow_t.ml";;

A shadow is a prawn, with an additional parameter
telling the total number of zones in the file.

*)

type  t = 
   Sh of int * Cee_prawn_t.t ;; 