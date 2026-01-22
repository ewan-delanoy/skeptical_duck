(*

#use"lib/Cee_language/cee_prawn_t.ml";;


The prawn of a file is the set of "accepted" (after preprocessing the
conditional directives) zones 
without conditional directives in a C file.
The integers in the int list parameter are the indices of the zones.


*)


type  t = P of int list ;; 