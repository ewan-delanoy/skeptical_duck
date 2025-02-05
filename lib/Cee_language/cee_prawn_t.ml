(*

#use"lib/Cee_language/cee_prawn_t.ml";;


A prawn is an atom in a shadow algebra for a file.

Its index is relative to the list of all possible prawns for 
this file.

*)


type  t = P of int list ;; 