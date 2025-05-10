(*

#use"lib/Szemeredi/sz5_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz5 is short for "fifth stab at Szemeredi problem".

*)

type width = Sz5_types.width = W of int ;; 

type constraint_t = Sz5_types.constraint_t = C of int list list ;;