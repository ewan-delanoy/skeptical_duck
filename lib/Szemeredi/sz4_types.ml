(*

#use"lib/Szemeredi/sz4_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz4 is short for "fourth stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type fan = F of int list list ;; 

