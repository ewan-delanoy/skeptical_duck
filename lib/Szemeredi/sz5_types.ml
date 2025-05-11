(*

#use"lib/Szemeredi/sz5_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz5 is short for "fifth stab at Szemeredi problem".


*)

type width = W of int ;; 

type constraint_t = C of int list list ;;

type point = {
   p_width : width ;
   size : int ;
   extra_constraint : constraint_t;
} ;;