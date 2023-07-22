(*

#use"lib/Szemeredi/sz3_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type extension_data = int list ;; 

type solution = int list ;; 

type mold = M of (solution list) * extension_data ;;

type point = P of finite_int_set * width ;; 

type medium_handle = 
      Mh_import  
    | Mh_cumulative of int 
    | Mh_fork of int * int *int  ;;   