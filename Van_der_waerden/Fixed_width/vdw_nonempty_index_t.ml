(*

#use"Van_der_Waerden/Fixed_width/vdw_nonempty_index_t.ml";;

*)

type t = 
   Part of int * int  
  |Helper of int * int 
  |Solution of int * int;;

  