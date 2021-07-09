(*

#use"Van_der_Waerden/Width_up_to_four/vdw_nonempty_index_t.ml";;

*)

type t = 
   Part of int * int  
  |Helper of int * int 
  |Solution of int * int;;

  