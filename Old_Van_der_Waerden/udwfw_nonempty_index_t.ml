(*

#use"Old_Van_der_Waerden/udwfw_nonempty_index_t.ml";;

*)

type t = 
   Part of int * int  
  |Helper of int * int 
  |Solution of int * int;;

  