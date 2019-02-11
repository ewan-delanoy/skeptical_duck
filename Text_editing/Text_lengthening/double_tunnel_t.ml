(*

#use "Text_editing/Text_lengthening/double_tunnel_t.ml";;

*)

type t={
   mutable size : int ; (* redundant but convenient *)
   incoming : (string option) array;
   outcoming : (string option) array; 
};;