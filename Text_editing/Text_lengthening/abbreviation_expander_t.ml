(*

#use "Text_editing/Text_lengthening/abbreviation_expander_t.ml";;

*)

type t={
   mutable worker : Text_lengthener_t.t ;
      prooduction : Double_tunnel_t.t 
};;