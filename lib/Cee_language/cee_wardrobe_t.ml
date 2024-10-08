(*

#use"lib/Cee_language/cee_wardrobe_t.ml";;

*)

(* 
The int parameter in the uple is the inclusion index
The string parameter is the name of the
(included or includer) file, with the extension removed
*)

type  t = Wr of ((int *string) * Cee_shadow_t.t) list ;; 