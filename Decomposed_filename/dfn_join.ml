(*

#use"Decomposed_filename/dfn_join.ml";;

*)


(* let root_to_short r (Dfn_short_t.J(s,m))=Dfn_endingless_t.J(r,s,m);; *)
let root_to_rootless r (Dfn_rootless_t.J(s,m,e))=Dfn_full_t.J(r,s,m,e);;
let subdirectory_to s (Dfn_short_t.J(m,e))=(Dfn_rootless_t.J(s,m,e));;  
let to_ending (Dfn_endingless_t.J(r,s,m)) e = Dfn_full_t.J(r,s,m,e);;

