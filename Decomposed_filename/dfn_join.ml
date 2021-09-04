(*

#use"Decomposed_filename/dfn_join.ml";;

*)

let middle_to_ending (Dfn_middle_t.J(s,m)) e= (Dfn_rootless_t.J(s,m,e));;
let root_to_middle r (Dfn_middle_t.J(s,m))=Dfn_endingless_t.J(r,s,m);; 
let root_to_rootless r (Dfn_rootless_t.J(s,m,e))=Dfn_full_t.J(r,s,m,e);;
let root_to_subdirectory (Dfa_root_t.R(r)) (Dfa_subdirectory_t.SD(s))=Dfa_root_t.R(r^"/"^s);;
let subdirectory_to_module s m=(Dfn_middle_t.J(s,m));;  
let subdirectory_to_short s (Dfn_short_t.J(m,e))=(Dfn_rootless_t.J(s,m,e));;  
let to_ending (Dfn_endingless_t.J(r,s,m)) e = Dfn_full_t.J(r,s,m,e);;

