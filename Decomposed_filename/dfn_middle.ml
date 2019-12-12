(*

#use"Decomposed_filename/dfn_middle.ml";;

*)


let to_line (Dfn_middle_t.J(s,m)) = (Dfa_subdirectory.connectable_to_subpath s)^ (Dfa_module.to_line m);;
let to_module (Dfn_middle_t.J(s,m))=m;;