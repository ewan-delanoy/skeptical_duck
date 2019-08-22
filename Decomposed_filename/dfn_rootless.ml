(*

#use"Decomposed_filename/dfn_rootless.ml";;


*)


let of_line line = Dfn_join.string_to_rootless line;;


let to_line (Dfn_rootless_t.J(s,m,e))=
   (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m)^(Dfa_ending.connectable_to_modulename e);;
