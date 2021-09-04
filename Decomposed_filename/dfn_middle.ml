(*

#use"Decomposed_filename/dfn_middle.ml";;

*)

let rename_module (m1,m2) middle =
  let (Dfn_middle_t.J(s,m)) = middle in 
  if m = m1 
  then Dfn_middle_t.J(s,m2)
  else middle;; 
let rename_subdirectory (s1,s2) middle =
  let (Dfn_middle_t.J(s,m)) = middle in 
  if s = s1 
  then Dfn_middle_t.J(s2,m)
  else middle;;       
let to_line (Dfn_middle_t.J(s,m)) = (Dfa_subdirectory.connectable_to_subpath s)^ (Dfa_module.to_line m);;
let to_module (Dfn_middle_t.J(s,m))=m;;