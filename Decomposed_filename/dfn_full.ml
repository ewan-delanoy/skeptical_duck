(*

#use"Decomposed_filename/dfn_full.ml";;

*)


let to_subdirectory  (Dfn_full_t.J(r,s,m,e))=s;;
let to_module  (Dfn_full_t.J(r,s,m,e))=m;;
let to_ending (Dfn_full_t.J(r,s,m,e))=e;;

let to_rootless (Dfn_full_t.J(r,s,m,e))=(Dfn_rootless_t.J(s,m,e));; 
let to_endingless (Dfn_full_t.J(r,s,m,e))=(Dfn_endingless_t.J(r,s,m));; 

   
let to_rootless_line (Dfn_full_t.J(r,s,m,e))=
    (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m)^(Dfa_ending.connectable_to_modulename e);;

let to_line (Dfn_full_t.J(r,s,m,e))=
   (Dfa_root.connectable_to_subpath r)^
   (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m)^(Dfa_ending.connectable_to_modulename e);;

let to_absolute_path mlx=Absolute_path.of_string(to_line mlx);;  

let from_absolute_path_with_root ap dir=
  let rless = Dfn_common.decompose_absolute_path_using_root ap dir in 
  Dfn_join.root_to_rootless dir rless;;

  
let relocate (Dfn_full_t.J(r,old_subdir,m,e)) new_subdir=
  (Dfn_full_t.J(r,new_subdir,m,e));;  
 


