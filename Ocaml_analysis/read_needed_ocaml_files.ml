(*

#use"Ocaml_analysis/read_needed_ocaml_files.ml";;

Given a module, first computes all the dependencies needed to
define this module, then reads the dependencies and 
finally the module itself.

*)

let read_needed_ocaml_files hm=
  let cs=(!(Usual_coma_state.main_ref)) in
  let nm=Dfn_with_ending_removed.naked_module hm in
  let idx1=Coma_state.find_module_index cs nm in
  let pre_temp2=(Coma_state.ancestors_at_idx cs idx1)@[nm] in
  let temp2=Image.image (Coma_state.hm_from_nm cs) pre_temp2 in
  let all_files=Image.image  (fun hm2->
     Dfn_full_path.to_path(Dfn_full_path.join hm2 Dfa_ending.Ml)
  ) temp2 in
   Read_ocaml_files.read_ocaml_files all_files;;
