(*

#use"Ocaml_analysis/read_needed_ocaml_files.ml";;

Given a module, first computes all the dependencies needed to
define this module, then reads the dependencies and 
finally the module itself.

*)

let read_needed_ocaml_files hm=
  let cs=(!(Usual_coma_state.main_ref)) in
  let nm=Dfn_endingless.to_module hm in
  let pre_temp2=(Coma_state.ancestors_at_module cs nm)@[nm] in
  let temp2=Image.vorstellung (Coma_state.endingless_at_module cs) pre_temp2 in
  let all_files=Image.vorstellung  (fun hm2->
     Dfn_full.to_absolute_path(Dfn_join.to_ending hm2 Dfa_ending.ml)
  ) temp2 in
   Read_ocaml_files.read_ocaml_files all_files;;
