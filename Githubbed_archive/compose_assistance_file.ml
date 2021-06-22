(*

#use"Githubbed_archive/compose_assistance_file.ml";;

*)

open Needed_values;;

let g1=(Needed_values.abo "usual_coma_state")@["Compilation_management/usual_coma_state"];;
let g2=Image.image (
  fun old_s->
   let s= Cull_string.after_rightmost old_s '/' in 
   let full_elt = Dfn_join.to_ending (hmx s) Dfa_ending.ml in 
   Dfn_full.to_absolute_path full_elt 
) g1;;
(*
This can be useful when the modifications to the system are extreme
let old_g2 = g2 ;;
let g2 = Image.image (
   fun ap -> 
    let old_s_ap = Absolute_path.to_string ap in 
    let new_s_ap = Replace_inside.replace_inside_string 
               ("Idaho","Ordinary") old_s_ap in 
    Absolute_path.of_string  new_s_ap              
)  old_g2 ;;
*)

let g3=Modularize.modularize_several "assistance_" g2;;

let g4=Replace_inside.replace_several_inside_string
  [
    "let githubbing=true;;",
    "let githubbing=false;;"
  ] g3;;

let prologue=String.concat "\n"
["(*"; ""; "#use\"Githubbed_archive/assistance.ml\";;"; "";
   "In an emergency situation, open a fresh terminal, load this ";
   "file with the above command and call ";
   "Assistance_usual_coma_state.refresh() )";
   "";"#load\"unix.cma\";;";"#load\"str.cma\";;";"#use\"Ordinary/Githubbed_archive/assistance.ml\";;";
   ""; "*)"; ""; "";""];;

let g5=prologue^g4;;

let ap_for_assistance=Absolute_path.of_string
  ("Githubbed_archive/assistance.ml");;

let act ()=Io.overwrite_with
  ap_for_assistance g5;;

