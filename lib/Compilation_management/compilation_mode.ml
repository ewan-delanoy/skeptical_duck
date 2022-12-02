(* 

#use"lib/Compilation_management/compilation_mode.ml";;

*)

exception Ending_for_last_module_exn ;; 
exception Ending_for_nonlast_module_exn ;; 

let workspace = function 
    Compilation_mode_t.Usual->Coma_constant.usual_build_subdir
   |Compilation_mode_t.Debug->Coma_constant.debug_build_subdir
   |Compilation_mode_t.Executable->Coma_constant.exec_build_subdir;;

let ending_for_last_module = function 
    Compilation_mode_t.Usual-> raise(Ending_for_last_module_exn)
   |Compilation_mode_t.Debug->".cmo"
   |Compilation_mode_t.Executable->".ml";;

let ending_for_nonlast_module = function 
   Compilation_mode_t.Usual-> raise(Ending_for_nonlast_module_exn)
   |Compilation_mode_t.Debug->".cmo"
   |Compilation_mode_t.Executable->".cmx";;
                     
let executioner = function 
   Compilation_mode_t.Usual->"ocamlc -bin-annot "
   |Compilation_mode_t.Debug->"ocamlc -g "
   |Compilation_mode_t.Executable->"ocamlopt ";;

let ending_for_final_product = function 
   Compilation_mode_t.Usual->""
   |Compilation_mode_t.Debug->".ocaml_debuggable "
   |Compilation_mode_t.Executable->".ocaml_executable ";;