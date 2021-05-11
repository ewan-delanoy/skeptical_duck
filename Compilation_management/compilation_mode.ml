(* 

#use"Makefile_makers/compilation_mode.ml";;

*)

exception Ending_for_last_module_exn ;; 
exception Ending_for_nonlast_module_exn ;; 

let workspace = function 
   Compilation_mode_t.Usual->Coma_constant.usual_build_subdir
                     |Debug->Coma_constant.debug_build_subdir
                     |Executable->Coma_constant.exec_build_subdir;;

let ending_for_last_module = function 
   Compilation_mode_t.Usual-> raise(Ending_for_last_module_exn)
                     |Debug->".cmo"
                     |Executable->".ml";;

let ending_for_nonlast_module = function 
   Compilation_mode_t.Usual-> raise(Ending_for_nonlast_module_exn)
                     |Debug->".cmo"
                     |Executable->".cmx";;                     

let executioner = function 
   Compilation_mode_t.Usual->"ocamlc -bin-annot "
                     |Debug->"ocamlc -g "
                     |Executable->"ocamlopt ";;

let ending_for_final_product = function 
   Compilation_mode_t.Usual->""
                     |Debug->".ocaml_debuggable "
                     |Executable->".ocaml_executable ";;   
           