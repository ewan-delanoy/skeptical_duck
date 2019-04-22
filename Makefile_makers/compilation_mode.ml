
(* 

#use"Makefile_makers/compilation_mode.ml";;

*)


let workspace = function 
   Compilation_mode_t.Usual->Coma_constant.build_subdir
                     |Debug->Coma_constant.debug_build_subdir
                     |Executable->Coma_constant.exec_build_subdir;;

let ending_for_element_module = function 
   Compilation_mode_t.Usual->".cmo"
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
           