
(* 

#use"Makefile_makers/compilation_mode.ml";;

*)


let workspace = function 
   Compilation_mode_t.Usual->Coma_constant.build_subdir
                     |Debug->Coma_constant.debug_build_subdir
                     |Executable->Coma_constant.exec_build_subdir;;

let ocamlc_option = function 
   Compilation_mode_t.Usual->""
                     |Debug->" -g "
                     |Executable->"";;


let ending_for_element_module = function 
   Compilation_mode_t.Usual->"cmo"
                     |Debug->"cmo"
                     |Executable->"cmx";;

   
           