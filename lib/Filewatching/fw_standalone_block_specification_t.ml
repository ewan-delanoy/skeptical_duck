(*

#use"lib/Filewatching/fw_standalone_block_specification_t.ml";;

*)

type t =
  By_modules of Dfa_module_t.t list 
  |By_subdirectories of Dfa_subdirectory_t.t list ;;