(*

#use"Filewatching/fw_file_small_details_t.ml";;


*)

type t ={
  used_modules : Dfa_module_t.t list ;
  used_libraries : Ocaml_library_t.t list ;
  has_printer : bool ;
  modification_time : string ;
};;
