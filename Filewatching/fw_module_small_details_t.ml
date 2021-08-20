(*

#use"Filewatching/fw_module_small_details_t.ml";;


*)

type t ={
  used_modules : Dfa_module_t.t list ;
  used_libraries : Ocaml_library_t.t list ;
  has_printer : bool ;
  subdirectory : Dfa_subdirectory_t.t ;
  principal_ending : Dfa_ocaml_ending_t.t ;
  mli_present : bool ;
  principal_modification_time : string ;
  mli_modification_time : string option ;
};;

