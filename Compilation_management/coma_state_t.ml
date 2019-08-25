
(* 

#use"Makefile_makers/coma_state_t.ml";;

*)

type t={
     root : Dfa_root_t.t;
     dir_for_backup : Dfa_root_t.t;
     push_after_backup : bool;
     modules : Dfa_module_t.t Small_array.t ;
     subdir_for_module : Dfa_subdirectory_t.t Small_array.t ;
     principal_ending_for_module : Dfa_ending_t.t Small_array.t ;
     mli_presence_for_module : bool Small_array.t ;
     principal_mt_for_module : string Small_array.t ;
     mli_mt_for_module : string Small_array.t ;
     needed_libs_for_module : Ocaml_library.t list Small_array.t ;
     direct_fathers_for_module : Dfa_module_t.t list Small_array.t;
     ancestors_for_module : Dfa_module_t.t list Small_array.t ; 
     needed_dirs_for_module : Dfa_subdirectory_t.t list Small_array.t;
     product_up_to_date_for_module : (Dfa_module_t.t * bool) list;
     directories : Dfa_subdirectory_t.t list;
     printer_equipped_types : (Dfn_endingless_t.t*bool) list;
};;


   

           