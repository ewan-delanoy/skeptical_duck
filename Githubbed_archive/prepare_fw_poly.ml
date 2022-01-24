(*

#use "Githubbed_archive/prepare_fw_poly.ml";;

*)

open Needed_values ;;


module Polymorphic_ocaml_record_t = struct 

   type field_t = {
      field_name : string ;
      field_type : string ;
      var_name : string ;
   } ;;
   
   type instance_t = {
      fields : string list ;
   } ;;
   
   type t = {
      main_name : string ;
      fields : field_t list ;
      instances : instance_t list ;
      type_signature_file : Absolute_path.t ;
      implementation_file : Absolute_path.t ;
   } ;;
   
   end ;;
   
   
   let pair_for_field (porf:Polymorphic_ocaml_record_t.field_t) =
     (String.make 3 ' ')^
     porf.Polymorphic_ocaml_record_t.field_name^" : "^
     porf.Polymorphic_ocaml_record_t.field_type^" ;" ;;
   
   let initial_comment_in_type_signature_file por =
       let ap = por.Polymorphic_ocaml_record_t.type_signature_file 
       and root = Coma_big_constant.This_World.root in 
       let s_ap=Absolute_path.to_string ap in 
       let s_cdir=Dfa_root.connectable_to_subpath root in 
       let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
       "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;
   
   let text_for_type_signature_file (por:Polymorphic_ocaml_record_t.t) = 
     let pairs = 
       ((String.make 3 ' ')^"place_in_hierarchy : string ;")::
       (Image.image pair_for_field por.Polymorphic_ocaml_record_t.fields) in 
     (initial_comment_in_type_signature_file por)^
     "type "^(por.Polymorphic_ocaml_record_t.main_name)^" = { \n"^ 
     (String.concat "\n" pairs) ^ 
     "\n} ;;" ;;
   
   let write_to_type_signature_file (por:Polymorphic_ocaml_record_t.t) = 
       let text = text_for_type_signature_file por 
       and file = por.Polymorphic_ocaml_record_t.type_signature_file in 
       Io.overwrite_with file text ;;
   
   let field_list_constructor l = Image.image (
      fun (a,b,c) -> {
       Polymorphic_ocaml_record_t.field_name = a ;
       field_type = b ;
       var_name =c ;
    }
   ) l;;
   
   let fields_for_fw_configuration = field_list_constructor [
     "root","Dfa_root_t.t","r";
     "ignored_subdirectories","Dfa_subdirectory_t.t list","ign_subdirs";
     "ignored_files","Dfn_rootless_t.t list","ign_files";
   ] ;; 
   
   let fields_for_file_watcher = field_list_constructor [
     "watched_files", "(Dfn_rootless_t.t * string) list","files";
   ] ;; 
   
   let fields_for_fw_with_archives = field_list_constructor [
     "subdirs_for_archived_mlx_files","Dfa_subdirectory_t.t list","archives_subdirs";
   ] ;; 
   
   let fields_for_fw_with_small_details = field_list_constructor [
     "small_details_in_files","(Dfn_rootless_t.t * Fw_file_small_details_t.t) list","small_details";
   ] ;; 
   
   let fields_for_fw_with_dependencies = field_list_constructor [
     "index_for_caching", "Fw_instance_index_t.t * Fw_state_index_t.t", "cache_idx";
   ] ;; 
   
   let fields_for_fw_with_batch_compilation = field_list_constructor [
     "last_compilation_result_for_module","(Dfa_module_t.t * bool) list","compilation_results";
   ] ;; 
   
   let fields_for_fw_with_githubbing = field_list_constructor [
     "dir_for_backup","Dfa_root_t.t","backup_dir";
     "gitpush_after_backup","bool","gab";
     "github_url","string","url";
     "encoding_protected_files","(Dfn_rootless_t.t * Dfn_rootless_t.t) list","protected_pairs";
   ] ;; 
   
   let all_fields = List.flatten [
     fields_for_fw_configuration ;
     fields_for_file_watcher ;
     fields_for_fw_with_archives ;
     fields_for_fw_with_small_details ;
     fields_for_fw_with_dependencies ;
     fields_for_fw_with_batch_compilation ;
     fields_for_fw_with_githubbing ;
   ] ;;
   
   
   
   let example = 
      let home = Sys.getenv "HOME" in 
      let file_there = (fun s-> 
        Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/Ordinary/Filewatching/"^s^".ml")) in 
     {
       Polymorphic_ocaml_record_t.main_name = "t" ;
       fields = all_fields ;
       instances = [] ;
       type_signature_file = (file_there "fw_poly_t") ;
       implementation_file = (file_there "fw_poly") ;
    } ;;
   
   let act () = write_to_type_signature_file example ;;

