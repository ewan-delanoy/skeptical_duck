(*

#use"lib/Mad_world/Filewatching/mw_poly.ml";;

*)


module Private = struct 

module Crobj = struct 
let salt = "Mw_poly_t." ;;
let label_for_type_name                          = salt ^ "type_name" ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;
let label_for_ignored_files                      = salt ^ "ignored_files" ;;
let label_for_ignored_subdirectories             = salt ^ "ignored_subdirectories" ;;
let label_for_last_compilation_result_for_module = salt ^ "last_compilation_result_for_module" ;;
let label_for_root                               = salt ^ "root" ;;
let label_for_small_details_in_files             = salt ^ "small_details_in_files" ;;
let label_for_subdirs_for_archived_mlx_files     = salt ^ "subdirs_for_archived_mlx_files" ;;
let label_for_watched_files                      = salt ^ "watched_files" ;;

let of_concrete_object ccrt_obj = 
 let g=Concrete_object.get_record ccrt_obj in 
 {
   Mw_poly_t.type_name = Crobj_converter.string_of_concrete_object (g label_for_type_name) ;
   dir_for_backup = Dfa_root.of_concrete_object (g label_for_dir_for_backup)  ;
   encoding_protected_files = Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files)  ;
   github_url = Crobj_converter.string_of_concrete_object (g label_for_github_url)  ;
   gitpush_after_backup = Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup)  ;
   ignored_files = Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object (g label_for_ignored_files)  ;
   ignored_subdirectories = Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_ignored_subdirectories)  ;
   index_for_caching = (Mw_indexer.make_full_instance ()) ;
   last_compilation_result_for_module = Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object (g label_for_last_compilation_result_for_module)  ;
   root = Dfa_root.of_concrete_object (g label_for_root)  ;
   small_details_in_files = Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Mw_file_small_details.of_concrete_object (g label_for_small_details_in_files)  ;
   subdirs_for_archived_mlx_files = Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_subdirs_for_archived_mlx_files)  ;
   watched_files = Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Crobj_converter.string_of_concrete_object (g label_for_watched_files)  ;
} ;;

let to_concrete_object fw = 
 let items =  
 [
     label_for_type_name, Crobj_converter.string_to_concrete_object fw.Mw_poly_t.type_name ;
     label_for_dir_for_backup, Dfa_root.to_concrete_object fw.Mw_poly_t.dir_for_backup ;
     label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object fw.Mw_poly_t.encoding_protected_files ;
     label_for_github_url, Crobj_converter.string_to_concrete_object fw.Mw_poly_t.github_url ;
     label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object fw.Mw_poly_t.gitpush_after_backup ;
     label_for_ignored_files, Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object fw.Mw_poly_t.ignored_files ;
     label_for_ignored_subdirectories, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object fw.Mw_poly_t.ignored_subdirectories ;
     label_for_last_compilation_result_for_module, Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object fw.Mw_poly_t.last_compilation_result_for_module ;
     label_for_root, Dfa_root.to_concrete_object fw.Mw_poly_t.root ;
     label_for_small_details_in_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Mw_file_small_details.to_concrete_object fw.Mw_poly_t.small_details_in_files ;
     label_for_subdirs_for_archived_mlx_files, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object fw.Mw_poly_t.subdirs_for_archived_mlx_files ;
     label_for_watched_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Crobj_converter.string_to_concrete_object fw.Mw_poly_t.watched_files ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 




module Extender = struct 

let file_watcher_to_fw_with_archives fw ~subdirs_for_archived_mlx_files:v1_archives_subdirs = {
   fw with 
   Mw_poly_t.type_name = "Mw_with_archives" ;
   subdirs_for_archived_mlx_files = v1_archives_subdirs ;
} ;;
let fw_configuration_to_file_watcher fw ~watched_files:v1_files = {
   fw with 
   Mw_poly_t.type_name = "File_watcher" ;
   watched_files = v1_files ;
} ;;
let fw_with_archives_to_fw_with_small_details fw ~small_details_in_files:v1_small_details = {
   fw with 
   Mw_poly_t.type_name = "Mw_with_small_details" ;
   small_details_in_files = v1_small_details ;
} ;;
let fw_with_batch_compilation_to_fw_with_githubbing fw ~dir_for_backup:v1_backup_dir ~gitpush_after_backup:v2_gab ~github_url:v3_url ~encoding_protected_files:v4_protected_pairs = {
   fw with 
   Mw_poly_t.type_name = "Mw_with_githubbing" ;
   dir_for_backup = v1_backup_dir ;
   gitpush_after_backup = v2_gab ;
   github_url = v3_url ;
   encoding_protected_files = v4_protected_pairs ;
} ;;
let fw_with_dependencies_to_fw_with_batch_compilation fw ~last_compilation_result_for_module:v1_compilation_results = {
   fw with 
   Mw_poly_t.type_name = "Mw_with_batch_compilation" ;
   last_compilation_result_for_module = v1_compilation_results ;
} ;;
let fw_with_small_details_to_fw_with_dependencies fw ~index_for_caching:v1_cache_idx = {
   fw with 
   Mw_poly_t.type_name = "Mw_with_dependencies" ;
   index_for_caching = v1_cache_idx ;
} ;;
end;;

module Parent = struct 
let designated_parents = [
    "Mw_with_archives" , "File_watcher" ;
    "Mw_with_small_details" , "Mw_with_archives" ;
    "Mw_with_dependencies" , "Mw_with_small_details" ;
    "Mw_with_batch_compilation" , "Mw_with_dependencies" ;
    "Mw_with_githubbing" , "Mw_with_batch_compilation" ;
] ;;

exception No_designated_parent of string ;; 
exception Set_parent_exn of string ;; 

let get_parent_name fw = 
 let name = fw.Mw_poly_t.type_name in 
 match List.assoc_opt name designated_parents with 
  Some(answer) ->answer
 |None -> raise (No_designated_parent(name)) ;;

let sp_for_fw_with_archives child new_parent = 
 Extender.file_watcher_to_fw_with_archives new_parent 
   ~subdirs_for_archived_mlx_files:(child.Mw_poly_t.subdirs_for_archived_mlx_files)
 ;;
let sp_for_fw_with_small_details child new_parent = 
 Extender.fw_with_archives_to_fw_with_small_details new_parent 
   ~small_details_in_files:(child.Mw_poly_t.small_details_in_files)
 ;;
let sp_for_fw_with_dependencies child new_parent = 
 Extender.fw_with_small_details_to_fw_with_dependencies new_parent 
   ~index_for_caching:(child.Mw_poly_t.index_for_caching)
 ;;
let sp_for_fw_with_batch_compilation child new_parent = 
 Extender.fw_with_dependencies_to_fw_with_batch_compilation new_parent 
   ~last_compilation_result_for_module:(child.Mw_poly_t.last_compilation_result_for_module)
 ;;
let sp_for_fw_with_githubbing child new_parent = 
 Extender.fw_with_batch_compilation_to_fw_with_githubbing new_parent 
   ~dir_for_backup:(child.Mw_poly_t.dir_for_backup)
   ~gitpush_after_backup:(child.Mw_poly_t.gitpush_after_backup)
   ~github_url:(child.Mw_poly_t.github_url)
   ~encoding_protected_files:(child.Mw_poly_t.encoding_protected_files)
 ;;

let set ~child ~new_parent = 
 let name = child.Mw_poly_t.type_name in 
 match List.assoc_opt name [
   "Mw_with_archives" , sp_for_fw_with_archives child new_parent ;
   "Mw_with_small_details" , sp_for_fw_with_small_details child new_parent ;
   "Mw_with_dependencies" , sp_for_fw_with_dependencies child new_parent ;
   "Mw_with_batch_compilation" , sp_for_fw_with_batch_compilation child new_parent ;
   "Mw_with_githubbing" , sp_for_fw_with_githubbing child new_parent ;
 ] with 
  Some(answer) ->answer
 |None -> raise (Set_parent_exn(name)) ;;

let get child = 
 let parent_name = get_parent_name child in 
 { child with Mw_poly_t.type_name = parent_name } ;;

end;; 




let origin = {
   Mw_poly_t.type_name = "" ;
   dir_for_backup = Dfa_root.of_line "dummy" ;
   encoding_protected_files = [] ;
   github_url = "" ;
   gitpush_after_backup = false ;
   ignored_files = [] ;
   ignored_subdirectories = [] ;
   index_for_caching = (Mw_indexer.make_full_instance ()) ;
   last_compilation_result_for_module = [] ;
   root = Dfa_root.of_line "dummy" ;
   small_details_in_files = [] ;
   subdirs_for_archived_mlx_files = [] ;
   watched_files = [] ;
} ;;

module Type_information = struct 
let fields_for_instances = [
"Mw_configuration" , ["root";"ignored_subdirectories";"ignored_files"];
"Mw_file_watcher" , ["root";"ignored_subdirectories";"ignored_files";"watched_files"];
"Mw_with_archives" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files"];
"Mw_with_small_details" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files"];
"Mw_with_dependencies" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching"];
"Mw_with_batch_compilation" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching";"last_compilation_result_for_module"];
"Mw_with_githubbing" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching";"last_compilation_result_for_module";"dir_for_backup";"gitpush_after_backup";"github_url";"encoding_protected_files"];
"Mw_github_configuration" , ["root";"dir_for_backup";"gitpush_after_backup";"github_url";"encoding_protected_files"]
] ;;

exception Get_fields_exn of string ;;

let get_fields_from_name tname = 
   try List.assoc tname fields_for_instances with
    _ -> raise(Get_fields_exn(tname)) ;;

let get_fields fw = get_fields_from_name fw.Mw_poly_t.type_name ;; 

let data_for_fields = [
   "dir_for_backup" , "Dfa_root_t.t";
   "encoding_protected_files" , "(Dfn_rootless_t.t * Dfn_rootless_t.t) list";
   "github_url" , "string";
   "gitpush_after_backup" , "bool";
   "ignored_files" , "Dfn_rootless_t.t list";
   "ignored_subdirectories" , "Dfa_subdirectory_t.t list";
   "index_for_caching" , "Mw_instance_index_t.t * Mw_state_index_t.t";
   "last_compilation_result_for_module" , "(Dfa_module_t.t * bool) list";
   "root" , "Dfa_root_t.t";
   "small_details_in_files" , "(Dfn_rootless_t.t * Mw_file_small_details_t.t) list";
   "subdirs_for_archived_mlx_files" , "Dfa_subdirectory_t.t list";
   "watched_files" , "(Dfn_rootless_t.t * string) list"
] ;;

exception Get_field_data_exn of string ;;

let get_field_data field_name = 
   try (field_name,List.assoc field_name data_for_fields) with
    _ -> raise(Get_field_data_exn(field_name)) ;;

let element_in_show_fields (fd_name,fd_type) = (String.make 3 ' ') ^ fd_name ^ " : " ^ fd_type ;;

let show_fields fw = 
 let fields = get_fields fw in 
 let data = Image.image get_field_data fields in 
 let msg = " "^ (fw.Mw_poly_t.type_name) ^ " : {\n" ^ 
 (String.concat "\n" (Image.image element_in_show_fields data))
 ^ " \n } " in
 print_string ("\n\n"^msg^"\n\n");;

let check_inclusion inst1 inst2 = 
   let cinst1 = String.capitalize_ascii inst1
   and cinst2 = String.capitalize_ascii inst2 in
   let fields1 = get_fields_from_name cinst1
   and fields2 = get_fields_from_name cinst2 in
   let nonincluded_fields = List.filter (fun x->not(List.mem x fields2)) fields1 in
   let n = List.length nonincluded_fields in
   if n = 0
   then true
   else
   let (left,right)= (if n>1 then ("s"," are ") else (""," is ") ) in
   let fld_list = String.concat " , " nonincluded_fields in
   let msg = " The field " ^ left ^ fld_list ^ right ^ " in " ^
   cinst1 ^ "but not in " ^
   cinst2 ^ "." in
   let _ = print_string ("\n\n"^msg^"\n\n") in
   false ;;
exception Check_inclusion_exn ;;

let check_inclusion_forcefully inst1 inst2 = 
   if (not(check_inclusion inst1 inst2)) then raise(Check_inclusion_exn) ;;

end;; 





end;; 




let construct_fw_configuration ~root:v1_r ~ignored_subdirectories:v2_ign_subdirs ~ignored_files:v3_ign_files = {
   Private.origin with 
   Mw_poly_t.type_name = "Mw_configuration" ;
   root = v1_r ;
   ignored_subdirectories = v2_ign_subdirs ;
   ignored_files = v3_ign_files ;
} ;;
let construct_github_configuration ~root:v1_r ~dir_for_backup:v2_backup_dir ~gitpush_after_backup:v3_gab ~github_url:v4_url ~encoding_protected_files:v5_protected_pairs = {
   Private.origin with 
   Mw_poly_t.type_name = "Github_configuration" ;
   root = v1_r ;
   dir_for_backup = v2_backup_dir ;
   gitpush_after_backup = v3_gab ;
   github_url = v4_url ;
   encoding_protected_files = v5_protected_pairs ;
} ;;
let dir_for_backup x = x.Mw_poly_t.dir_for_backup ;;
let encoding_protected_files x = x.Mw_poly_t.encoding_protected_files ;;
let extend_file_watcher_to_fw_with_archives  = Private.Extender.file_watcher_to_fw_with_archives ;;
let extend_fw_configuration_to_file_watcher  = Private.Extender.fw_configuration_to_file_watcher ;;
let extend_fw_with_archives_to_fw_with_small_details  = Private.Extender.fw_with_archives_to_fw_with_small_details ;;
let extend_fw_with_batch_compilation_to_fw_with_githubbing  = Private.Extender.fw_with_batch_compilation_to_fw_with_githubbing ;;
let extend_fw_with_dependencies_to_fw_with_batch_compilation  = Private.Extender.fw_with_dependencies_to_fw_with_batch_compilation ;;
let extend_fw_with_small_details_to_fw_with_dependencies  = Private.Extender.fw_with_small_details_to_fw_with_dependencies ;;
let github_url x = x.Mw_poly_t.github_url ;;
let gitpush_after_backup x = x.Mw_poly_t.gitpush_after_backup ;;
let ignored_files x = x.Mw_poly_t.ignored_files ;;
let ignored_subdirectories x = x.Mw_poly_t.ignored_subdirectories ;;
let index_for_caching x = x.Mw_poly_t.index_for_caching ;;
let last_compilation_result_for_module x = x.Mw_poly_t.last_compilation_result_for_module ;;
let of_concrete_object = Private.Crobj.of_concrete_object ;;
let parent  = Private.Parent.get ;;
let print_out (fmt:Format.formatter) fw  = Format.fprintf fmt "@[%s@]" ("< "^(fw.Mw_poly_t.type_name)^" >") ;;
let root x = x.Mw_poly_t.root ;;
let set_dir_for_backup x backup_dir = { x with Mw_poly_t.dir_for_backup = backup_dir} ;;
let set_encoding_protected_files x protected_pairs = { x with Mw_poly_t.encoding_protected_files = protected_pairs} ;;
let set_github_url x url = { x with Mw_poly_t.github_url = url} ;;
let set_gitpush_after_backup x gab = { x with Mw_poly_t.gitpush_after_backup = gab} ;;
let set_ignored_files x ign_files = { x with Mw_poly_t.ignored_files = ign_files} ;;
let set_ignored_subdirectories x ign_subdirs = { x with Mw_poly_t.ignored_subdirectories = ign_subdirs} ;;
let set_index_for_caching x cache_idx = { x with Mw_poly_t.index_for_caching = cache_idx} ;;
let set_last_compilation_result_for_module x compilation_results = { x with Mw_poly_t.last_compilation_result_for_module = compilation_results} ;;
let set_parent  = Private.Parent.set ;;
let set_root x r = { x with Mw_poly_t.root = r} ;;
let set_small_details_in_files x small_details = { x with Mw_poly_t.small_details_in_files = small_details} ;;
let set_subdirs_for_archived_mlx_files x archives_subdirs = { x with Mw_poly_t.subdirs_for_archived_mlx_files = archives_subdirs} ;;
let set_watched_files x files = { x with Mw_poly_t.watched_files = files} ;;
let show_fields  = Private.Type_information.show_fields ;;
let small_details_in_files x = x.Mw_poly_t.small_details_in_files ;;
let subdirs_for_archived_mlx_files x = x.Mw_poly_t.subdirs_for_archived_mlx_files ;;
let to_concrete_object = Private.Crobj.to_concrete_object ;;
let to_fw_configuration fw  = 
  let tname = fw.Mw_poly_t.type_name in 
  let _ = Private.Type_information.check_inclusion "mw_configuration" tname in 
   {
   fw with 
   Mw_poly_t.type_name = "Mw_configuration" ;
} ;;
let to_github_configuration fw  = 
  let tname = fw.Mw_poly_t.type_name in 
  let _ = Private.Type_information.check_inclusion "github_configuration" tname in 
   {
   fw with 
   Mw_poly_t.type_name = "Mw_ithub_configuration" ;
} ;;
let watched_files x = x.Mw_poly_t.watched_files ;; 