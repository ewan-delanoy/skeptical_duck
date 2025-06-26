(*

#use"lib/Filewatching/fw_poly.ml";;

*)


exception Get_exn of string ;;

module Private = struct 

let  get_type_name fw = try Option.get ( fw.Fw_poly_t.type_name )  with _ -> raise(Get_exn("type_name")) ;;
let  get_dir_for_backup fw = try Option.get ( fw.Fw_poly_t.dir_for_backup )  with _ -> raise(Get_exn("dir_for_backup")) ;;
let  get_encoding_protected_files fw = try Option.get ( fw.Fw_poly_t.encoding_protected_files )  with _ -> raise(Get_exn("encoding_protected_files")) ;;
let  get_github_url fw = try Option.get ( fw.Fw_poly_t.github_url )  with _ -> raise(Get_exn("github_url")) ;;
let  get_gitpush_after_backup fw = try Option.get ( fw.Fw_poly_t.gitpush_after_backup )  with _ -> raise(Get_exn("gitpush_after_backup")) ;;
let  get_index_for_caching fw = try Option.get ( fw.Fw_poly_t.index_for_caching )  with _ -> raise(Get_exn("index_for_caching")) ;;
let  get_ignored_files fw = try Option.get ( fw.Fw_poly_t.ignored_files )  with _ -> raise(Get_exn("ignored_files")) ;;
let  get_ignored_subdirectories fw = try Option.get (fw.Fw_poly_t.ignored_subdirectories )  with _ -> raise(Get_exn("ignored_subdirectories")) ;;
let  get_last_compilation_result_for_module fw = try Option.get ( fw.Fw_poly_t.last_compilation_result_for_module )  with _ -> raise(Get_exn("last_compilation_result_for_module")) ;;
let  get_root fw = try Option.get ( fw.Fw_poly_t.root )  with _ -> raise(Get_exn("root")) ;;
let  get_small_details_in_files fw = try Option.get ( fw.Fw_poly_t.small_details_in_files )  with _ -> raise(Get_exn("small_details_in_files")) ;;
let  get_subdirs_for_archived_mlx_files fw = try Option.get ( fw.Fw_poly_t.subdirs_for_archived_mlx_files )  with _ -> raise(Get_exn("subdirs_for_archived_mlx_files")) ;;
let  get_watched_files fw = try Option.get ( fw.Fw_poly_t.watched_files )  with _ -> raise(Get_exn("watched_files")) ;;
   


module Crobj = struct 
let salt = "Fw_poly_t." ;;
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
   Fw_poly_t.type_name = Some(Crobj_converter.string_of_concrete_object (g label_for_type_name)) ;
   dir_for_backup = Some(Dfa_root.of_concrete_object (g label_for_dir_for_backup))  ;
   encoding_protected_files = Some(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files))  ;
   github_url = Some(Crobj_converter.string_of_concrete_object (g label_for_github_url))  ;
   gitpush_after_backup = Some(Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup))  ;
   ignored_files = Some(Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object (g label_for_ignored_files))  ;
   ignored_subdirectories = Some(Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_ignored_subdirectories))  ;
   index_for_caching = Some (Fw_indexer.make_full_instance ()) ;
   last_compilation_result_for_module = Some(Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object (g label_for_last_compilation_result_for_module))  ;
   root = Some(Dfa_root.of_concrete_object (g label_for_root))  ;
   small_details_in_files = Some(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Fw_file_small_details.of_concrete_object (g label_for_small_details_in_files))  ;
   subdirs_for_archived_mlx_files = Some(Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_subdirs_for_archived_mlx_files))  ;
   watched_files = Some(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Crobj_converter.string_of_concrete_object (g label_for_watched_files))  ;
} ;;

let to_concrete_object fw = 
 let items =  
 [
   label_for_type_name, Crobj_converter.string_to_concrete_object (get_type_name fw);
   label_for_dir_for_backup, Dfa_root.to_concrete_object ( get_dir_for_backup fw ) ;
   label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object ( get_encoding_protected_files fw ) ;
   label_for_github_url, Crobj_converter.string_to_concrete_object ( get_github_url fw ) ;
   label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object ( get_gitpush_after_backup fw ) ;
   label_for_ignored_files, Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object ( get_ignored_files fw ) ;
   label_for_ignored_subdirectories, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object ( get_ignored_subdirectories fw ) ;
   label_for_last_compilation_result_for_module, Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object ( get_last_compilation_result_for_module fw ) ;
   label_for_root, Dfa_root.to_concrete_object ( get_root fw ) ;
   label_for_small_details_in_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Fw_file_small_details.to_concrete_object ( get_small_details_in_files fw ) ;
   label_for_subdirs_for_archived_mlx_files, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object ( get_subdirs_for_archived_mlx_files fw ) ;
   label_for_watched_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Crobj_converter.string_to_concrete_object ( get_watched_files fw ) ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 



module Extender = struct 

let file_watcher_to_fw_with_archives fw ~subdirs_for_archived_mlx_files:v1_archives_subdirs = {
   fw with 
   Fw_poly_t.type_name = Some"Fw_with_archives" ;
   subdirs_for_archived_mlx_files = Some v1_archives_subdirs ;
} ;;
let fw_configuration_to_file_watcher fw ~watched_files:v1_files = {
   fw with 
   Fw_poly_t.type_name = Some "File_watcher" ;
   watched_files = Some v1_files ;
} ;;
let fw_with_archives_to_fw_with_small_details fw ~small_details_in_files:v1_small_details = {
   fw with 
   Fw_poly_t.type_name = Some "Fw_with_small_details" ;
   small_details_in_files = Some v1_small_details ;
} ;;
let fw_with_batch_compilation_to_fw_with_githubbing fw ~dir_for_backup:v1_backup_dir ~gitpush_after_backup:v2_gab ~github_url:v3_url ~encoding_protected_files:v4_protected_pairs = {
   fw with 
   Fw_poly_t.type_name = Some "Fw_with_githubbing" ;
   dir_for_backup = Some v1_backup_dir ;
   gitpush_after_backup = Some v2_gab ;
   github_url = Some v3_url ;
   encoding_protected_files = Some v4_protected_pairs ;
} ;;
let fw_with_dependencies_to_fw_with_batch_compilation fw ~last_compilation_result_for_module:v1_compilation_results = {
   fw with 
   Fw_poly_t.type_name = Some "Fw_with_batch_compilation" ;
   last_compilation_result_for_module = Some v1_compilation_results ;
} ;;
let fw_with_small_details_to_fw_with_dependencies fw ~index_for_caching:v1_cache_idx = {
   fw with 
   Fw_poly_t.type_name = Some "Fw_with_dependencies" ;
   index_for_caching = Some v1_cache_idx ;
} ;;
end;;

module Parent = struct 
let designated_parents = [
    "Fw_with_archives" , "File_watcher" ;
    "Fw_with_small_details" , "Fw_with_archives" ;
    "Fw_with_dependencies" , "Fw_with_small_details" ;
    "Fw_with_batch_compilation" , "Fw_with_dependencies" ;
    "Fw_with_githubbing" , "Fw_with_batch_compilation" ;
] ;;

exception No_designated_parent of string ;; 
exception Set_parent_exn of string ;; 

let get_parent_name fw = 
 let name = get_type_name fw in 
 match List.assoc_opt name designated_parents with 
  Some(answer) ->answer
 |None -> raise (No_designated_parent(name)) ;;

let sp_for_fw_with_archives child new_parent = 
 Extender.file_watcher_to_fw_with_archives new_parent 
   ~subdirs_for_archived_mlx_files:(get_subdirs_for_archived_mlx_files child)
 ;;
let sp_for_fw_with_small_details child new_parent = 
 Extender.fw_with_archives_to_fw_with_small_details new_parent 
   ~small_details_in_files:(get_small_details_in_files child)
 ;;
let sp_for_fw_with_dependencies child new_parent = 
 Extender.fw_with_small_details_to_fw_with_dependencies new_parent 
   ~index_for_caching:(get_index_for_caching child)
 ;;
let sp_for_fw_with_batch_compilation child new_parent = 
 Extender.fw_with_dependencies_to_fw_with_batch_compilation new_parent 
   ~last_compilation_result_for_module:(get_last_compilation_result_for_module child)
 ;;
let sp_for_fw_with_githubbing child new_parent = 
 Extender.fw_with_batch_compilation_to_fw_with_githubbing new_parent 
   ~dir_for_backup:(get_dir_for_backup child)
   ~gitpush_after_backup:(get_gitpush_after_backup child)
   ~github_url:(get_github_url child)
   ~encoding_protected_files:(get_encoding_protected_files child)
 ;;

let set ~child ~new_parent = 
 let name = get_type_name child in 
 match List.assoc_opt name [
   "Fw_with_archives" , sp_for_fw_with_archives child new_parent ;
   "Fw_with_small_details" , sp_for_fw_with_small_details child new_parent ;
   "Fw_with_dependencies" , sp_for_fw_with_dependencies child new_parent ;
   "Fw_with_batch_compilation" , sp_for_fw_with_batch_compilation child new_parent ;
   "Fw_with_githubbing" , sp_for_fw_with_githubbing child new_parent ;
 ] with 
  Some(answer) ->answer
 |None -> raise (Set_parent_exn(name)) ;;

let get child = 
 let parent_name = get_parent_name child in 
 { child with Fw_poly_t.type_name = Some parent_name } ;;

end;; 




let origin = {
   Fw_poly_t.type_name = Some "" ;
   dir_for_backup = Some(Dfa_root.of_line "dummy") ;
   encoding_protected_files = Some [] ;
   github_url = Some "" ;
   gitpush_after_backup = Some false ;
   ignored_files = Some [] ;
   ignored_subdirectories = Some [] ;
   index_for_caching = Some (Fw_indexer.make_full_instance ()) ;
   last_compilation_result_for_module = Some [] ;
   root = Some (Dfa_root.of_line "dummy") ;
   small_details_in_files = Some [] ;
   subdirs_for_archived_mlx_files = Some [] ;
   watched_files = Some [] ;
} ;;

module Type_information = struct 
let fields_for_instances = [
"Fw_configuration" , ["root";"ignored_subdirectories";"ignored_files"];
"File_watcher" , ["root";"ignored_subdirectories";"ignored_files";"watched_files"];
"Fw_with_archives" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files"];
"Fw_with_small_details" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files"];
"Fw_with_dependencies" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching"];
"Fw_with_batch_compilation" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching";"last_compilation_result_for_module"];
"Fw_with_githubbing" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching";"last_compilation_result_for_module";"dir_for_backup";"gitpush_after_backup";"github_url";"encoding_protected_files"];
"Github_configuration" , ["root";"dir_for_backup";"gitpush_after_backup";"github_url";"encoding_protected_files"]
] ;;

exception Get_fields_exn of string ;;

let get_fields_from_name tname = 
   try List.assoc tname fields_for_instances with
    _ -> raise(Get_fields_exn(tname)) ;;

let get_fields fw = get_fields_from_name (get_type_name fw);; 

let data_for_fields = [
   "dir_for_backup" , "Dfa_root_t.t";
   "encoding_protected_files" , "(Dfn_rootless_t.t * Dfn_rootless_t.t) list";
   "github_url" , "string";
   "gitpush_after_backup" , "bool";
   "ignored_files" , "Dfn_rootless_t.t list";
   "ignored_subdirectories" , "Dfa_subdirectory_t.t list";
   "index_for_caching" , "Fw_instance_index_t.t * Fw_state_index_t.t";
   "last_compilation_result_for_module" , "(Dfa_module_t.t * bool) list";
   "root" , "Dfa_root_t.t";
   "small_details_in_files" , "(Dfn_rootless_t.t * Fw_file_small_details_t.t) list";
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
 let msg = " "^ (get_type_name fw) ^ " : {\n" ^ 
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
   Fw_poly_t.type_name = Some "Fw_configuration" ;
   root = Some v1_r ;
   ignored_subdirectories = Some v2_ign_subdirs ;
   ignored_files = Some v3_ign_files ;
} ;;
let construct_github_configuration ~root:v1_r ~dir_for_backup:v2_backup_dir ~gitpush_after_backup:v3_gab ~github_url:v4_url ~encoding_protected_files:v5_protected_pairs = {
   Private.origin with 
   Fw_poly_t.type_name = Some "Github_configuration" ;
   root = Some v1_r ;
   dir_for_backup = Some v2_backup_dir ;
   gitpush_after_backup = Some v3_gab ;
   github_url = Some v4_url ;
   encoding_protected_files = Some v5_protected_pairs ;
} ;;
let dir_for_backup x = Private.get_dir_for_backup x;;
let encoding_protected_files x = Private.get_encoding_protected_files x;;
let extend_file_watcher_to_fw_with_archives  = Private.Extender.file_watcher_to_fw_with_archives ;;
let extend_fw_configuration_to_file_watcher  = Private.Extender.fw_configuration_to_file_watcher ;;
let extend_fw_with_archives_to_fw_with_small_details  = Private.Extender.fw_with_archives_to_fw_with_small_details ;;
let extend_fw_with_batch_compilation_to_fw_with_githubbing  = Private.Extender.fw_with_batch_compilation_to_fw_with_githubbing ;;
let extend_fw_with_dependencies_to_fw_with_batch_compilation  = Private.Extender.fw_with_dependencies_to_fw_with_batch_compilation ;;
let extend_fw_with_small_details_to_fw_with_dependencies  = Private.Extender.fw_with_small_details_to_fw_with_dependencies ;;
let github_url x = Private.get_github_url x;;
let gitpush_after_backup x = Private.get_gitpush_after_backup x;;
let ignored_files x = Private.get_ignored_files x;;
let ignored_subdirectories x = Private.get_ignored_subdirectories x;;
let index_for_caching x = Private.get_index_for_caching x;;
let last_compilation_result_for_module x = Private.get_last_compilation_result_for_module x;;
let of_concrete_object = Private.Crobj.of_concrete_object ;;
let origin = Private.origin ;;
let parent  = Private.Parent.get ;;
let print_out (fmt:Format.formatter) fw  = Format.fprintf fmt "@[%s@]" ("< "^(Private.get_type_name fw)^" >") ;;
let root x = Private.get_root x;;
let set_dir_for_backup x backup_dir = { x with Fw_poly_t.dir_for_backup = Some backup_dir} ;;
let set_encoding_protected_files x protected_pairs = { x with Fw_poly_t.encoding_protected_files = Some protected_pairs} ;;
let set_github_url x url = { x with Fw_poly_t.github_url = Some url} ;;
let set_gitpush_after_backup x gab = { x with Fw_poly_t.gitpush_after_backup = Some gab} ;;
let set_ignored_files x ign_files = { x with Fw_poly_t.ignored_files = Some ign_files} ;;
let set_ignored_subdirectories x ign_subdirs = { x with Fw_poly_t.ignored_subdirectories = Some ign_subdirs} ;;
let set_index_for_caching x cache_idx = { x with Fw_poly_t.index_for_caching = Some cache_idx} ;;
let set_last_compilation_result_for_module x compilation_results = { x with Fw_poly_t.last_compilation_result_for_module = Some compilation_results} ;;
let set_parent  = Private.Parent.set ;;
let set_root x r = { x with Fw_poly_t.root = Some r} ;;
let set_small_details_in_files x small_details = { x with Fw_poly_t.small_details_in_files = Some small_details} ;;
let set_subdirs_for_archived_mlx_files x archives_subdirs = { x with Fw_poly_t.subdirs_for_archived_mlx_files = Some archives_subdirs} ;;
let set_watched_files x files = { x with Fw_poly_t.watched_files = Some files} ;;
let show_fields  = Private.Type_information.show_fields ;;
let small_details_in_files x = Private.get_small_details_in_files x;;
let subdirs_for_archived_mlx_files x = Private.get_subdirs_for_archived_mlx_files x ;;
let to_concrete_object = Private.Crobj.to_concrete_object ;;
let to_fw_configuration fw  = 
  let tname = Private.get_type_name fw in 
  let _ = Private.Type_information.check_inclusion "fw_configuration" tname in 
   {
   fw with 
   Fw_poly_t.type_name = Some "Fw_configuration" ;
} ;;
let to_github_configuration fw  = 
  let tname = Private.get_type_name fw in 
  let _ = Private.Type_information.check_inclusion "github_configuration" tname in 
   {
   fw with 
   Fw_poly_t.type_name = Some "Github_configuration" ;
} ;;
let watched_files x = Private.get_watched_files x;;