(*

#use"node_project.ml";;

*)

let compact_replacer = 
  Dfn_rootless.of_line (
  (Dfa_subdirectory.connectable_to_subpath 
   Coma_constant.confidential_data_subdir 
  )^"compact_replacer.txt");;

let config = 
  let home = Sys.getenv "HOME" in 
 {Fw_configuration_t.root =
    Dfa_root_t.R (home^"/Teuliou/Sites/Gwerzher_Leoriou");
   dir_for_backup =
    Dfa_root_t.R (home^"/Teuliou/Sites/Githubbed_gwl");
   gitpush_after_backup = true;
   ignored_subdirectories =
    [
     Dfa_subdirectory_t.SD "images"; 
     Dfa_subdirectory_t.SD "node_modules"
    ];
   ignored_files =
    [Dfn_rootless_t.J (Dfa_subdirectory_t.SD "", Dfa_module_t.M "package-lock",
      Dfa_ending_t.E "json");
     Dfn_rootless_t.J (Dfa_subdirectory_t.SD "", Dfa_module_t.M "deizlevr",
      Dfa_ending_t.E "txt")];
   github_url = "https://github.com/ewan-delanoy/node_app";
   encoding_protected_files = [
     compact_replacer,
   Dfn_rootless_t.J (Dfa_subdirectory_t.SD "models", Dfa_module_t.M "mysql_connection",
       Dfa_ending_t.E "js")];
   } ;;

let watcher_ref = ref (File_watcher.empty_one config);;


let refresh () =
    let diff = Check_ocaml_dircopy.check config in 
    let _ = Reflect_change_in_github.backup config diff None in 
    watcher_ref:=(File_watcher.of_configuration config) ;;

let update opt_msg =
    let old_fw = (!watcher_ref) in 
    let (fw2,_) = File_watcher.z_inspect_and_update old_fw true in 
    let fw3 = File_watcher.reflect_latest_changes_in_github fw2 opt_msg in 
    let _=(watcher_ref:=fw3) in fw3;;
   


