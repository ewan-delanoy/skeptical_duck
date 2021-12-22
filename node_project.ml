(*

#use"node_project.ml";;

*)

let compact_replacer = 
  Dfn_rootless.of_line (
  (Dfa_subdirectory.connectable_to_subpath 
   Coma_constant.confidential_data_subdir 
  )^"compact_replacer.txt");;

let fw_config = 
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

  let github_config = 
    let home = Sys.getenv "HOME" in 
   {Github_configuration_t.root =
      Dfa_root_t.R (home^"/Teuliou/Sites/Gwerzher_Leoriou");
     dir_for_backup =
      Dfa_root_t.R (home^"/Teuliou/Sites/Githubbed_gwl");
     gitpush_after_backup = true;
     github_url = "https://github.com/ewan-delanoy/node_app";
     encoding_protected_files = [
       compact_replacer,
     Dfn_rootless_t.J (Dfa_subdirectory_t.SD "models", Dfa_module_t.M "mysql_connection",
         Dfa_ending_t.E "js")];
     } ;;

let watcher_ref = ref (File_watcher.empty_one fw_config);;


let refresh () =
    let diff = Check_ocaml_dircopy.check fw_config in 
    let _ = Transmit_change_to_github.backup github_config diff None in 
    watcher_ref:=(File_watcher.of_configuration fw_config) ;;

let update opt_msg =
    let old_fw = (!watcher_ref) in 
    let (fw2,changes) = File_watcher.inspect_and_update old_fw true in 
    let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changes in 
    let _ = Transmit_change_to_github.backup github_config diff opt_msg in 
    let _=(watcher_ref:=fw2) in fw2;;
   


