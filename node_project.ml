(*

#use"node_project.ml";;

*)

let config = 
 {Fw_configuration_t.root =
    Dfa_root_t.R "/Users/ewandelanoy/Teuliou/Sites/Gwerzher_Leoriou";
   dir_for_backup =
    Dfa_root_t.R "/Users/ewandelanoy/Teuliou/Sites/Githubbed_gwl";
   gitpush_after_backup = true;
   ignored_subdirectories =
    [Dfa_subdirectory_t.SD "Traou_all";
     Dfa_subdirectory_t.SD "images"; 
     Dfa_subdirectory_t.SD "node_modules"
    ];
   ignored_files =
    [Dfn_rootless_t.J (Dfa_subdirectory_t.SD "", Dfa_module_t.M "package-lock",
      Dfa_ending_t.E "json");
     Dfn_rootless_t.J (Dfa_subdirectory_t.SD "", Dfa_module_t.M "deizlevr",
      Dfa_ending_t.E "txt")];
   github_url = "https://github.com/ewan-delanoy/node_app";
   confidential_files = []} ;;

let watcher_ref = ref (Fw_wrapper.empty_one config);;