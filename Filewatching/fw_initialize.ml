(*

#use"Filewatching/fw_initialize.ml";;

*)

let init config =
   let the_root = config.Fw_configuration_t.root in 
   let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
   let list1 = More_unix.complete_ls_with_nondirectories_only the_dir in 
   let list2 = Option.filter_and_unpack(
     fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
              _->None 
   ) list1 in
   let (specials,nonspecials) = List.partition (
      fun rootless -> List.mem rootless config.Fw_configuration_t.special_git_saved_files 
   ) list2 in  
   let nonspecials_to_be_watched1 = List.filter (
      fun rootless ->  (List.mem (Dfn_rootless.to_ending rootless)
         config.Fw_configuration_t.allowed_endings )
         &&
         (
            not(List.mem (Dfn_rootless.to_subdirectory rootless)
             config.Fw_configuration_t.git_ignored_subdirectories
            )
         )
   ) nonspecials in 
   let the_cleaner =config.Fw_configuration_t.final_cleaner in 
   let nonspecials_to_be_watched = Fw_final_cleaner.clean the_cleaner  nonspecials_to_be_watched1 in 
   let compute_info=( fun path->
      let s_root = Dfa_root.connectable_to_subpath the_root
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     (path,string_of_float((Unix.stat file).Unix.st_mtime),Io.read_whole_file(Absolute_path.of_string file))
   ) in 
   let w_files = Image.image compute_info nonspecials_to_be_watched 
   and sw_files = Image.image compute_info specials in 
   {
      Fw_wrapper_t.configuration = config;
      Fw_wrapper_t.watched_files = w_files;
       special_watched_files = sw_files;
   }
   ;;