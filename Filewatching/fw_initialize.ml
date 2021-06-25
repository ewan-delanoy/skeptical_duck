(*

#use"Filewatching/fw_initialize.ml";;

*)

(*
module Private = struct 

  let first_init config =
     let the_root = config.Fw_configuration_t.root in 
     let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
     let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir config.Fw_configuration_t.ignored_subdirectories in 
     let list2 = Option.filter_and_unpack(
       fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
                _->None 
     ) list1 in
     let list3 = 
     List.filter (Fw_configuration.test_for_admissibility config) list2 in 
     let (compilables,noncompilables ) =List.partition (
        fun rootless -> List.mem
            (Dfn_rootless.to_ending rootless) Dfa_ending.endings_for_compilable_files
     ) list3 in 
     let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in  
     let (archived_compilables,usual_compilables) =List.partition (
        fun rootless -> 
          List.exists (Dfn_rootless.is_in rootless) archived_subdirs
     ) compilables in 
     (archived_compilables,usual_compilables,noncompilables);;
  
  end ;;
  
  let compute_and_store_modification_times config 
         (ac_to_be_watched,uc_to_be_watched,noncompilables) =
      let the_root = config.Fw_configuration_t.root in  
      let compute_info=( fun path->
        let s_root = Dfa_root.connectable_to_subpath the_root
        and s_path=Dfn_rootless.to_line path in 
        let file = s_root^s_path in 
        let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
        (path,mtime)
     ) in 
       {
         Fw_wrapper_t.configuration = config;
         Fw_wrapper_t.archived_compilable_files = Image.image compute_info ac_to_be_watched;
         usual_compilable_files = Image.image compute_info uc_to_be_watched;
         noncompilable_files = Image.image compute_info noncompilables;
         last_noticed_changes = Dircopy_diff.empty_one;
       };;
  
  let init config = 
     let (ac_to_be_watched,uc_to_be_watched,noncompilables) = Private.first_init config in 
     compute_and_store_modification_times config (ac_to_be_watched,uc_to_be_watched,noncompilables);;
*)  