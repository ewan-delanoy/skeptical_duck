(*

#use"Filewatching/fw_initialize.ml";;

*)

let first_init config =
   let the_root = config.Fw_configuration_t.root in 
   let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
   let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir config.Fw_configuration_t.git_ignored_subdirectories in 
   let list2 = Option.filter_and_unpack(
     fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
              _->None 
   ) list1 in
   let (specials,nonspecials) = List.partition (
      fun rootless -> (List.mem rootless config.Fw_configuration_t.special_git_saved_files)
          || ((Dfn_rootless.to_ending rootless)=Dfa_ending.of_line "txt") 
   ) list2 in  
   let nonspecials_to_be_watched1 = List.filter (
      fun rootless ->  (List.mem (Dfn_rootless.to_ending rootless)
         config.Fw_configuration_t.allowed_endings )
   ) nonspecials in 
   let nonspecials_to_be_watched = 
   List.filter (fun x->
         (not(List.mem x config.Fw_configuration_t.ignored_files)) 
       ) nonspecials_to_be_watched1 in 
   (nonspecials_to_be_watched,specials);;

let second_init config (nonspecials_to_be_watched,specials) =
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
       Fw_wrapper_t.watched_files = Image.image compute_info nonspecials_to_be_watched;
       special_watched_files = Image.image compute_info specials;
     };;

let init config = 
   let (nonspecials_to_be_watched,specials) = first_init config in 
   second_init config (nonspecials_to_be_watched,specials);;

   