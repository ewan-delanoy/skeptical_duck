(*

#use"Filewatching/fw_nonmodular_wrapper.ml";;

*)

exception Register_rootless_path_exn of string list;;

module Private = struct

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
     let s_root = Dfa_root.connectable_to_subpath (Fw_nonmodular_wrapper_automatic.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     mtime file;;


let recompute_all_info fw path =
     let s_root = Dfa_root.connectable_to_subpath (Fw_nonmodular_wrapper_automatic.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     (path,mtime file);;

let update_in_list_of_pairs fw  to_be_updated pairs  =
   Image.image (
      fun pair -> 
        let (rootless,mtime)=pair in 
        if List.mem rootless to_be_updated 
        then recompute_all_info fw rootless 
        else pair
   ) pairs;;

let update_some_files fw (w_files,sw_files) = {
    fw with 
      Fw_nonmodular_wrapper_t.watched_files = update_in_list_of_pairs fw w_files 
      (fw.Fw_nonmodular_wrapper_t.watched_files) ;
} ;;


let remove_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Fw_nonmodular_wrapper_automatic.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
    let fw2 ={
      fw with 
      Fw_nonmodular_wrapper_t.watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_nonmodular_wrapper_t.watched_files)  ;
   } in 
   Fw_nonmodular_wrapper_automatic.reflect_destructions_in_diff fw2 rootless_paths ;;


let register_rootless_paths fw rootless_paths= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_nonmodular_wrapper_automatic.root fw) in
   let bad_paths = Option.filter_and_unpack (
     fun rp-> let s_full_path = s_root^(Dfn_rootless.to_line rp)  in 
     if not(Sys.file_exists s_full_path)
     then Some(s_full_path)
     else None
   ) rootless_paths in 
   if bad_paths<>[]
   then raise(Register_rootless_path_exn(bad_paths))
   else 
   let fw2=  {
      fw with 
      Fw_nonmodular_wrapper_t.watched_files =  
        (fw.Fw_nonmodular_wrapper_t.watched_files)@
          (Image.image (recompute_all_info fw) rootless_paths)  ;
    }  in 
    Fw_nonmodular_wrapper_automatic.reflect_creations_in_diff fw2 rootless_paths;;

let deal_with_initial_comment_if_needed fw rless =
   if (Dfn_rootless.to_ending rless)<> Dfa_ending.ml 
   then ()
   else
      let root = Fw_nonmodular_wrapper_automatic.root fw in 
      let full = Dfn_join.root_to_rootless root rless in 
      let ap = Dfn_full.to_absolute_path full in 
      Put_use_directive_in_initial_comment.put_usual root ap
   ;;    





let helper_during_string_replacement fw (old_string,new_string) accu old_list=
    let new_list =Image.image (
       fun pair->
         let (old_path,_)=pair in 
         if not(Supstring.contains (Fw_nonmodular_wrapper_automatic.get_content fw old_path) old_string)
         then pair 
         else 
            let ap = Dfn_full.to_absolute_path (Dfn_join.root_to_rootless (Fw_nonmodular_wrapper_automatic.root fw) old_path) in 
            let _=(
             Replace_inside.replace_inside_file (old_string,new_string) ap;
             accu:=old_path::(!accu)
            ) in 
            recompute_all_info fw old_path 
         ) old_list in 
    (new_list,List.rev(!accu));;

let replace_string fw (old_string,new_string)=
    let ref_for_changed_files=ref[]  in 
    let (new_files,changed_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_changed_files fw.Fw_nonmodular_wrapper_t.watched_files in 
    let new_fw ={
       fw with
       Fw_nonmodular_wrapper_t.watched_files = new_files ;
    }  in 
    (new_fw,changed_files);;         
       
let rename_value_inside_rootless fw (old_name,new_name) preceding_files rootless_path=
   let full_path = Dfn_join.root_to_rootless (Fw_nonmodular_wrapper_automatic.root fw) rootless_path in 
   let absolute_path=Dfn_full.to_absolute_path  full_path in 
   let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name absolute_path in 
   let new_watched_files =Image.image (
       fun pair->
         let (path,_)=pair in 
         if path = rootless_path 
         then recompute_all_info fw path
         else pair 
   ) (fw.Fw_nonmodular_wrapper_t.watched_files) in 
   {
      fw with 
       Fw_nonmodular_wrapper_t.watched_files = new_watched_files
   };;  

let ref_for_subdirectory_renaming = ref [];;

let remember_during_subdirectory_renaming pair =
   (ref_for_subdirectory_renaming := pair :: (!ref_for_subdirectory_renaming) );;

let helper1_during_subdirectory_renaming fw (old_subdir,new_subdir) pair=
   let (rootless_path,_)=pair in 
   match Dfn_rootless.soak (old_subdir,new_subdir) rootless_path with 
   Some(new_rootless_path) -> 
        let _=(
           remember_during_subdirectory_renaming (rootless_path,new_rootless_path)
        ) in 
        recompute_all_info fw new_rootless_path
   |None -> pair;;

let helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) l_pairs =
     let _=(ref_for_subdirectory_renaming := []) in 
     let comp=Image.image (helper1_during_subdirectory_renaming fw (old_subdir,new_subdir)) l_pairs in 
     (comp,List.rev(!ref_for_subdirectory_renaming));;

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Fw_nonmodular_wrapper_automatic.root fw)  in 
    let s_old_subdir = Dfa_subdirectory.without_trailing_slash old_subdir 
    and s_new_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
    let old_full_path = s_root^s_old_subdir 
    and new_full_path = s_root^s_new_subdir in 
    let cmd=" mv "^old_full_path^" "^new_full_path in 
        let _=Unix_command.hardcore_uc cmd in 
    let (files,reps)   =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_nonmodular_wrapper_t.watched_files) in 
   let fw2 = {
      fw with
      Fw_nonmodular_wrapper_t.watched_files = files  ;
   } in 
   Fw_nonmodular_wrapper_automatic.reflect_replacements_in_diff fw2 reps;;   

let helper1_during_inspection fw accu pair=
   let (rootless_path,old_mtime)=pair in 
   let new_mtime = recompute_mtime fw rootless_path in 
   if new_mtime <> old_mtime
   then let _=(accu:=rootless_path::(!accu)) in 
        recompute_all_info fw rootless_path
   else pair;;

let helper2_during_inspection fw accu l_pairs =
   let new_l_pairs = Image.image (helper1_during_inspection fw accu) l_pairs in 
   (new_l_pairs,List.rev(!accu));;

let inspect_and_update fw = 
    let ref_for_files=ref[]  in 
    let (new_files,changed_files)=
        helper2_during_inspection fw ref_for_files fw.Fw_nonmodular_wrapper_t.watched_files in 
    let fw2 ={
       fw with
       Fw_nonmodular_wrapper_t.watched_files         = new_files ;
    }  in 
    let new_fw = Fw_nonmodular_wrapper_automatic.reflect_changes_in_diff fw2 changed_files in 
    (new_fw,changed_files);;         

let replace_string_in_list_of_pairs fw (replacee,replacer) l=
   let changed_ones=ref[]  in 
   let new_l=Image.image (
      fun pair ->
        let (rootless,mtime)=pair in 
        let content = Fw_nonmodular_wrapper_automatic.get_content fw rootless in 
        if Substring.is_a_substring_of replacee content 
        then let _=(changed_ones:= rootless:: (!changed_ones)) in 
             let s_root = Dfa_root.connectable_to_subpath (Fw_nonmodular_wrapper_automatic.root fw) 
             and s_path=Dfn_rootless.to_line rootless in 
             let file = s_root^s_path in  
             let ap=Absolute_path.of_string file in 
             let _=(Replace_inside.replace_inside_file (replacee,replacer) ap;
                   ) in 
             recompute_all_info fw rootless 
        else pair     
   ) l in 
   (new_l,List.rev(!changed_ones));;

let replace_string fw (replacee,replacer) =
   let rep = replace_string_in_list_of_pairs fw (replacee,replacer)  in 
   let (new_files,changed_files) =  rep fw.Fw_nonmodular_wrapper_t.watched_files  in 
   let fw2 ={
       fw with
       Fw_nonmodular_wrapper_t.watched_files = new_files;
   } in 
   let fw3 = Fw_nonmodular_wrapper_automatic.reflect_changes_in_diff fw2 changed_files in 
   (fw3,changed_files);;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path (Fw_nonmodular_wrapper_automatic.root fw)  in 
    let fw2= update_some_files fw ([rootless],[]) in 
    let (fw3,changed_files)=replace_string fw2 (replacee,replacer) in 
    let fw4 =  Fw_nonmodular_wrapper_automatic.reflect_changes_in_diff fw3 (rootless::changed_files) in         
    (fw4,(rootless::changed_files));;

   module Initialization = struct 

      module Private = struct 
        
        let first_init config =
           let the_root = config.Fw_configuration_t.root in 
           let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
           let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir config.Fw_configuration_t.ignored_subdirectories in 
           let list2 = Option.filter_and_unpack(
             fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
                      _->None 
           ) list1 in
           List.filter (Fw_configuration.test_for_admissibility config) list2 ;;
        
        end ;;
        
        let compute_and_store_modification_times config to_be_watched =
            let the_root = config.Fw_configuration_t.root in  
            let compute_info=( fun path->
              let s_root = Dfa_root.connectable_to_subpath the_root
              and s_path=Dfn_rootless.to_line path in 
              let file = s_root^s_path in 
              let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
              (path,mtime)
           ) in 
             {
               Fw_nonmodular_wrapper_t.configuration = config;
               watched_files = Image.image compute_info to_be_watched;
               last_noticed_changes = Dircopy_diff.empty_one;
             };;
        
        let init config = 
           let to_be_watched = Private.first_init config in 
           compute_and_store_modification_times config to_be_watched ;;
        
        
    
    end ;;  


end;;


let empty_one config= {
   Fw_nonmodular_wrapper_t.configuration = config;
   watched_files = [];
   last_noticed_changes = Dircopy_diff.empty_one;
};; 

let initialize = Private.Initialization.init ;;

let inspect_and_update = Private.inspect_and_update;;



let reflect_latest_changes_in_github fw opt_msg=
   let config = fw.Fw_nonmodular_wrapper_t.configuration in 
   let _= Reflect_change_in_github.backup config fw.Fw_nonmodular_wrapper_t.last_noticed_changes opt_msg in 
   {fw with Fw_nonmodular_wrapper_t.last_noticed_changes = Dircopy_diff.empty_one} ;; 


let register_rootless_paths = Private.register_rootless_paths;;

let remove_files = Private.remove_files;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;


