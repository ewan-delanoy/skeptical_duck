(*

#use"Filewatching/fw_modular.ml";;

*)











































































































































































































































































































































































































































































































 module Private = struct

      let canonical_tripartition fw all_files =
            let (c_files,nc_files) = List.partition (
                fun rl->
                  Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
            )  all_files in 
            let config = File_watcher.configuration fw in
            let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in 
            let is_archived = (fun rl->List.exists (Dfn_rootless.is_in rl) archived_subdirs) in 
            let (a_files,u_files) = List.partition is_archived  c_files in 
            (a_files,u_files,nc_files) ;;     
      
      let compilable_files fw =
         Option.filter_and_unpack (
                      fun (rl,_)->
                     if Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
                     then Some rl 
                     else None   
         )  (File_watcher.watched_files fw) ;;
      
      let compute_small_details_on_one_file fw rl=
         let root = Fw_configuration.root (fw.File_watcher_t.configuration) in 
         let s_ap = Dfn_common.recompose_potential_absolute_path root rl in 
         let ap = Absolute_path.of_string s_ap in 
         Fw_file_small_details.compute ap ;;

      let compute_all_small_details fw =
         let c_files = compilable_files fw in 
         Image.image (
            fun rl ->
               (rl,compute_small_details_on_one_file fw rl)
         ) c_files ;;
           

      let forget_modules fw mod_names =
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         let the_files = List.filter (
                 fun path-> List.mem (Dfn_rootless.to_module path) mod_names 
         ) u_files in    
         File_watcher.remove_files fw the_files;;      

      let noncompilable_files fw  =
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         let (_,_,nc_files) = canonical_tripartition fw all_files in 
         nc_files ;;
      
      let relocate_module_to fw mod_name new_subdir=
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         let the_files = List.filter (
                  fun path-> (Dfn_rootless.to_module path)=mod_name 
         ) u_files in 
         File_watcher.relocate_files_to fw the_files new_subdir ;;
         
      let rename_module_on_filename_level fw (old_module,new_module) =
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         let acolytes = List.filter (
                fun rl -> (Dfn_rootless.to_module rl) = old_module 
         ) u_files in
         let replacements = Image.image (fun old_rl->
                (old_rl,Dfn_rootless.rename_module_as (old_module,new_module) old_rl )) acolytes in
         let s_root = Dfa_root.connectable_to_subpath (File_watcher.root fw) in 
         let l_cmds = Image.image (
                fun (old_rl,new_rl) ->
                  let s_old_ap=s_root^(Dfn_rootless.to_line old_rl) 
                  and s_new_ap=s_root^(Dfn_rootless.to_line new_rl) in    
                  "mv "^s_old_ap^" "^s_new_ap
            ) replacements  in
         let _ =Unix_command.conditional_multiple_uc l_cmds in  
         File_watcher.rename_files  fw replacements  ;;  
            
      let rename_module_on_content_level fw (old_module,new_module) files_to_be_rewritten =
         File_watcher.apply_text_transformation_on_some_files fw
            (Look_for_module_names.change_module_name_in_ml_ocamlcode  
            old_module new_module)  files_to_be_rewritten  ;;  
               
      let rename_module_on_filename_level_and_in_files fw old_module new_module files_to_be_rewritten=
         let fw2=rename_module_on_filename_level fw (old_module,new_module) in 
         let fw3=rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
         fw3;;
         
      let restrict fw smaller_list_of_modules =  
         let old_watched_files = File_watcher.watched_files fw in 
         {
         fw with
         File_watcher_t.watched_files = List.filter (
             fun (rl,_) -> List.mem (Dfn_rootless.to_module rl) smaller_list_of_modules
         ) old_watched_files;
         last_noticed_changes = Dircopy_diff.empty_one;
       };;
  

      let usual_compilable_files fw  =
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         u_files ;;        

      end ;;

let compute_all_small_details = Private.compute_all_small_details ;;

let compute_small_details_on_one_file = Private.compute_small_details_on_one_file ;;

let forget_modules = Private.forget_modules ;;

let noncompilable_files = Private.noncompilable_files ;;

let partition_for_singles = Private.canonical_tripartition ;; 

let relocate_module_to = Private.relocate_module_to ;;

let usual_compilable_files = Private.usual_compilable_files ;;
































































