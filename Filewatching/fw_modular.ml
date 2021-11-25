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
      
      let full_tripartition fw =
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         canonical_tripartition fw all_files ;;

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

      let inspect_and_update old_fw =
         let answer = File_watcher.inspect_and_update old_fw ~verbose:false in 
         let (new_fw,changed_files) = answer in 
         let (a_files,u_files,nc_files) = canonical_tripartition new_fw changed_files in 
         let announce = (fun trail files ->
            Strung.announce 
                     ~trailer: trail
                        ~printer:Dfn_rootless.to_line ~items:files 
                        ~separator: "\n"
         ) in
         let _ = (
            announce "The following noncompilables have been changed :" nc_files;
            announce "The following archived files have been changed :" a_files;
            announce "The following usual compilables have been changed :" u_files;
         ) in
         (new_fw,changed_files,(a_files,u_files)) ;;   

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
         
         let rename_module_on_filename_level old_fw (old_module,new_module) = 
            let all_files = Image.image fst (File_watcher.watched_files old_fw) in 
            let (a_files,u_files,nc_files) = canonical_tripartition old_fw all_files in 
            let acolytes = List.filter (
                   fun rl -> (Dfn_rootless.to_module rl) = old_module 
            ) u_files in
            let replacements = Image.image (fun old_rl->
                   (old_rl,Dfn_rootless.rename_module_as (old_module,new_module) old_rl )) acolytes in
            let new_fw = File_watcher.rename_files  old_fw replacements in 
            (new_fw,replacements) ;;     
               
         let rename_module_on_content_level old_fw (old_module,new_module) files_to_be_rewritten =
            let apply = (fun fw files->
               File_watcher.apply_text_transformation_on_some_files fw 
               (Look_for_module_names.change_module_name_in_ml_ocamlcode  
               old_module new_module) files
            ) in 
            let (all_a_files,_,_) = full_tripartition old_fw  in 
            let (fw1,changed_u_files) = apply old_fw files_to_be_rewritten in 
            let (fw2,changed_a_files) = apply fw1 all_a_files in 
            let announce = (fun trail files ->
               Strung.announce 
                        ~trailer: trail
                           ~printer:Dfn_rootless.to_line ~items:files 
                           ~separator: "\n"
            ) in
            let _ = (
               announce "The following archived files have their content affected by the renaming :" changed_a_files;
            ) in   
            (fw2,changed_u_files,changed_a_files) ;;  
                  
         let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten)=
            let (fw2,file_renamings) = rename_module_on_filename_level fw (old_module,new_module) in 
            let (fw3,u_files,a_files) = rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
            (fw3,file_renamings,u_files,a_files) ;;   
   
      
let replace_string fw (replacee,replacer) =
   File_watcher.apply_text_transformation_on_all_files fw (
      Replace_inside.replace_inside_string (replacee,replacer)
   ) ;;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path 
        (File_watcher.root fw)  in 
    let fw2= File_watcher.update_some_files fw [rootless] in 
    let (fw3,changed_files)=replace_string fw2 (replacee,replacer) in 
    let fw4 = File_watcher.Automatic.reflect_changes_in_diff 
        fw3 (rootless::changed_files) in         
    (fw4,(rootless::changed_files));;

      let usual_compilable_files fw  =
         let all_files = Image.image fst (File_watcher.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         u_files ;;        

      end ;;

let compute_all_small_details = Private.compute_all_small_details ;;

let compute_small_details_on_one_file = Private.compute_small_details_on_one_file ;;

let forget_modules = Private.forget_modules ;;

let inspect_and_update = Private.inspect_and_update ;;

let noncompilable_files = Private.noncompilable_files ;;

let partition_for_singles = Private.canonical_tripartition ;; 

let relocate_module_to = Private.relocate_module_to ;;

let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;

let usual_compilable_files = Private.usual_compilable_files ;;

