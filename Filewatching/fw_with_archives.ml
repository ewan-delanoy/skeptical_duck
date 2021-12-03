(*

#use"Filewatching/fw_with_archives.ml";;

*)

module Private = struct

   let parent fw = fw.Fw_with_archives_t.parent ;;
   
   (* Inherited methods *)

   let configuration fw = File_watcher.configuration (parent fw) ;;
   let get_content fw rl = File_watcher.get_content (parent fw) rl ;;
   let get_mtime fw rl = File_watcher.get_mtime (parent fw) rl ;;
   let get_mtime_or_zero_if_file_is_nonregistered fw rl = File_watcher.get_mtime_or_zero_if_file_is_nonregistered (parent fw) rl ;;
   let last_noticed_changes fw = File_watcher.last_noticed_changes (parent fw);;
   let root fw = File_watcher.root (parent fw) ;;
   let update_parent fw new_parent = {fw with Fw_with_archives_t.parent = new_parent} ;;
   let watched_files fw = File_watcher.watched_files (parent fw) ;;
   
   (* End of inherited methods *)  
   (* Inherited constructors *)

   let constructor par opt_subdirs= 
      let subdirs = (match opt_subdirs with (Some l)->l |None -> [Coma_constant.githubbed_archive_subdir]) in    
      {
         Fw_with_archives_t.parent = par;
         subdirs_for_archived_mlx_files = subdirs;

      };; 
   let empty_one config = constructor(File_watcher.empty_one config) None ;;
   let of_configuration config = constructor(File_watcher.of_configuration config) None ;;
   let of_configuration_and_list config l = constructor (File_watcher.of_configuration_and_list config l) None;;
   let overwrite_file_if_it_exists fw rl new_content = 
      let old_parent = parent fw in 
      let (new_parent,file_exists) = File_watcher.overwrite_file_if_it_exists old_parent rl new_content in 
      (update_parent fw new_parent,file_exists);;
   let reflect_latest_changes_in_github fw opt_msg = 
      let old_parent = parent fw in 
      let new_parent = File_watcher.reflect_latest_changes_in_github old_parent opt_msg in 
      update_parent fw new_parent ;; 
   let register_rootless_paths fw rls = 
      let old_parent = parent fw in 
      let new_parent = File_watcher.register_rootless_paths old_parent rls in 
      update_parent fw new_parent ;;   
   let remove_files fw rls = 
      let old_parent = parent fw in 
      let new_parent = File_watcher.remove_files old_parent rls in 
      update_parent fw new_parent ;;               
   let rename_subdirectory_as fw sd_pair = 
      let old_parent = parent fw in 
      let new_parent = File_watcher.rename_subdirectory_as old_parent sd_pair in 
      update_parent fw new_parent ;;  
   let set_gitpush_after_backup fw g = 
      let old_parent = parent fw in 
      let new_parent = File_watcher.set_gitpush_after_backup old_parent g in 
      update_parent fw new_parent ;;
   let set_last_noticed_changes fw l = 
      let old_parent = parent fw in 
      let new_parent = File_watcher.set_last_noticed_changes old_parent l in 
      update_parent fw new_parent ;;       

   (* End of inherited constructors *)  
      
   let canonical_tripartition fw all_files =
      let (c_files,nc_files) = List.partition (
                fun rl->
                  Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
      )  all_files in 
      let archived_subdirs = fw.Fw_with_archives_t.subdirs_for_archived_mlx_files in 
      let is_archived = (fun rl->List.exists (Dfn_rootless.is_in rl) archived_subdirs) in 
      let (a_files,u_files) = List.partition is_archived  c_files in 
      (a_files,u_files,nc_files) ;;     
      
   let full_tripartition fw =
      let all_files = Image.image fst (watched_files fw) in 
      canonical_tripartition fw all_files ;;

   let compilable_files fw =
      Option.filter_and_unpack (
         fun (rl,_)->
            if Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
            then Some rl 
            else None   
      )  (watched_files fw) ;;
      
   let compute_small_details_on_one_file fw rl=
      let root = Fw_configuration.root (configuration fw) in 
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
      let all_files = Image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      let the_files = List.filter (
                 fun path-> List.mem (Dfn_rootless.to_module path) mod_names 
         ) u_files in  
      let old_parent = parent fw in    
      let new_parent = File_watcher.remove_files old_parent the_files in 
      update_parent fw new_parent ;;      

   let inspect_and_update old_fw = 
      let old_parent = parent old_fw in    
      let (new_parent,changed_files) = 
          File_watcher.inspect_and_update old_parent
           ~verbose:false in 
      let new_fw = update_parent old_fw new_parent in     
      let (a_files,u_files,nc_files) = 
         canonical_tripartition new_fw changed_files in 
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
      let all_files = Image.image fst (watched_files fw) in 
      let (_,_,nc_files) = canonical_tripartition fw all_files in 
      nc_files ;;
      
   let relocate_module_to fw mod_name new_subdir=
      let all_files = Image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      let the_files = List.filter (
                  fun path-> (Dfn_rootless.to_module path)=mod_name 
      ) u_files in 
      let old_parent = parent fw in    
      let new_parent = File_watcher.relocate_files_to old_parent the_files new_subdir in 
      update_parent fw new_parent  ;;
         
   let rename_module_on_filename_level old_fw (old_module,new_module) = 
      let all_files = Image.image fst (watched_files old_fw) in 
      let (a_files,u_files,nc_files) = canonical_tripartition old_fw all_files in 
      let acolytes = List.filter (
                   fun rl -> (Dfn_rootless.to_module rl) = old_module 
      ) u_files in
      let replacements = Image.image (fun old_rl->
                   (old_rl,Dfn_rootless.rename_module_as (old_module,new_module) old_rl )) acolytes in
      let old_parent = parent old_fw in    
      let new_parent = File_watcher.rename_files old_parent replacements in                    
      let new_fw = update_parent old_fw new_parent in 
      (new_fw,replacements) ;;     
               
   let rename_module_on_content_level old_fw (old_module,new_module) files_to_be_rewritten =
      let apply = (fun par files->
         File_watcher.apply_text_transformation_on_some_files par 
         (Look_for_module_names.change_module_name_in_ml_ocamlcode  
         old_module new_module) files
      ) in 
      let (all_a_files,_,_) = full_tripartition old_fw  in 
      let old_parent = parent old_fw in    
      let (par1,changed_u_files) = apply old_parent files_to_be_rewritten in 
      let (par2,changed_a_files) = apply par1 all_a_files in 
      let announce = (fun trail files ->
               Strung.announce 
                        ~trailer: trail
                           ~printer:Dfn_rootless.to_line ~items:files 
                           ~separator: "\n"
      ) in
      let _ = (
               announce "The following archived files have their content affected by the renaming :" changed_a_files;
               announce "The following usual compilables have their content affected by the renaming :" changed_u_files;
      ) in   
      (update_parent old_fw par2,changed_u_files,changed_a_files) ;;  
                  
   let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten)=
      let (fw2,file_renamings) = rename_module_on_filename_level fw (old_module,new_module) in 
      let (fw3,u_files,a_files) = rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
      (fw3,file_renamings,u_files,a_files) ;;   
   
      
   let replace_string old_fw (replacee,replacer) = 
      let apply = (fun par files->
         File_watcher.apply_text_transformation_on_some_files par 
         (Replace_inside.replace_inside_string (replacee,replacer)) files
      ) in 
      let (all_a_files,all_u_files,_) = full_tripartition old_fw  in 
      let old_parent = parent old_fw in    
      let (par1,changed_u_files) = apply old_parent all_u_files in 
      let (par2,changed_a_files) = apply par1 all_a_files in 
      let announce = (fun trail files ->
         Strung.announce 
               ~trailer: trail
                  ~printer:Dfn_rootless.to_line ~items:files 
                  ~separator: "\n"
      ) in
      let _ = (
         announce "The following archived files have their content affected by the renaming :" changed_a_files;
         announce "The following usual compilables have their content affected by the renaming :" changed_u_files;
      ) in 
      (update_parent old_fw par2,(changed_a_files,changed_u_files)) ;;

   let replace_value old_fw (preceding_files,path) (replacee,pre_replacer) =
      let replacer=(Cull_string.before_rightmost replacee '.')^
          "."^pre_replacer in 
      let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
         preceding_files replacee (Overwriter.of_string pre_replacer) 
           path in 
      let old_parent = parent old_fw in   
      let rootless = Dfn_common.decompose_absolute_path_using_root path 
        (File_watcher.root old_parent)  in 
      let par2= File_watcher.update_some_files old_parent [rootless] in 
      let fw2 = update_parent old_fw par2 in 
      let (fw3,(changed_a_files,changed_u_files))=replace_string fw2 (replacee,replacer) in 
      let par4 = File_watcher.Automatic.reflect_changes_in_diff 
        (parent fw3) (changed_a_files@rootless::changed_u_files) in         
      (update_parent fw2 par4,(changed_a_files,rootless::changed_u_files));;

   let usual_compilable_files fw  =
      let all_files = Image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      u_files ;;        

   let cr_of_list l= Crobj_converter_combinator.of_list  Dfa_subdirectory.to_concrete_object  l;;
   let cr_to_list crobj= Crobj_converter_combinator.to_list  Dfa_subdirectory.of_concrete_object crobj;;
         
   let salt = "Fw_"^"with_archives_t.";;
      
   let parent_label                   = salt ^ "parent";;
   let subdirs_for_archived_mlx_files_label = salt ^ "subdirs_for_archived_mlx_files";;
      
      
   let of_concrete_object ccrt_obj = 
      let g=Concrete_object.get_record ccrt_obj in
      {
         Fw_with_archives_t.parent = File_watcher.of_concrete_object(g parent_label);
         subdirs_for_archived_mlx_files = cr_to_list (g subdirs_for_archived_mlx_files_label);

      };; 
      
   
   let to_concrete_object fw=
      let items= 
      [
           parent_label, File_watcher.to_concrete_object fw.Fw_with_archives_t.parent;
           subdirs_for_archived_mlx_files_label, cr_of_list   (fw.Fw_with_archives_t.subdirs_for_archived_mlx_files);
         
      ]  in
      Concrete_object_t.Record items;;   

   let constructor par opt_subdirs= 
      let subdirs = (match opt_subdirs with (Some l)->l |None -> [Coma_constant.githubbed_archive_subdir]) in    
      {
         Fw_with_archives_t.parent = par;
         subdirs_for_archived_mlx_files = subdirs;

      };; 

end ;;

let compute_all_small_details = Private.compute_all_small_details ;;

let compute_small_details_on_one_file = Private.compute_small_details_on_one_file ;;

let configuration = Private.configuration ;;

let constructor = Private.constructor ;;

let empty_one = Private.empty_one ;;

let forget_modules = Private.forget_modules ;;

let get_content = Private.get_content ;;

let get_mtime = Private.get_mtime ;;

let get_mtime_or_zero_if_file_is_nonregistered = Private.get_mtime_or_zero_if_file_is_nonregistered ;;

let inspect_and_update = Private.inspect_and_update ;;

let last_noticed_changes = Private.last_noticed_changes ;;

let noncompilable_files = Private.noncompilable_files ;;

let of_concrete_object = Private.of_concrete_object ;;

let of_configuration = Private.of_configuration ;;

let of_configuration_and_list = Private.of_configuration_and_list ;;

let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;

let partition_for_singles = Private.canonical_tripartition ;; 

let reflect_latest_changes_in_github = Private.reflect_latest_changes_in_github ;;

let register_rootless_paths = Private.register_rootless_paths ;; 

let remove_files = Private.remove_files ;; 

let rename_subdirectory_as = Private.rename_subdirectory_as ;; 

let relocate_module_to = Private.relocate_module_to ;;

let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;

let root = Private.root ;; 

let set_gitpush_after_backup = Private.set_gitpush_after_backup ;; 

let set_last_noticed_changes = Private.set_last_noticed_changes ;; 

let to_concrete_object = Private.to_concrete_object ;;

let usual_compilable_files = Private.usual_compilable_files ;;

let watched_files = Private.watched_files ;;

