(*

#use"lib/Filewatching/Fw_classes/fwc_with_archives.ml";;

*)


type t = Fwg_with_archives.t ;;

module Inherited = struct 

   module Ancestry = Fwc_file_watcher.Inherited ;;

   module Parent = Fwc_file_watcher ;;
   let parent = Fwg_with_archives.parent ;;

   let ignored_files fw = Ancestry.ignored_files (parent fw) ;;
   let ignored_subdirectories fw = Ancestry.ignored_subdirectories (parent fw) ;;
   let root fw = Ancestry.root (parent fw) ;;
   let test_for_admissibility fw = Ancestry.test_for_admissibility (parent fw) ;;
   let to_fw_configuration fw = Ancestry.to_fw_configuration (parent fw) ;;

   let test_equality fw1 fw2 = 
      (
        Ancestry.test_equality (parent fw1) (parent fw2)
      )
      @
      (
        List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld)
        [
          "subdirs_for_archived_mlx_files",(
            (Fwg_with_archives.subdirs_for_archived_mlx_files fw1)=
          (Fwg_with_archives.subdirs_for_archived_mlx_files fw2))
        ]
      ) ;;
    
   let watched_files fw = Fwc_file_watcher.watched_files (parent fw) ;;   

end ;;   

module Crobj = struct 
   let salt = "Fwc_with_archives." ;;
   let label_for_parent = salt ^ "parent" ;;
   let label_for_subdirs_for_archived_mlx_files  = salt ^ "subdirs_for_archived_mlx_files" ;;
       
       
   let of_concrete_object ccrt_obj = 
     let g=Concrete_object.get_record ccrt_obj in 
     Fwg_with_archives.make 
     (Fwc_file_watcher.Crobj.of_concrete_object (g label_for_parent))
     (Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object  (g label_for_subdirs_for_archived_mlx_files))
     ;;
       
   let to_concrete_object fw = 
     let items =  
     [
          label_for_parent, Fwc_file_watcher.Crobj.to_concrete_object ( Fwg_with_archives.parent fw ) ;
          label_for_subdirs_for_archived_mlx_files, 
          Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object 
           (Fwg_with_archives.subdirs_for_archived_mlx_files fw ) ;
     ] in 
     Concrete_object_t.Record items ;;
       
       
end;; 

module Private = struct

 

   let parent fw = Inherited.parent fw ;;
   
   (* Inherited methods *)


   let root fw = Inherited.root fw ;;
   let update_parent fw new_parent = 
      Fwg_with_archives.make new_parent (Fwg_with_archives.subdirs_for_archived_mlx_files fw) ;;
   let watched_files fw = Inherited.watched_files fw ;;
   
   (* End of inherited methods *)  
   (* Inherited constructors *)

   let constructor par opt_subdirs= 
      let subdirs = (match opt_subdirs with (Some l)->l |None -> [Fw_constant.watched_and_githubbed_subdir]) in    
      Fwg_with_archives.make par subdirs ;;
      
   let plunge_fw_configuration config = constructor(Fwc_file_watcher.plunge_fw_configuration config) None ;;
   let of_configuration config = constructor(Fwc_file_watcher.of_configuration config) None ;;
   let of_configuration_and_list config l = constructor (Fwc_file_watcher.of_configuration_and_list config l) None;;
   let overwrite_file_if_it_exists fw rl new_content = 
      let old_parent = parent fw in 
      let (new_parent,file_exists) = Fwc_file_watcher.overwrite_file_if_it_exists old_parent rl new_content in 
      (update_parent fw new_parent,file_exists);;
   let register_rootless_paths fw rls = 
      let old_parent = parent fw in 
      let new_parent = Fwc_file_watcher.register_rootless_paths old_parent rls in 
      update_parent fw new_parent ;;   
   let remove_files fw rls = 
      let old_parent = parent fw in 
      let new_parent = Fwc_file_watcher.remove_files old_parent rls in 
      update_parent fw new_parent ;;               
   let rename_subdirectory_as fw sd_pair = 
      let old_parent = parent fw in 
      let (new_parent,extra) = Fwc_file_watcher.rename_subdirectory_as old_parent sd_pair in 
      (update_parent fw new_parent,extra) ;;  
     

   (* End of inherited constructors *)  
      
   let canonical_tripartition fw all_files =
      let (c_files,nc_files) = List.partition (
                fun rl->
                  Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
      )  all_files in 
      let archived_subdirs = Fwg_with_archives.subdirs_for_archived_mlx_files fw in 
      let is_archived = (fun rl->List.exists (Dfn_rootless.is_in rl) archived_subdirs) in 
      let (a_files,u_files) = List.partition is_archived  c_files in 
      (a_files,u_files,nc_files) ;;     
      
   let full_tripartition fw =
      let all_files = Image.image fst (watched_files fw) in 
      canonical_tripartition fw all_files ;;

   let compilable_files fw =
      List.filter_map (
         fun (rl,_)->
            if Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
            then Some rl 
            else None   
      )  (watched_files fw) ;;
      
   let compute_small_details_on_one_file fw rl=
      let root = Inherited.root fw in 
      let s_ap = Dfn_common.recompose_potential_absolute_path root rl in 
      let ap = Absolute_path.of_string s_ap in 
      Fw_file_details.compute ap ;;

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
      let new_parent = Fwc_file_watcher.remove_files old_parent the_files in 
      (update_parent fw new_parent,the_files) ;;      
   
   
   let announce_changes fw changed_files=    
         let (a_files,u_files,nc_files) = 
            canonical_tripartition fw changed_files in 
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
         (a_files,u_files,nc_files) ;;   


   let inspect_and_update old_fw = 
      let old_parent = parent old_fw in    
      let (new_parent,changed_files) = 
          Fwc_file_watcher.inspect_and_update old_parent
           ~verbose:false in 
      let new_fw = update_parent old_fw new_parent in     
      let (a_files,u_files,_nc_files) = announce_changes new_fw changed_files in 
      (new_fw,changed_files,(a_files,u_files)) ;;   

   let latest_changes fw = 
      let changed_files = Fwc_file_watcher.latest_changes (parent fw) ~verbose:false in 
      announce_changes fw changed_files ;;

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
      let replacements = Image.image (fun path->
         (path,Dfn_rootless.relocate_to path new_subdir)
       ) the_files in 
      let new_parent = Fwc_file_watcher.rename_files old_parent replacements in 
      (update_parent fw new_parent,replacements)  ;;
         
   let rename_module_on_filename_level old_fw (old_module,new_module) = 
      let all_files = Image.image fst (watched_files old_fw) in 
      let (_a_files,u_files,_nc_files) = canonical_tripartition old_fw all_files in 
      let acolytes = List.filter (
                   fun rl -> (Dfn_rootless.to_module rl) = old_module 
      ) u_files in
      let replacements = Image.image (fun old_rl->
                   (old_rl,Dfn_rootless.rename_module_as (old_module,new_module) old_rl )) acolytes in
      let old_parent = parent old_fw in    
      let new_parent = Fwc_file_watcher.rename_files old_parent replacements in                    
      let new_fw = update_parent old_fw new_parent in 
      (new_fw,replacements) ;;     
               
   let rename_module_on_content_level old_fw (old_module,new_module) files_to_be_rewritten =
      let apply = (fun par files->
         Fwc_file_watcher.apply_text_transformation_on_some_files par 
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
      let (fw3,changed_u_files,changed_a_files) = rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
      (fw3,file_renamings,changed_u_files,changed_a_files) ;;   
   
      
   let replace_string old_fw (replacee,replacer) = 
      let apply = (fun par files->
         Fwc_file_watcher.apply_text_transformation_on_some_files par 
         (Replace_inside.replace_inside_text ~display_number_of_matches:false (replacee,replacer)) files
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
         preceding_files replacee pre_replacer 
           path in 
      let old_parent = parent old_fw in   
      let rootless = Dfn_common.decompose_absolute_path_using_root path 
        (Fwc_file_watcher.Inherited.root old_parent)  in 
      let par2= Fwc_file_watcher.update_some_files old_parent [rootless] in 
      let fw2 = update_parent old_fw par2 in 
      let (fw3,(changed_a_files,changed_u_files))=replace_string fw2 (replacee,replacer) in 
      let all_changes = changed_a_files@rootless::changed_u_files in       
      (fw3,(all_changes,rootless::changed_u_files));;

   let usual_compilable_files fw  =
      let all_files = Image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      u_files ;;        

      
   let archived_files fw  =
      let all_files = Image.image fst (watched_files fw) in 
      let (a_files,_,_) = canonical_tripartition fw all_files in 
      a_files ;;     
     
   let check_that_no_change_has_occurred fw =
      Fwc_file_watcher.check_that_no_change_has_occurred (parent fw) ;; 
      
end ;;

let archived_files = Private.archived_files ;; 
let check_that_no_change_has_occurred = Private.check_that_no_change_has_occurred;;
let compute_all_small_details = Private.compute_all_small_details ;;
let compute_small_details_on_one_file = Private.compute_small_details_on_one_file ;;
let forget_modules = Private.forget_modules ;;
let inspect_and_update = Private.inspect_and_update ;;
let latest_changes = Private.latest_changes ;;
let noncompilable_files = Private.noncompilable_files ;;
let of_configuration = Private.of_configuration ;;
let of_configuration_and_list = Private.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
let partition_for_singles = Private.canonical_tripartition ;; 
let plunge_fw_configuration = Private.plunge_fw_configuration ;;
let register_rootless_paths = Private.register_rootless_paths ;; 
let relocate_module_to = Private.relocate_module_to ;;
let remove_files = Private.remove_files ;;  
let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;
let rename_subdirectory_as = Private.rename_subdirectory_as ;;
let replace_string = Private.replace_string;;
let replace_value = Private.replace_value;;
let usual_compilable_files = Private.usual_compilable_files ;; 
