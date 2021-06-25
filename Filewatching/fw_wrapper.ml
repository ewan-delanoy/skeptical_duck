(*

#use"Filewatching/fw_wrapper.ml";;

*)

exception Register_rootless_path_exn of string list;;

module Automatic = struct 

   exception Rootless_not_found of Dfn_rootless_t.t;;

   module Private = struct 
   
   let pair_of_crobj crobj=
      let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
     (
       Dfn_rootless.of_concrete_object arg1,
       Crobj_converter.string_of_concrete_object arg2
     );;
   
   let pair_to_crobj (watched_file,modif_date)=
     Concrete_object_t.Variant("Dfn_"^"rootless.J",
        [
           
           Dfn_rootless.to_concrete_object watched_file;
           Crobj_converter.string_to_concrete_object(modif_date);
        ]
      ) ;;
   
   let salt = "Fw_"^"wrapper_t.";;
   
   let parent_label                    = salt ^ "parent";;

   
   let of_concrete_object ccrt_obj = 
      let g=Concrete_object.get_record ccrt_obj in
      {
         Fw_wrapper_t.parent = Fw_nonmodular_wrapper.of_concrete_object(g parent_label);
      };; 
   
   let to_concrete_object fw=
      let items= 
      [
       parent_label, Fw_nonmodular_wrapper.to_concrete_object fw.Fw_wrapper_t.parent;
      
      ]  in
      Concrete_object_t.Record items;;
   
   let configuration fw =
       (fw.Fw_wrapper_t.parent).Fw_nonmodular_wrapper_t.configuration ;;

   
   let watched_files fw = (fw.Fw_wrapper_t.parent).Fw_nonmodular_wrapper_t.watched_files ;;
   
   let partition_for_pairs parent all_files =
      let (c_files,nc_files) = List.partition (
          fun (rl,_)->
            Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
      )  all_files in 
      let config = Fw_nonmodular_wrapper.Automatic.configuration parent in
      let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in 
      let is_archived = (fun (rl,_)->List.exists (Dfn_rootless.is_in rl) archived_subdirs) in 
      let (a_files,u_files) = List.partition is_archived  c_files in 
      (a_files,u_files,nc_files) ;;

   let usual_update mother =
   {
      Fw_wrapper_t.parent = mother ;
   } ;;  
      
      
   let parent fw = fw.Fw_wrapper_t.parent ;;

   end ;;

   let configuration      = Private.configuration ;;
   let get_content fw     = Fw_nonmodular_wrapper.get_content 
                                  (Private.parent fw) ;;
   let get_mtime fw       = Fw_nonmodular_wrapper.get_mtime 
                                  (Private.parent fw) ;; 
   let get_mtime_or_zero_if_file_is_nonregistered fw  = 
                       Fw_nonmodular_wrapper.get_mtime_or_zero_if_file_is_nonregistered
                                  (Private.parent fw) ;;     
   let last_noticed_changes fw = Fw_nonmodular_wrapper.Automatic.last_noticed_changes
                                  (Private.parent fw) ;;                                                                                         
   let of_concrete_object = Private.of_concrete_object;;
   let parent             = Private.parent ;;
   let root fw     = Fw_nonmodular_wrapper.Automatic.root (Private.parent fw) ;;
   let set_configuration fw new_config = 
      let old_parent = fw.Fw_wrapper_t.parent in
      let new_parent = Fw_nonmodular_wrapper.Automatic.set_configuration old_parent new_config in 
      Private.usual_update new_parent
      (* {
      fw with 
       Fw_wrapper_t.parent = new_parent;
      } *);;
   let set_last_noticed_changes fw new_config = 
      let old_parent = fw.Fw_wrapper_t.parent in
      let new_parent = Fw_nonmodular_wrapper.Automatic.set_last_noticed_changes old_parent new_config in 
      Private.usual_update new_parent
      (* {
      fw with 
       Fw_wrapper_t.parent = new_parent;
      } *) ;;
   let to_concrete_object = Private.to_concrete_object;;
   let usual_update       = Private.usual_update ;;
   let watched_files      = Private.watched_files ;;

end ;;   




module Private = struct

   let partition_for_singles parent all_files =
      let (c_files,nc_files) = List.partition (
          fun rl->
            Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
      )  all_files in 
      let config = Fw_nonmodular_wrapper.Automatic.configuration parent in
      let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in 
      let is_archived = (fun rl->List.exists (Dfn_rootless.is_in rl) archived_subdirs) in 
      let (a_files,u_files) = List.partition is_archived  c_files in 
      (a_files,u_files,nc_files) ;;   

let noncompilable_files fw  =
      let parent = Automatic.parent fw in 
      let all_files = Image.image fst (Fw_nonmodular_wrapper.Automatic.watched_files parent) in 
      let (_,_,nc_files) = partition_for_singles parent all_files in 
      nc_files ;;

let usual_compilable_files fw  =
   let parent = Automatic.parent fw in 
   let all_files = Image.image fst (Fw_nonmodular_wrapper.Automatic.watched_files parent) in 
   let (_,u_files,_) = partition_for_singles parent all_files in 
   u_files ;;

let forget_modules fw mod_names =
   let old_parent = Automatic.parent fw in 
   let the_files = List.filter (
     fun path-> List.mem (Dfn_rootless.to_module path) mod_names 
   ) (usual_compilable_files fw) in    
   let new_parent = Fw_nonmodular_wrapper.remove_files old_parent the_files in
   Automatic.usual_update new_parent;;

let inspect_and_update fw  =
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = Fw_nonmodular_wrapper.inspect_and_update old_parent in
   (Automatic.usual_update new_parent,
    partition_for_singles new_parent changed_files);;

let of_configuration config =   
    let mother = Fw_nonmodular_wrapper.of_configuration config in 
    Automatic.usual_update mother ;;

let of_configuration_and_list config files=   
      let mother = Fw_nonmodular_wrapper.of_configuration_and_list config files  in 
      Automatic.usual_update mother ;;

let overwrite_file_if_it_exists fw rootless new_content = 
   let old_parent = Automatic.parent fw in 
   let (new_parent,change_made) = 
      Fw_nonmodular_wrapper.overwrite_file_if_it_exists 
        old_parent rootless new_content in 
   if change_made 
   then Automatic.usual_update new_parent
   else fw ;;           

let reflect_latest_changes_in_github fw opt_msg=  
   let old_parent = Automatic.parent fw in 
   let new_parent = Fw_nonmodular_wrapper.reflect_latest_changes_in_github old_parent opt_msg in 
   Automatic.usual_update new_parent ;;   

let register_rootless_paths fw rootless_paths= 
   let old_parent = Automatic.parent fw in 
   let new_parent = Fw_nonmodular_wrapper.register_rootless_paths 
        old_parent rootless_paths in 
   (Automatic.usual_update new_parent,
     partition_for_singles new_parent rootless_paths ) ;;    
   
let relocate_module_to fw mod_name new_subdir=
   let the_files = List.filter (
      fun path-> (Dfn_rootless.to_module path)=mod_name 
   ) (usual_compilable_files fw) in 
   let old_parent = Automatic.parent fw in 
   let new_parent = Fw_nonmodular_wrapper.relocate_files_to 
        old_parent the_files new_subdir in 
   Automatic.usual_update new_parent ;;  

let remove_files fw rootless_paths=   
   let old_parent = Automatic.parent fw in 
   let new_parent = Fw_nonmodular_wrapper.remove_files 
      old_parent rootless_paths in 
   Automatic.usual_update new_parent ;;

let rename_module_on_filename_level fw (old_module,new_module) =
      let acolytes = List.filter (
          fun rl -> (Dfn_rootless.to_module rl) = old_module 
      ) (usual_compilable_files fw) in
      let replacements = Image.image (fun old_rl->
          (old_rl,Dfn_rootless.rename_module_as (old_module,new_module) old_rl )) acolytes in
      let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
      let l_cmds = Image.image (
          fun (old_rl,new_rl) ->
            let s_old_ap=s_root^(Dfn_rootless.to_line old_rl) 
            and s_new_ap=s_root^(Dfn_rootless.to_line new_rl) in    
            "mv "^s_old_ap^" "^s_new_ap
      ) replacements  in
      let _ =Unix_command.conditional_multiple_uc l_cmds in  
      let old_parent = Automatic.parent fw in 
      let new_parent = Fw_nonmodular_wrapper.rename_files  old_parent replacements in 
      Automatic.usual_update new_parent ;;  
      
let rename_module_on_content_level fw (old_module,new_module) files_to_be_rewritten =
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = Fw_nonmodular_wrapper.apply_text_transformation_on_some_files old_parent
      (Look_for_module_names.change_module_name_in_ml_ocamlcode  
      old_module new_module)  files_to_be_rewritten in 
   (Automatic.usual_update new_parent,changed_files) ;;  
         
let rename_module_on_both_levels fw old_module new_module files_to_be_rewritten=
   let fw2=rename_module_on_filename_level fw (old_module,new_module) in 
   let fw3=rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
   fw3;;

let rename_subdirectory_as fw (old_subdir,new_subdir)=   
   let old_parent = Automatic.parent fw in 
   let new_parent = Fw_nonmodular_wrapper.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
   Automatic.usual_update new_parent ;;   

let replace_string fw (replacee,replacer)=
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = Fw_nonmodular_wrapper.replace_string old_parent (replacee,replacer) in
   (Automatic.usual_update new_parent,
    partition_for_singles new_parent changed_files);;   

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = 
        Fw_nonmodular_wrapper.replace_value 
         old_parent (preceding_files,path) (replacee,pre_replacer) in
   (Automatic.usual_update new_parent,
    partition_for_singles new_parent changed_files);;   



end;;

let canonical_tripartition = Private.partition_for_singles ;;

let empty_one config= {
   Fw_wrapper_t.parent = Fw_nonmodular_wrapper.empty_one config;
};; 

let forget_modules = Private.forget_modules ;;

let get_content = Automatic.get_content ;;

let get_mtime = Automatic.get_mtime ;;

let get_mtime_or_zero_if_file_is_nonregistered = Automatic.get_mtime_or_zero_if_file_is_nonregistered ;;

let inspect_and_update = Private.inspect_and_update;;

let noncompilable_files = Private.noncompilable_files ;;

let of_concrete_object = Automatic.of_concrete_object ;;

let of_configuration = Private.of_configuration ;;

let of_configuration_and_list = Private.of_configuration_and_list ;;

let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;

let reflect_latest_changes_in_github = Private.reflect_latest_changes_in_github ;;

let register_rootless_paths = Private.register_rootless_paths;;

let relocate_module_to = Private.relocate_module_to;;

let remove_files = Private.remove_files;;

let rename_module_on_filename_level_and_in_files = Private.rename_module_on_both_levels ;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;

let root = Automatic.root ;;

let to_concrete_object = Automatic.to_concrete_object ;;

let usual_compilable_files = Private.usual_compilable_files ;;
