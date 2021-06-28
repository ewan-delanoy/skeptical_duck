(*

#use"Filewatching/fw_with_module_linking.ml";;

*)

exception Register_rootless_path_exn of string list;;

module Automatic = struct 

   exception Rootless_not_found of Dfn_rootless_t.t;;

   module Private = struct 
   
   let pair_of_crobj crobj=
      let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
     (
       Dfn_rootless.of_concrete_object arg1,
       Crobj_converter_combinator.to_list Dfa_module.of_concrete_object arg2
     );;
   
   let pair_to_crobj (watched_file,linking)=
     Concrete_object_t.Variant("Dfn_"^"rootless.J",
        [
           
           Dfn_rootless.to_concrete_object watched_file;
           Crobj_converter_combinator.of_list Dfa_module.to_concrete_object linking
        ]
      ) ;;
   
   let salt = "Fw_"^"with_module_linking_t.";;
   
   let parent_label             = salt ^ "parent";;
   let module_linking_label     = salt ^ "module_linking";;
   let index_for_caching_label  = salt ^ "index_for_caching";;

   
   let of_concrete_object ccrt_obj = 
      let g=Concrete_object.get_record ccrt_obj in
      {
         Fw_with_module_linking_t.parent = File_watcher.of_concrete_object(g parent_label);
         module_linking = Crobj_converter_combinator.to_list pair_of_crobj (g module_linking_label);
         index_for_caching = Crobj_converter.int_of_concrete_object(g index_for_caching_label);
      };; 
   
   let to_concrete_object fw=
      let items= 
      [
         parent_label, File_watcher.to_concrete_object fw.Fw_with_module_linking_t.parent;
         module_linking_label, Crobj_converter_combinator.of_list pair_to_crobj fw.Fw_with_module_linking_t.module_linking;
         index_for_caching_label, Crobj_converter.int_to_concrete_object fw.Fw_with_module_linking_t.index_for_caching;
      
      ]  in
      Concrete_object_t.Record items;;
   
   let configuration fw = File_watcher.Automatic.configuration (fw.Fw_with_module_linking_t.parent) ;;

   
   let watched_files fw = File_watcher.Automatic.watched_files (fw.Fw_with_module_linking_t.parent) ;;

   let usual_update mother =
   {
      Fw_with_module_linking_t.parent = mother ;
      module_linking = [];
      index_for_caching = 0;

   } ;;  
      
      
   let parent fw = fw.Fw_with_module_linking_t.parent ;;

   end ;;

   let configuration      = Private.configuration ;;
   let get_content fw     = File_watcher.get_content 
                                  (Private.parent fw) ;;
   let get_mtime fw       = File_watcher.get_mtime 
                                  (Private.parent fw) ;; 
   let get_mtime_or_zero_if_file_is_nonregistered fw  = 
                       File_watcher.get_mtime_or_zero_if_file_is_nonregistered
                                  (Private.parent fw) ;;     
   let last_noticed_changes fw = File_watcher.Automatic.last_noticed_changes
                                  (Private.parent fw) ;;                                                                                         
   let of_concrete_object = Private.of_concrete_object;;
   let parent             = Private.parent ;;
   let root fw     = File_watcher.Automatic.root (Private.parent fw) ;;
   let set_configuration fw new_config = 
      let old_parent = fw.Fw_with_module_linking_t.parent in
      let new_parent = File_watcher.Automatic.set_configuration old_parent new_config in 
      {
      fw with 
         Fw_with_module_linking_t.parent = new_parent;
      } ;;
   let set_last_noticed_changes fw new_config = 
      let old_parent = fw.Fw_with_module_linking_t.parent in
      let new_parent = File_watcher.Automatic.set_last_noticed_changes old_parent new_config in 
      {
      fw with 
         Fw_with_module_linking_t.parent = new_parent;
      }  ;;
   let to_concrete_object = Private.to_concrete_object;;
   let usual_update       = Private.usual_update ;;
   let watched_files      = Private.watched_files ;;

end ;;   




module Private = struct


let forget_modules fw mod_names =
   let old_parent = Automatic.parent fw in   
   let new_parent = File_watcher.forget_modules old_parent mod_names in 
   
   {
      fw with 
      Fw_with_module_linking_t.parent = new_parent;
   } ;;

let inspect_and_update fw  =
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = File_watcher.inspect_and_update old_parent in
   (Automatic.usual_update new_parent,
   File_watcher.partition_for_singles new_parent changed_files);;

let of_configuration config =   
    let mother = File_watcher.of_configuration config in 
    Automatic.usual_update mother ;;

let of_configuration_and_list config files=   
      let mother = File_watcher.of_configuration_and_list config files  in 
      Automatic.usual_update mother ;;

let overwrite_file_if_it_exists fw rootless new_content = 
   let old_parent = Automatic.parent fw in 
   let (new_parent,change_made) = 
      File_watcher.overwrite_file_if_it_exists 
        old_parent rootless new_content in 
   if change_made 
   then Automatic.usual_update new_parent
   else fw ;;           

let reflect_latest_changes_in_github fw opt_msg=  
   let old_parent = Automatic.parent fw in 
   let new_parent = File_watcher.reflect_latest_changes_in_github old_parent opt_msg in 
   Automatic.usual_update new_parent ;;   

let register_rootless_paths fw rootless_paths= 
   let old_parent = Automatic.parent fw in 
   let new_parent = File_watcher.register_rootless_paths 
        old_parent rootless_paths in 
   (Automatic.usual_update new_parent,
   File_watcher.partition_for_singles new_parent rootless_paths ) ;;    
   
let relocate_module_to fw mod_name new_subdir=
   let old_parent = Automatic.parent fw in 
   let new_parent = File_watcher.relocate_module_to 
        old_parent mod_name new_subdir in 
   Automatic.usual_update new_parent ;;  

let remove_files fw rootless_paths=   
   let old_parent = Automatic.parent fw in 
   let new_parent = File_watcher.remove_files 
      old_parent rootless_paths in 
   Automatic.usual_update new_parent ;;

let rename_module_on_filename_level fw (old_module,new_module) =
      let old_parent = Automatic.parent fw in 
      let acolytes = List.filter (
          fun rl -> (Dfn_rootless.to_module rl) = old_module 
      ) (File_watcher.usual_compilable_files old_parent) in
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
      let new_parent = File_watcher.rename_files  old_parent replacements in 
      Automatic.usual_update new_parent ;;  
      
let rename_module_on_content_level fw (old_module,new_module) files_to_be_rewritten =
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = File_watcher.apply_text_transformation_on_some_files old_parent
      (Look_for_module_names.change_module_name_in_ml_ocamlcode  
      old_module new_module)  files_to_be_rewritten in 
   (Automatic.usual_update new_parent,changed_files) ;;  
         
let rename_module_on_both_levels fw old_module new_module files_to_be_rewritten=
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = File_watcher.rename_module_on_filename_level_and_in_files
     old_parent old_module new_module files_to_be_rewritten in 
     (Automatic.usual_update new_parent,changed_files);;

let rename_subdirectory_as fw (old_subdir,new_subdir)=   
   let old_parent = Automatic.parent fw in 
   let new_parent = File_watcher.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
   Automatic.usual_update new_parent ;;   

let replace_string fw (replacee,replacer)=
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = File_watcher.replace_string old_parent (replacee,replacer) in
   (Automatic.usual_update new_parent,
   File_watcher.partition_for_singles new_parent changed_files);;   

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
   let old_parent = Automatic.parent fw in 
   let (new_parent,changed_files) = 
        File_watcher.replace_value 
         old_parent (preceding_files,path) (replacee,pre_replacer) in
   (Automatic.usual_update new_parent,
   File_watcher.partition_for_singles new_parent changed_files);;   



end;;


let empty_one config= {
   Fw_with_module_linking_t.parent = File_watcher.empty_one config;
   module_linking = [];
   index_for_caching = 0;
};; 

let forget_modules = Private.forget_modules ;;

let get_content = Automatic.get_content ;;

let get_mtime = Automatic.get_mtime ;;

let get_mtime_or_zero_if_file_is_nonregistered = Automatic.get_mtime_or_zero_if_file_is_nonregistered ;;

let inspect_and_update = Private.inspect_and_update;;

let noncompilable_files fw = File_watcher.noncompilable_files (Automatic.parent fw) ;;

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

let usual_compilable_files fw = File_watcher.usual_compilable_files (Automatic.parent fw) ;;
