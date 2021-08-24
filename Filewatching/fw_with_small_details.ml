(*

#use"Filewatching/fw_with_small_details.ml";;

*)

exception Register_rootless_path_exn of string list;;

module Automatic = struct 

   exception Rootless_not_found of Dfn_rootless_t.t;;

   module Private = struct 
   
      let cr_of_pair_list f l= Crobj_converter_combinator.of_pair_list  Dfn_rootless.to_concrete_object f l;;
      let cr_to_pair_list f crobj= Crobj_converter_combinator.to_pair_list  Dfn_rootless.of_concrete_object f crobj;;
         
      let salt = "Fw_"^"with_module_linking_t.";;
      
      let parent_label                  = salt ^ "parent";;
      let small_details_in_files_label  = salt ^ "small_details_in_files";;
      
      
      let of_concrete_object ccrt_obj = 
         let g=Concrete_object.get_record ccrt_obj in
         {
            Fw_with_small_details_t.parent = File_watcher.of_concrete_object(g parent_label);
            small_details_in_files = cr_to_pair_list Fw_file_small_details.of_concrete_object (g small_details_in_files_label);

         };; 
      
   
      let to_concrete_object fw=
         let items= 
         [
           parent_label, File_watcher.to_concrete_object fw.Fw_with_small_details_t.parent;
           small_details_in_files_label, cr_of_pair_list  Fw_file_small_details.to_concrete_object (fw.Fw_with_small_details_t.small_details_in_files);
         
         ]  in
         Concrete_object_t.Record items;;
      
   
   
   let configuration fw = File_watcher.Automatic.configuration (fw.Fw_with_small_details_t.parent) ;;

   
   let watched_files fw = File_watcher.Automatic.watched_files (fw.Fw_with_small_details_t.parent) ;;

   let constructor mother =
   {
      Fw_with_small_details_t.parent = mother ;
      small_details_in_files = File_watcher.compute_all_small_details mother;
   } ;;  
      
      
   let parent fw = fw.Fw_with_small_details_t.parent ;;

   end ;;

   let configuration      = Private.configuration ;;
   let constructor        = Private.constructor ;;
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
   let set_gitpush_after_backup fw new_gab = 
      let old_nonmodular = fw.Fw_with_small_details_t.parent in 
      let new_nonmodular = File_watcher.Automatic.set_gitpush_after_backup 
            old_nonmodular new_gab in 
      {
          fw with  
         Fw_with_small_details_t.parent = new_nonmodular ;
      } ;;
      
   let set_last_noticed_changes fw new_config = 
      let old_parent = fw.Fw_with_small_details_t.parent in
      let new_parent = File_watcher.Automatic.set_last_noticed_changes old_parent new_config in 
      {
      fw with 
       Fw_with_small_details_t.parent = new_parent;
      } ;;
   let small_details_in_files fw = fw.Fw_with_small_details_t.small_details_in_files ;;   
   let to_concrete_object = Private.to_concrete_object;;
   
   let watched_files      = Private.watched_files ;;

end ;;   




module Private = struct


let forget_modules fw mod_names =
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let new_parent = File_watcher.forget_modules old_parent mod_names in
   {
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = List.filter (
        fun (rl,_)->not(List.mem (Dfn_rootless.to_module rl) mod_names)
      ) old_details;
   } ;;  

let inspect_and_update fw  =
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let (new_parent,changed_files) = File_watcher.inspect_and_update old_parent in
   let changed_details_ref = ref [] in 
   ({
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = Image.image (
        fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_files 
         then let new_pair = (rl,File_watcher.compute_small_details_on_one_file new_parent rl) in 
              let _ = (changed_details_ref:=new_pair::(!changed_details_ref) ) in 
              new_pair 
         else old_pair  
      ) old_details;
   },
   (File_watcher.partition_for_singles new_parent changed_files,!changed_details_ref));;

let of_configuration config =   
    let mother = File_watcher.of_configuration config in 
    Automatic.constructor mother ;;

let of_configuration_and_list (config,files)=   
   let mother = File_watcher.of_configuration_and_list config files  in 
   Automatic.constructor mother ;;

let overwrite_file_if_it_exists fw (rootless,new_content) = 
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let (new_parent,change_made) = 
      File_watcher.overwrite_file_if_it_exists 
        old_parent rootless new_content in 
   let accu = ref None in      
   let new_fw = (
   if change_made 
   then 
      {
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = Image.image (
            fun old_pair->
            let rl = fst old_pair in
            if rl  = rootless 
            then let new_pair = (rl,File_watcher.compute_small_details_on_one_file new_parent rl) in 
                 let _= (accu:=Some(new_pair)) in 
                 new_pair
            else old_pair  
         ) old_details;
      }
   else fw ) in (new_fw,!accu);;           

let reflect_latest_changes_in_github fw opt_msg=  
   let old_parent = Automatic.parent fw in 
   let new_parent = File_watcher.reflect_latest_changes_in_github old_parent opt_msg in 
   {
      fw with 
       Fw_with_small_details_t.parent = new_parent;
   } ;;  

let register_rootless_paths fw rootless_paths= 
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let new_parent = File_watcher.register_rootless_paths 
        old_parent rootless_paths in 
   ({
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = old_details @ 
      (Image.image (fun rl->
         (rl,File_watcher.compute_small_details_on_one_file new_parent rl)) 
         rootless_paths)
      ;
   },
   File_watcher.partition_for_singles new_parent rootless_paths ) ;;    
   
let relocate_module_to fw (mod_name,new_subdir)=
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let new_parent = File_watcher.relocate_module_to 
        old_parent mod_name new_subdir in 
   {
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if (Dfn_rootless.to_module rl) = mod_name 
         then let new_rl = Dfn_rootless.relocate_to rl new_subdir in 
              (new_rl,snd old_pair)
         else old_pair        
      ) old_details;
   } ;;  

let remove_files fw rootless_paths=   
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let new_parent = File_watcher.remove_files 
      old_parent rootless_paths in 
   {
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = List.filter (
         fun (rl,_)->not(List.mem rl rootless_paths)
      ) old_details;
   } ;;

let rename_module_on_filename_level fw (old_module,new_module) =
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
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
   {
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = Image.image (
            fun old_pair->
            let rl = fst old_pair in
            match List.assoc_opt rl replacements with 
            Some(new_rl) -> (new_rl,File_watcher.compute_small_details_on_one_file new_parent new_rl)
            | None -> old_pair        
      ) old_details;
   } ;;     
      
let rename_module_on_content_level fw (old_module,new_module) files_to_be_rewritten =
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let (new_parent,changed_files) = File_watcher.apply_text_transformation_on_some_files old_parent
      (Look_for_module_names.change_module_name_in_ml_ocamlcode  
      old_module new_module)  files_to_be_rewritten in 
   ({
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = Image.image (
        fun old_pair->
          let rl = fst old_pair in
          if List.mem rl changed_files
          then (rl,File_watcher.compute_small_details_on_one_file new_parent rl)
          else old_pair  
      ) old_details;
    },changed_files) ;;  
         
let rename_module_on_both_levels fw (old_module,new_module,files_to_be_rewritten)=
   let fw2 = rename_module_on_filename_level fw (old_module,new_module) in 
   rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten ;;

let rename_subdirectory_as fw (old_subdir,new_subdir)=   
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let new_parent = File_watcher.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
   {
      Fw_with_small_details_t.parent = new_parent ;
       small_details_in_files = Image.image (
      fun old_pair->
      let rl = fst old_pair in
      match Dfn_rootless.soak (old_subdir,new_subdir) rl with 
      (Some new_rl) -> (new_rl,snd old_pair)
      | None -> old_pair        
      ) old_details ;
} ;;   

let replace_string fw (replacee,replacer)=
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let (new_parent,changed_files) = File_watcher.replace_string old_parent (replacee,replacer) in
   ({
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_files
         then (rl,File_watcher.compute_small_details_on_one_file new_parent rl)
         else old_pair  
      ) old_details;
   },
   File_watcher.partition_for_singles new_parent changed_files);;   

let replace_value fw ((preceding_files,path),(replacee,pre_replacer)) =
   let old_parent = Automatic.parent fw 
   and old_details = Automatic.small_details_in_files fw  in 
   let (new_parent,changed_files) = 
        File_watcher.replace_value 
         old_parent (preceding_files,path) (replacee,pre_replacer) in
   ({
      Fw_with_small_details_t.parent = new_parent ;
      small_details_in_files = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_files
         then (rl,File_watcher.compute_small_details_on_one_file new_parent rl)
         else old_pair  
      ) old_details;
   },
   File_watcher.partition_for_singles new_parent changed_files);;   

end;;

let configuration = Automatic.configuration ;;

let empty_one config= {
   Fw_with_small_details_t.parent = File_watcher.empty_one config;
   small_details_in_files = [];
};; 

let forget_modules = Private.forget_modules ;;

let get_content = Automatic.get_content ;;

let get_mtime = Automatic.get_mtime ;;

let get_mtime_or_zero_if_file_is_nonregistered = Automatic.get_mtime_or_zero_if_file_is_nonregistered ;;

let inspect_and_update = Private.inspect_and_update;;

let last_noticed_changes = Automatic.last_noticed_changes ;;

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

let set_gitpush_after_backup = Automatic.set_gitpush_after_backup ;;

let set_last_noticed_changes = Automatic.set_last_noticed_changes ;;

let to_concrete_object = Automatic.to_concrete_object ;;

let usual_compilable_files fw = File_watcher.usual_compilable_files (Automatic.parent fw) ;;
