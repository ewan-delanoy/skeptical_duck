(* 

#use"lib/Filewatching/Fw_classes/fwc_with_githubbing.ml";;

*)

type t = Fwg_with_githubbing.t ;;

module Inherited = struct

  module Aunt = Fwc_with_dependencies ;;

  let uncle fw = Fwg_with_batch_compilation.parent 
      (Fwg_with_githubbing.parent fw) ;;

  module Tapistry = Aunt.Inherited ;;  

  let machen fw_deps github_config= 
    let fw_batch = Fwg_with_batch_compilation.make fw_deps [] in 
    Fwg_with_githubbing.make fw_batch github_config ;;

  

  (* module Parent = Fwc_with_batch_compilation ;;

  module Ancestry = Parent.Inherited ;;
  let parent = Fwg_with_githubbing.parent ;;
  
  let set_parent fw fw_batch =
    Fwg_with_githubbing.make fw_batch 
       (Fwg_with_githubbing.github_configuration fw) ;; *)
  
  
  let all_endinglesses fw = Aunt.all_endinglesses (uncle fw) ;;
  let all_moduled_mlx_files fw = Aunt.all_moduled_mlx_files (uncle fw) ;;
  let all_subdirectories fw = Aunt.all_subdirectories (uncle fw) ;;
  let ancestors_for_module fw = Aunt.ancestors_for_module (uncle fw) ;;
  let below fw = Aunt.below (uncle fw) ;;
  
  let check_module_sequence_for_forgettability fw = 
    Aunt.check_module_sequence_for_forgettability (uncle fw) ;;
  
  let check_that_no_change_has_occurred fw = 
    Tapistry.check_that_no_change_has_occurred(uncle fw)  ;;  
    
  
  let decipher_module fw = Aunt.decipher_module (uncle fw) ;;
  let decipher_path fw = Aunt.decipher_path (uncle fw) ;;
  let dep_ordered_modules fw = Aunt.dep_ordered_modules (uncle fw) ;;
  let directly_below fw = Aunt.directly_below (uncle fw) ;;
  let direct_fathers_for_module fw = Aunt.direct_fathers_for_module (uncle fw) ;;
  let duplicate_module fw = Aunt.duplicate_module (uncle fw) ;;
  let endingless_at_module fw = Aunt.endingless_at_module (uncle fw) ;;
  let find_subdir_from_suffix fw = Aunt.find_subdir_from_suffix (uncle fw) ;;
  
  
  let gitpush_after_backup fw = Fwc_github_configuration.gitpush_after_backup (Fwg_with_githubbing.github_configuration fw) ;;
  
  let ignored_files fw = Tapistry.ignored_files (uncle fw) ;;
  let ignored_subdirectories fw = Tapistry.ignored_subdirectories (uncle fw) ;;
  
  
  let  latest_changes fw = Tapistry.latest_changes(uncle fw)  ;;    
  
  let list_values_from_module fw = Aunt.list_values_from_module(uncle fw)  ;;
  
  
  let modules_using_value fw = Aunt.modules_using_value (uncle fw) ;; 
  
  let  noncompilable_files fw = Tapistry.noncompilable_files(uncle fw)  ;; 
  
  let number_of_modules fw = Aunt.number_of_modules (uncle fw) ;;
  
  let root fw = Tapistry.root (uncle fw) ;;
  
  let set_fw_with_dependencies fw fw_deps = 
    machen fw_deps 
       (Fwg_with_githubbing.github_configuration fw) ;;
  let set_gitpush_after_backup fw gab = 
    let old_github_config = Fwg_with_githubbing.github_configuration fw in 
    let new_github_config = Fwc_github_configuration.set_gitpush_after_backup old_github_config gab in 
    machen (uncle fw) new_github_config ;;
  
  
  
  let  show_value_occurrences fw = Aunt.show_value_occurrences(uncle fw)  ;;
  
  let test_equality fw1 fw2 = 
    let gc = Fwg_with_githubbing.github_configuration in 
    let get_github_root = (fun fw->Fwc_github_configuration.root(gc fw))
    and get_dir_for_backup = (fun fw->Fwc_github_configuration.dir_for_backup(gc fw))
    and get_github_url = (fun fw->Fwc_github_configuration.github_url(gc fw))
    and get_encoding_protected_files = (fun fw->Fwc_github_configuration.encoding_protected_files(gc fw)) in 
  
    (
      Tapistry.test_equality (uncle fw1) (uncle fw2)
    )
    @
    (
      List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld)
      [
        "github_root",((get_github_root fw1)=(get_github_root fw2));
        "dir_for_backup",((get_dir_for_backup fw1)=(get_dir_for_backup fw2));
        "github_url",((get_github_url fw1)=(get_github_url fw2));
        "encoding_protected_files",((get_encoding_protected_files fw1)=(get_encoding_protected_files fw2));
      ]
    ) ;;
  
    
  let test_for_admissibility fw = Tapistry.test_for_admissibility (uncle fw) ;;
  
  let to_fw_configuration fw = Tapistry.to_fw_configuration (uncle fw) ;;
  
  let to_fw_with_archives fw = Tapistry.to_fw_with_archives (uncle fw) ;;
  let to_fw_with_dependencies fw = uncle fw ;;

  
  let  usual_compilable_files fw = Tapistry.usual_compilable_files(uncle fw)  ;; 
  
  
end ;;  


module Crobj = struct 
  let salt = "Fwc_with_githubbing." ;;
  let label_for_parent = salt ^ "parent" ;;
  let label_for_github_config  = salt ^ "github_config" ;;
  
  
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    Fwg_with_githubbing.make 
     (Fwc_with_batch_compilation.Crobj.of_concrete_object (g label_for_parent))
     (Fwc_github_configuration.Crobj.of_concrete_object (g label_for_github_config))
    ;;
  
  let to_concrete_object fw = 
   let items =  
   [
     label_for_parent, Fwc_with_batch_compilation.Crobj.to_concrete_object ( Fwg_with_githubbing.parent fw ) ;
     label_for_github_config, Fwc_github_configuration.Crobj.to_concrete_object ( Fwg_with_githubbing.github_configuration fw ) ;
   ] in 
   Concrete_object_t.Record items ;;
  
  
end;; 
  
   

module Private = struct 

let parent = Fwg_with_githubbing.parent ;; 
let github_configuration = Fwg_with_githubbing.github_configuration ;;
let make = Fwg_with_githubbing.make;;


let usual_extension fw_batch backup_dir gab git_url enc_files = 
  make fw_batch (Fwc_github_configuration.make 
 ~v_root:(Fwc_with_batch_compilation.Inherited.root fw_batch)
  ~v_dir_for_backup:backup_dir
~v_gitpush_after_backup:gab
~v_github_url:git_url
~v_encoding_protected_files:enc_files) ;;


  let set_parent ~child ~new_parent = 
   make new_parent child ;;
  
  


  let plunge_fw_config_with_github_config fw_config github_config= 
   Inherited.machen (Fwc_with_dependencies.plunge_fw_configuration fw_config)
   github_config ;;
  
    
   let backup fw diff opt_msg = Fwc_github_configuration.backup  
     (Fwg_with_githubbing.github_configuration fw) diff opt_msg ;;

  exception Forget_modules_exn of Dfa_module_t.t  list ;;     

  let forget_modules fw mods = 
    let check = Inherited.check_module_sequence_for_forgettability fw mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let (new_parent,removed_files) = Fwc_with_batch_compilation.forget_modules (parent fw) mods in 
    let descr = String.concat " , " (Image.image Dfa_module.to_line mods) in 
    let msg="delete "^descr in 
    let diff = Dircopy_diff.destroy Dircopy_diff.empty_one removed_files  in  
    let _ = backup fw diff (Some msg) in     
    set_parent ~child:(github_configuration fw) ~new_parent ;;     

  let forget_nonmodular_rootlesses fw rootless_paths=
      let new_parent = Fwc_with_batch_compilation.remove_files (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="delete "^descr in 
      let diff = Dircopy_diff.destroy Dircopy_diff.empty_one rootless_paths  in  
      let _ = backup fw diff (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;;     
    
  let inspect_and_update fw opt_comment = 
        let fw_batch = parent fw in 
        let fw_with_deps = Fwc_with_batch_compilation.Private.parent fw_batch in
        let (marcia_baila,(_,_,changed_files))
                   =Fwc_with_dependencies.inspect_and_update fw_with_deps in 
        let new_parent = Fwc_with_batch_compilation.Inherited.set_parent fw_batch marcia_baila  in 
        let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changed_files in 
        let _ = backup fw diff opt_comment in 
        set_parent ~child:
          (github_configuration fw) ~new_parent ;;  

  let register_rootless_paths fw rootless_paths = 
      let new_parent = Fwc_with_batch_compilation.register_rootless_paths (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="register "^descr in 
      let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
      let _ = backup fw diff (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;;  
  
   let register_rootless_paths fw rootless_paths = 
        let old_fw_deps = Inherited.to_fw_with_dependencies fw in 
        let (new_fw_deps,_)=
             Fwc_with_dependencies.register_rootless_paths old_fw_deps rootless_paths in 
        let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
        let msg="register "^descr in 
        let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
        let _ = backup fw diff (Some msg) in     
        Inherited.set_fw_with_dependencies fw new_fw_deps ;;    
   


  let relocate_module_to fw mod_name new_subdir = 
      let (new_parent,(_,replacements)) = Fwc_with_batch_compilation.relocate_module_to (parent fw) mod_name new_subdir in 
      let msg="move "^(Dfa_module.to_line mod_name)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let diff = Dircopy_diff.replace Dircopy_diff.empty_one replacements  in  
      let _ = backup fw diff (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;; 

  let rename_module fw old_middle_name new_nonslashed_name = 
      let (new_parent,(_,(file_renamings,changed_files))) = Fwc_with_batch_compilation.rename_module (parent fw) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
              " as "^(No_slashes.to_string new_nonslashed_name) in       
      let diff1 = Dircopy_diff.replace Dircopy_diff.empty_one file_renamings  in  
      let diff2 = Dircopy_diff.add_changes diff1  changed_files  in  
      let _ = backup fw diff2 (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;;    

  let rename_subdirectory_as fw (old_subdir,new_subdir) = 
    let (new_parent,(_,original_reps)) = Fwc_with_batch_compilation.rename_subdirectory_as 
          (parent fw) (old_subdir,new_subdir) in 
    let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let diff = Dircopy_diff.replace Dircopy_diff.empty_one original_reps in   
    let _ = backup fw diff (Some msg) in     
    set_parent ~child:(github_configuration fw) ~new_parent ;; 

  
  let replace_string fw old_s new_s = 
      let (parent1,(changed_modules_in_any_order,all_changed_files)) = 
      Fwc_with_batch_compilation.replace_string (parent fw) old_s new_s  in 
      let new_parent = Fwc_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_s^" as "^new_s in 
      let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changed_files in 
      let _ = backup fw diff (Some msg) in 
      set_parent ~child:(github_configuration fw) ~new_parent ;;

let replace_value fw
      ((preceding_files,path),(old_v,new_v)) = 
      let old_fw_deps = Inherited.uncle fw in 
      let (new_fw_deps,(_u_changes,all_changes)) = 
        Fwc_with_dependencies.replace_value old_fw_deps 
         ((preceding_files,path),(old_v,new_v)) in 
      let msg="rename "^old_v^" as "^new_v in 
      let diff = Dircopy_diff.add_changes 
      Dircopy_diff.empty_one all_changes in 
      let _ = backup fw diff (Some msg) in 
      Inherited.machen 
       new_fw_deps (github_configuration fw)  ;;
 
   

      

 

    

end ;; 


let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let github_configuration = Fwg_with_githubbing.github_configuration ;;
let inspect_and_update = Private.inspect_and_update ;;
let of_fw_with_batch_compilation =Private.usual_extension ;;

let plunge_fw_config_with_github_config = Private.plunge_fw_config_with_github_config ;;
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    


