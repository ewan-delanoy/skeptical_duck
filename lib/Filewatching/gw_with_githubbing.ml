(* 

#use"lib/Filewatching/gw_with_githubbing.ml";;

*)

(* Beginning of POR(Polymorphic Ocaml Record)-related code *)

module Background = struct 

let fw_space = Por_space_example.filewatching ;; 


let dir_for_backup_field =
  {Por_field_t.field_name = "dir_for_backup";
  field_type = "Dfa_root_t.t"; var_name = "backup_dir";
  default_value = "Dfa_root.of_line \"dummy\"";
  crobj_converters =
   Some ("Dfa_root.of_concrete_object", "Dfa_root.to_concrete_object")}
;;

let gitpush_after_backup_field =
  {Por_field_t.field_name = "gitpush_after_backup";
  field_type = "bool"; var_name = "gab"; default_value = "false";
  crobj_converters =
   Some
    ("Crobj_converter.bool_of_concrete_object",
     "Crobj_converter.bool_to_concrete_object")}
;;

let github_url_field =
  {Por_field_t.field_name = "github_url";
  field_type = "string"; var_name = "url"; default_value = "\"\"";
  crobj_converters =
   Some
    ("Crobj_converter.string_of_concrete_object",
     "Crobj_converter.string_to_concrete_object")}
;;


let encoding_protected_files_field =
  {Por_field_t.field_name = "encoding_protected_files";
  field_type = "(Dfn_rootless_t.t * Dfn_rootless_t.t) list";
  var_name = "protected_pairs"; default_value = "[]";
  crobj_converters =
   Some
    ("Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object",
     "Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object")}

;;



let gw_with_githubbing_extension =
  {Por_subclass_t.subclass_name = "gw_with_githubbing";
  subclass_fields = [dir_for_backup_field;gitpush_after_backup_field;github_url_field;encoding_protected_files_field];
  parent = Some "gw_with_batch_compilation"; 
  extensions_leading_here = [];
  has_restriction = false; 
  has_constructor = false} ;;
  
  
Por_space.add_extension fw_space "gw_with_batch_compilation" gw_with_githubbing_extension;;  
  


end ;; 

(* End of POR(Polymorphic Ocaml Record)-related code *)


module Private = struct

  let parent = Gw_poly.parent ;; 
  
  let set_parent = Gw_poly.set_parent ;;
  

  let usual_batch fw modnames = 
    let (new_parent,rejected_ones,accepted_ones) = 
      Gw_with_batch_compilation.usual_batch (parent fw) modnames in 
    (set_parent ~child:fw ~new_parent,rejected_ones,accepted_ones) ;; 
  
  let usual_extension fw_batch backup_dir gab git_url enc_files = 
      Gw_poly.extend_gw_with_batch_compilation_to_gw_with_githubbing 
      fw_batch
      ~dir_for_backup:backup_dir 
      ~gitpush_after_backup:gab
      ~github_url:git_url
      ~encoding_protected_files:enc_files ;;

  
  let of_gw_config_and_github_config fw_config github_config = usual_extension 
    (Gw_with_batch_compilation.of_configuration fw_config)
    (Gw_poly.dir_for_backup github_config) 
    (Gw_poly.gitpush_after_backup github_config) 
    (Gw_poly.github_url github_config)
    (Gw_poly.encoding_protected_files github_config);;


  let plunge_gw_config_with_github_config fw_config github_config= usual_extension 
      (Gw_with_batch_compilation.plunge_gw_configuration fw_config)
      (Gw_poly.dir_for_backup github_config) 
      (Gw_poly.gitpush_after_backup github_config) 
      (Gw_poly.github_url github_config)
      (Gw_poly.encoding_protected_files github_config);;
    
  exception Forget_modules_exn of Dfa_module_t.t  list ;;     

   module Transmit = struct 

    module Private = struct

      let commands_for_backup config diff=
         if Dircopy_diff.is_empty diff
         then ([],[])
         else 
         let source_dir = Gw_poly.root config 
         and destination_dir = Gw_poly.dir_for_backup config in 
         let s_destination=Dfa_root.connectable_to_subpath destination_dir in
         let created_ones=Image.image Dfn_rootless.to_line (Dircopy_diff.recently_created diff) in
         let temp2=List.filter_map
         (fun fn->
           if String.contains fn '/'
           then let dn=Cull_string.before_rightmost fn '/' in
                Some("mkdir -p "^s_destination^dn)
           else None 
          ) created_ones in
         let temp3=Ordered.sort Total_ordering.silex_for_strings temp2 in
         let s_source=Dfa_root.connectable_to_subpath source_dir in
         let temp4=Image.image(
            fun fn->
            "cp "^s_source^fn^" "^s_destination^(Cull_string.before_rightmost fn '/')
         ) created_ones in
         let changed_ones=Image.image Dfn_rootless.to_line (Dircopy_diff.recently_changed diff) in
         let temp5=Image.image(
            fun fn->
            "cp "^s_source^fn^" "^s_destination^fn
         ) changed_ones in
         let temp6=Image.image(
            fun fn->
            "git -C "^s_destination^" add "^fn
         ) (created_ones@changed_ones) in
         let temp7=Image.image(
            fun rl->
            let fn = Dfn_rootless.to_line rl in 
            "git -C "^s_destination^" rm "^fn
         ) (Dircopy_diff.recently_deleted diff) in
         let temp8= Image.image (
           fun (replacer,replacee) ->
             let s_replacer = Dfn_rootless.to_line  replacer 
             and s_backup_dir = Dfa_root.connectable_to_subpath destination_dir in 
             let s_full_path = s_backup_dir^(Dfn_rootless.to_line replacee) in 
             Unix_command.prefix_for_replacing_patterns^s_replacer^" "^s_full_path
         ) (Gw_poly.encoding_protected_files config) in 
         (temp3@temp4@temp5@temp8,temp6@temp7);;
      
      let backup_with_message config  diff msg=
        let destination_dir = Gw_poly.dir_for_backup config in 
        let (nongit_cmds,git_cmds)=commands_for_backup config diff in
        let s_destination=Dfa_root.connectable_to_subpath destination_dir in
        let _=Image.image Unix_command.uc nongit_cmds in
        let _=(
        if Gw_poly.gitpush_after_backup config
        then let cwd=Sys.getcwd() in
             Image.image Unix_command.uc
             (
             [Unix_command.cd s_destination]@   
             git_cmds@   
             [
               "git -C "^s_destination^" commit -m \""^msg^"\"";
               "git -C "^s_destination^" push"
             ]@
             [Unix_command.cd cwd]
             ) 
        else let cwd=Sys.getcwd() in
             Image.image Unix_command.uc
             (
             [Unix_command.cd s_destination]@   
             git_cmds@   
             [
               "git -C "^s_destination^" commit -m \""^msg^"\""
             ]@
             [Unix_command.cd cwd]
             ) 
        ) in
        ();;
      
      let backup config diff opt_msg=
        if Dircopy_diff.is_empty diff
        then (print_string "No recent changes to commit ...";flush stdout) 
        else 
        let msg=(
         match opt_msg with
          None->Dircopy_diff.explain diff
         |Some(msg0)->msg0) in
        backup_with_message config diff msg;;
        
      end ;; 
      
      let backup config diff opt_msg=
        Private.backup config diff opt_msg;;
      
        
         

   end ;;  


  let forget_modules fw mods = 
    let check = Gw_with_dependencies.check_module_sequence_for_forgettability (parent fw) mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let (new_parent,removed_files) = Gw_with_batch_compilation.forget_modules (parent fw) mods in 
    let descr = String.concat " , " (Image.image Dfa_module.to_line mods) in 
    let msg="delete "^descr in 
    let diff = Dircopy_diff.destroy Dircopy_diff.empty_one removed_files  in  
    let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in     
    set_parent ~child:fw ~new_parent ;;     

  let forget_nonmodular_rootlesses fw rootless_paths=
      let new_parent = Gw_with_batch_compilation.remove_files (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="delete "^descr in 
      let diff = Dircopy_diff.destroy Dircopy_diff.empty_one rootless_paths  in  
      let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in     
      set_parent ~child:fw ~new_parent ;;     
    
  
  let register_rootless_paths fw rootless_paths = 
      let new_parent = Gw_with_batch_compilation.register_rootless_paths (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="register "^descr in 
      let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
      let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in     
      set_parent ~child:fw ~new_parent ;;  

   

  let relocate_module_to fw mod_name new_subdir = 
      let (new_parent,(_,replacements)) = Gw_with_batch_compilation.relocate_module_to (parent fw) mod_name new_subdir in 
      let msg="move "^(Dfa_module.to_line mod_name)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let diff = Dircopy_diff.replace Dircopy_diff.empty_one replacements  in  
      let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in     
      set_parent ~child:fw ~new_parent ;; 

  let rename_module fw old_middle_name new_nonslashed_name = 
      let (new_parent,(_,(file_renamings,changed_files))) = Gw_with_batch_compilation.rename_module (parent fw) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
              " as "^(No_slashes.to_string new_nonslashed_name) in       
      let diff1 = Dircopy_diff.replace Dircopy_diff.empty_one file_renamings  in  
      let diff2 = Dircopy_diff.add_changes diff1  changed_files  in  
      let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff2 (Some msg) in     
      set_parent ~child:fw ~new_parent ;;    

  let rename_subdirectory_as fw (old_subdir,new_subdir) = 
    let (new_parent,(_,original_reps)) = Gw_with_batch_compilation.rename_subdirectory_as 
          (parent fw) (old_subdir,new_subdir) in 
    let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let diff = Dircopy_diff.replace Dircopy_diff.empty_one original_reps in   
    let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in     
    set_parent ~child:fw ~new_parent ;; 

  
  let replace_string fw old_s new_s = 
      let (parent1,(changed_modules_in_any_order,all_changed_files)) = 
      Gw_with_batch_compilation.replace_string (parent fw) old_s new_s  in 
      let new_parent = Gw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_s^" as "^new_s in 
      let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changed_files in 
      let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in 
      set_parent ~child:fw ~new_parent ;;

  let replace_value fw ((preceding_files,path),(old_v,new_v)) = 
        let (parent1,(changed_modules_in_any_order,all_changes)) = 
        Gw_with_batch_compilation.replace_value (parent fw) ((preceding_files,path),(old_v,new_v))  in 
        let new_parent = Gw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
        let msg="rename "^old_v^" as "^new_v in 
        let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changes in 
        let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff (Some msg) in 
        set_parent ~child:fw ~new_parent ;; 
 
   
  let usual_recompile fw opt_comment = 
    let (new_parent,(_changed_uc,changed_files)) = Gw_with_batch_compilation.usual_recompile (parent fw)  in 
    let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changed_files in 
    let _ = Transmit.backup (Gw_poly.to_gw_configuration fw) diff opt_comment in 
    set_parent ~child:fw ~new_parent ;;
    
end;;  
      

let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let of_gw_with_batch_compilation =Private.usual_extension ;;
let of_gw_config_and_github_config = Private.of_gw_config_and_github_config ;;
let plunge_gw_config_with_github_config = Private.plunge_gw_config_with_github_config ;;
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    
let usual_recompile = Private.usual_recompile ;;

