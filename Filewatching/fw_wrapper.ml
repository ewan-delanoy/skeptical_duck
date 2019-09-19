(*

#use"Filewatching/fw_wrapper.ml";;



*)

module Private = struct 


let remove_watched_files rootless_paths fw=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.watched_files = List.filter (fun (path,_,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.watched_files)  
   };;

let remove_special_watched_files rootless_paths fw=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.special_watched_files = List.filter (fun (path,_,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.special_watched_files)  
   };;



end ;;


