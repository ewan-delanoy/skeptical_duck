(* 

#use"lib/Filewatching/fw_syntactic_sugar.ml";;

*)

exception Rename_string_or_value_exn of string ;;


module Syntactic_sugar = struct 
   
module Private = struct 
   let display_number_of_modules fw_ref =
      let number_of_modules = List.length(Fwc_with_githubbing.Inherited.all_endinglesses (!fw_ref)) in 
      let msg = "\nThere are now "^(string_of_int number_of_modules)^" modules defined.\n" in 
      (print_string msg;flush stdout) ;; 

end ;; 
      
let forget fw_ref data = 
   let ref_for_modules = ref []
   and ref_for_paths = ref [] in 
   let _=List.iter (
      fun descr ->
      if String.contains descr '.'
      then ref_for_paths:= (Dfn_rootless.of_line descr)::(!ref_for_paths)
      else ref_for_modules:= (Dfa_module.of_line descr) ::(!ref_for_modules)
   ) data in
   let all_paths = List.rev(!ref_for_paths) 
   and all_modules =  List.rev(!ref_for_modules) in 
   let _=(if all_paths=[] then () else Fw_act_on_reference.forget_nonmodular_rootlesses fw_ref all_paths) in 
   let _=(if all_modules=[] then () else Fw_act_on_reference.forget_modules fw_ref all_modules) in 
   Private.display_number_of_modules fw_ref;;
   
   
let register_several fw_ref lines =
   let rootless_paths = Image.image Dfn_rootless.of_line lines in 
   let _ = Fw_act_on_reference.register_rootless_paths fw_ref  rootless_paths in 
   Private.display_number_of_modules fw_ref ;;
   
let register_one fw_ref line = 
   register_several fw_ref [line]  ;;
   
let relocate_module_to fw_ref old_module_name new_subdir=
   let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
   Fw_act_on_reference.relocate_module_to fw_ref mn new_subdir ;;
   
let rename_module fw_ref old_module_name new_name=
   let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
   let old_eless = Fwc_with_githubbing.Inherited.endingless_at_module (!fw_ref) mn in
   let old_middle_name = Dfn_endingless.to_middle old_eless in    
   let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
   Fw_act_on_reference.rename_module fw_ref old_middle_name new_nonslashed_name;; 
   
let rename_string_or_value fw_ref old_sov new_sov =
   if not(String.contains old_sov '.')
   then Fw_act_on_reference.replace_string fw_ref old_sov new_sov 
   else 
   let j_opt=Substring.leftmost_index_of_in_from_opt "." old_sov 1 in
   if j_opt= None
   then raise(Rename_string_or_value_exn(old_sov))
   else 
   let j = Option.get j_opt in 
   let module_name=Cull_string.beginning (j-1) old_sov in
   let endingless= Fwc_with_githubbing.Inherited.decipher_module (!fw_ref)  module_name 
   and path= Fwc_with_githubbing.Inherited.decipher_path (!fw_ref)  module_name in 
   let nm=Dfn_endingless.to_module endingless in
   let pre_temp2=(Fwc_with_githubbing.Inherited.ancestors_for_module (!fw_ref) nm)@[nm] in
   let temp2=Image.image (Fwc_with_githubbing.Inherited.endingless_at_module (!fw_ref)) pre_temp2 in
   let preceding_files=Image.image  (fun eless2->
                           Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
   ) temp2 in
   Fw_act_on_reference.replace_value fw_ref ((preceding_files,path),(old_sov,new_sov)) ;;       

let rename_subdirectory fw_ref old_subdirname new_subdir_short_name=
   let old_subdir = Fwc_with_githubbing.Inherited.find_subdir_from_suffix (!fw_ref) old_subdirname  in
   let new_subdir = Dfa_subdirectory.compute_long_subdir_name old_subdir new_subdir_short_name  in 
   Fw_act_on_reference.rename_subdirectory fw_ref old_subdir new_subdir ;;
   
   
end;;