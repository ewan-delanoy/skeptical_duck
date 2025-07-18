(*

#use"lib/Filewatching/Fw_classes/fwc_with_modular_infrastructure.ml";;

*)

type t = Fwg_with_modular_infrastructure.t ;; 

exception Absent_module of string;;
exception Duplicate_module_already_exists of string;;
exception Find_subdir_from_suffix_exn of string * (Dfa_subdirectory_t.t list) ;;
exception Module_not_found_exn of string ;;

module Inherited = struct 

  module Ancestry = Fwc_with_small_details.Inherited ;;
  let parent = Fwg_with_modular_infrastructure.parent ;;
  
  let check_that_no_change_has_occurred fw = Ancestry.check_that_no_change_has_occurred(parent fw)  ;;  

  let ignored_files fw = Ancestry.ignored_files (parent fw) ;;
  let ignored_subdirectories fw = Ancestry.ignored_subdirectories (parent fw) ;;

  let  latest_changes fw = Ancestry.latest_changes(parent fw)  ;;  

  let  noncompilable_files fw = Ancestry.noncompilable_files(parent fw)  ;;  

  let root fw = Ancestry.root (parent fw) ;;

  let test_equality fw1 fw2 = 
    (
      Ancestry.test_equality (parent fw1) (parent fw2)
    )
    @
    (
      List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld)
      [
        
        "dependencies",((Fwg_with_modular_infrastructure.dependencies fw1)=(Fwg_with_modular_infrastructure.dependencies fw2))
        
      ]
    ) ;; 

  let test_for_admissibility fw = Ancestry.test_for_admissibility (parent fw) ;;
  
  let to_fw_configuration fw = Ancestry.to_fw_configuration (parent fw) ;;

  let to_fw_with_archives fw = Ancestry.to_fw_with_archives (parent fw) ;;
  let  usual_compilable_files fw = Ancestry.usual_compilable_files(parent fw)  ;;  

end ;;  

module Crobj = struct 

  module Private = struct 

    let parent fw = Fwg_with_modular_infrastructure.parent fw ;;

  end ;;   

  let salt = "Fwc_with_dependencies." ;;
  let label_for_parent = salt ^ "parent" ;;
  let label_for_dependencies  = salt ^ "dependencies" ;;
      

  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    Fwg_with_modular_infrastructure.make 
    (Fwc_with_small_details.Crobj.of_concrete_object (g label_for_parent))
    (Fw_modular_infrastructure.Crobj.of_concrete_object (g label_for_dependencies))
    ;;

    let to_concrete_object fw = 
      let items =  
      [
           label_for_parent, Fwc_with_small_details.Crobj.to_concrete_object ( Private.parent fw ) ;
           label_for_dependencies, Fw_modular_infrastructure.Crobj.to_concrete_object (Fwg_with_modular_infrastructure.dependencies fw ) ;
      ] in 
      Concrete_object_t.Record items ;;

end ;;  


module Private = struct

  let parent fw = Fwg_with_modular_infrastructure.parent fw ;;
 let usual_extension fw_with_archives = 
    Fwg_with_modular_infrastructure.make 
    fw_with_archives Fw_modular_infrastructure.starter;;






module Core = struct 

  let parent = Fwg_with_modular_infrastructure.parent ;;
  let dependencies = Fwg_with_modular_infrastructure.dependencies ;;
  let make fw_dets deps= Fwg_with_modular_infrastructure.make fw_dets  deps ;;


let forget_modules old_fw mods_to_be_erased =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.forget_modules old_fw_dets mods_to_be_erased in 
 let _ = Fw_modular_infrastructure.ReactOnReference.forget_modules  deps_ref mods_to_be_erased in 
 (make new_fw_dets(!deps_ref),extra);;

let inspect_and_update old_fw  =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.inspect_and_update old_fw_dets  in 
 let _ = Fw_modular_infrastructure.ReactOnReference.inspect_and_update extra deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;

let of_configuration config =  
 let deps_ref = ref (Fw_modular_infrastructure.starter) in 
 let new_fw_dets = Fwc_with_small_details.of_configuration config in 
 let _ = Fw_modular_infrastructure.ReactOnReference.of_configuration new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref));;

let of_configuration_and_list (config,files) =  
 let deps_ref = ref (Fw_modular_infrastructure.starter) in 
 let new_fw_dets = Fwc_with_small_details.of_configuration_and_list (config,files) in 
 let _ = Fw_modular_infrastructure.ReactOnReference.of_configuration_and_list new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref));;

let overwrite_file_if_it_exists old_fw (rootless,new_content) =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.overwrite_file_if_it_exists old_fw_dets (rootless,new_content) in 
 let _ = Fw_modular_infrastructure.ReactOnReference.overwrite_file_if_it_exists extra new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;

let plunge_fw_configuration config =  
 let deps_ref = ref (Fw_modular_infrastructure.starter) in 
 let new_fw_dets = Fwc_with_small_details.plunge_fw_configuration config in 
 let _ = Fw_modular_infrastructure.ReactOnReference.plunge_fw_configuration  deps_ref  in 
 (make new_fw_dets(!deps_ref));;

let register_rootless_paths old_fw rootlesses =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.register_rootless_paths old_fw_dets rootlesses in 
 let _ = Fw_modular_infrastructure.ReactOnReference.register_rootless_paths extra new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;

let relocate_module_to old_fw (mod_name,new_subdir) =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.relocate_module_to old_fw_dets (mod_name,new_subdir) in 
 let _ = Fw_modular_infrastructure.ReactOnReference.relocate_module_to extra new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;

let remove_files old_fw removed_rootless_paths =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.remove_files old_fw_dets removed_rootless_paths in 
 let _ = Fw_modular_infrastructure.ReactOnReference.remove_files extra new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.rename_module_on_filename_level_and_in_files old_fw_dets triple in 
 let _ = Fw_modular_infrastructure.ReactOnReference.rename_module_on_filename_level_and_in_files extra new_fw_dets deps_ref triple in 
 (make new_fw_dets(!deps_ref),extra);;

let rename_subdirectory_as old_fw sdir_pair =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.rename_subdirectory_as old_fw_dets sdir_pair in 
 let _ = Fw_modular_infrastructure.ReactOnReference.rename_subdirectory_as extra new_fw_dets deps_ref sdir_pair in 
 (make new_fw_dets(!deps_ref),extra);;

let replace_string old_fw (replacee,replacer) =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.replace_string old_fw_dets (replacee,replacer) in 
 let _ = Fw_modular_infrastructure.ReactOnReference.replace_string extra new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;

let replace_value old_fw (replacee,replacer) =  
 let old_fw_dets = parent old_fw 
 and deps_ref = ref (dependencies old_fw) in 
 let (new_fw_dets,extra) = Fwc_with_small_details.replace_value old_fw_dets (replacee,replacer) in 
 let _ = Fw_modular_infrastructure.ReactOnReference.replace_value extra new_fw_dets deps_ref  in 
 (make new_fw_dets(!deps_ref),extra);;




end ;;

  
  let mod_details fw = (Fwg_with_modular_infrastructure.dependencies fw).Fw_modular_infrastructure_t.modularized_details ;;

  let mod_order fw = (Fwg_with_modular_infrastructure.dependencies fw).Fw_modular_infrastructure_t.order ;;

  let mod_libs fw = (Fwg_with_modular_infrastructure.dependencies fw).Fw_modular_infrastructure_t.needed_libs ;;

  let mod_printables fw = (Fwg_with_modular_infrastructure.dependencies fw).Fw_modular_infrastructure_t.all_printables ;;

  let mod_subdirs fw = (Fwg_with_modular_infrastructure.dependencies fw).Fw_modular_infrastructure_t.all_subdirectories ;;

  let details_for_module  fw mn = try List.assoc mn (mod_details fw) with 
   Not_found -> raise(Module_not_found_exn(Dfa_module.to_line mn));;
  let check_ending_on_module fw edg  mn=
   if edg=Fw_module_details.principal_ending (details_for_module fw mn)
   then true 
   else 
   if edg=Dfa_ocaml_ending_t.Mli
   then Fw_module_details.mli_present (details_for_module fw mn) 
   else false;;
  let modules_with_their_ancestors fw l=
   let temp1=List.filter_map (
     fun (nm,_)->if List.mem nm l then Some nm else None
     ) (mod_order fw )   in 
   let temp2=Image.image (
     fun nm->
       (snd (List.assoc nm (mod_order fw)))@[nm] 
   ) temp1 in 
   let temp3=List.flatten temp2 in 
   List_again.nonredundant_version temp3;;
  
  let root fw = Inherited.root  (fw) ;;

  let subdir_for_module fw mn =Fw_module_details.subdirectory (details_for_module fw mn) ;;

   let endingless_at_module cs mn=
   Dfn_endingless_t.J(
        root cs,
        subdir_for_module cs mn,
        mn
    );;

  let check_ending_in_at_module edg fw mn=
    if edg= Fw_module_details.principal_ending (details_for_module fw mn)
    then true 
    else 
    if edg=Dfa_ocaml_ending_t.Mli
    then Fw_module_details.mli_present (details_for_module fw mn)
    else false;;


  let acolytes_at_module fw mn=
    let eless = endingless_at_module fw mn in
    List.filter_map (fun 
    edg->
      if check_ending_in_at_module edg fw mn
      then Some(Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending edg))
      else None
    ) Dfa_ocaml_ending.all ;;

  let all_moduled_mlx_files fw=
   let mods=Image.image fst (mod_order fw) in
   List.flatten(Image.image(acolytes_at_module fw) mods);;                
       
 let all_moduled_mlx_paths cs=Image.image Dfn_full.to_absolute_path (all_moduled_mlx_files cs);;  

let archived_mlx_paths fw = List.filter_map (
   fun rl -> let edg = Dfn_rootless.to_ending rl in 
     if List.mem edg Dfa_ending.all_ocaml_endings 
     then let full = Dfn_join.root_to_rootless (root fw) rl in 
           Some(Dfn_full.to_absolute_path full)
     else None   
) (Fwc_with_small_details.Inherited.archived_files (parent fw));;


let all_mlx_paths fw = (archived_mlx_paths fw) @ (all_moduled_mlx_paths fw) ;;

 let list_values_from_module fw module_name=
 let temp1=all_mlx_paths fw in
 let temp2=Image.image (fun ap->
  let ttemp1=Look_for_module_names.list_values_from_module_in_file module_name ap in
  Set_of_strings.image (fun x->(x,ap) ) ttemp1
  ) temp1 in
 let temp3=List.flatten temp2 in
 let temp4=Image.image fst temp3 in 
 let temp5=Ordered.sort Total_ordering.lex_for_strings temp4 in
 Image.image (
    fun x->(x,List.filter_map(
      fun (y,ap)->if y=x then Some(ap) else None
    ) temp3)
 ) temp5 ;;


let show_value_occurrences fw t=
 let m=String.length(Dfa_root.connectable_to_subpath (root fw)) in
 let temp1=all_mlx_paths fw in
 let temp2=Image.image (fun ap->
  let text = Io.read_whole_file ap in   
  let temp3=Substring.occurrences_of_in t text in 
  let mname=Cull_string.cobeginning(m)(Absolute_path.to_string ap) in
  Image.image (fun occ_idx ->
    let center_line_idx = (Strung.number_of_lines_before text occ_idx)+1 in 
    let closeup = Lines_in_text.closeup_around_index text occ_idx in 
    mname^", line "^(string_of_int center_line_idx)^" :\n"^closeup
  ) temp3
) temp1 in
 let temp4=List.flatten temp2 in
 let temp5=String.concat "\n\n\n" (""::temp4@[""]) in 
 print_string temp5;; 

let number_of_modules fw = List.length (mod_order fw) ;;

let below fw eless=
  let mods_in_order = mod_order fw in 
  let mn0=Dfn_endingless.to_module eless  in
  List.filter_map(fun (mn,_)->
    if List.mem mn0 (snd(List.assoc mn mods_in_order))
    then Some(mn)
    else None) mods_in_order;;

let below_several fw mods = 
  let all_mods_in_order = Image.image fst (mod_order fw) in 
  let below_module = (fun mn->below fw (endingless_at_module fw mn)) in 
  let temp1 = List.flatten(mods :: (Image.image below_module mods)) in
  let all_deps = List.filter (fun mn->List.mem mn temp1) all_mods_in_order in 
  let (mods_in_order,new_deps) = List.partition (fun mn->List.mem mn mods) all_deps in 
  (all_deps,new_deps,mods_in_order) ;;

let decipher_path fw x=Find_suitable_ending.find_file_location 
  (root fw) (mod_subdirs fw) x;;

let decipher_module fw capitalized_or_not_x=
  let x=String.uncapitalize_ascii capitalized_or_not_x in 
  let s=Cull_string.before_rightmost_possibly_all x '.' in
  match (List.find_map(
      fun edg->
      let t=s^(Dfa_ending.connectable_to_modulename edg) in 
      try(Some(decipher_path fw t)) with _->None
  ) Dfa_ending.all_ocaml_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->
    let rootless_path = Dfn_common.decompose_absolute_path_using_root ap (root fw) in 
    let mlx = Dfn_join.root_to_rootless (root fw) rootless_path in 
    Dfn_full.to_endingless mlx ;;
  
  let above fw mn = snd (List.assoc mn (mod_order fw));;

  
  let below fw mn0 =
        let ordered_data = mod_order fw in 
        List.filter_map(fun (mn,_)->
            let ancestors_for_mn = snd (List.assoc mn ordered_data) in 
            if List.mem mn0 ancestors_for_mn
            then Some(mn)
            else None) ordered_data;;  
   
  let directly_below fw mn0 =
    let ordered_data = mod_order fw in 
    List.filter_map(fun (mn,_)->
      let fathers_for_mn = fst (List.assoc mn ordered_data) in 
        if List.mem mn0 fathers_for_mn
        then Some(mn)
        else None) ordered_data;;  


  let modules_using_value fw value_name =
    List.filter_map (fun (mn,_)->
      let eless=endingless_at_module fw mn
      and pr_end=Fw_module_details.principal_ending (details_for_module fw mn) in
      let mlx=Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending pr_end) in
      let ap=Dfn_full.to_absolute_path mlx in
      if Substring.is_a_substring_of 
             value_name (Io.read_whole_file ap)
      then Some eless
      else None ) (mod_order fw);;
          
  
  let find_subdir_from_suffix fw possibly_slashed_suffix =
    let suffix = Cull_string.trim_slashes_on_the_right possibly_slashed_suffix  in
    let temp1 = List.filter (
    fun subdir -> Substring.is_a_substring_of suffix (Dfa_subdirectory.without_trailing_slash subdir) 
    ) (mod_subdirs fw) in 
    let test_for_minimality = (fun subdir1->
     List.for_all (fun subdir2 ->
        if subdir2 = subdir1 then true else 
        not(Dfa_subdirectory.begins_with subdir1 subdir2) 
     ) temp1
    ) in 
    let temp2 = List.filter test_for_minimality temp1 in 
    if List.length(temp2)<>1
    then raise(Find_subdir_from_suffix_exn(suffix,temp2))
    else let (Dfa_subdirectory_t.SD container) = List.hd temp2 in 
         let j1 = Option.get(Substring.leftmost_index_of_in_from_opt suffix container 1) in 
         let j2 = j1 + (String.length suffix) -1 in 
        Dfa_subdirectory.of_line(Cull_string.beginning j2 container);;

    let duplicate_module fw old_t1 old_t2=
        let t1=String.uncapitalize_ascii old_t1
        and t2=String.uncapitalize_ascii old_t2 in 
        let ap1=decipher_path fw t1 in
        let s_ap1=Absolute_path.to_string ap1 in
        let s_ending = Cull_string.after_rightmost s_ap1 '.' in 
        let s_ap2=(Cull_string.before_rightmost_possibly_all s_ap1 '/')^"/"^t2^"."^s_ending in
        if Sys.file_exists s_ap2
        then raise(Duplicate_module_already_exists(t2))
        else 
        let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
        let ap2=Absolute_path.of_string s_ap2 in
        let _ =  (
          if s_ending = "ml"
          then Use_directive_in_initial_comment.replace_with_usual (root fw) ap2) in 
        (*
           
        On a Mac, this was 
        Unix_command.uc ("open -a \"/Applications/Sublime Text.app\" "^s_ap2);; 

        *)  
        Unix_command.uc ("xdg-open "^s_ap2);;      

    let all_moduled_ml_absolute_paths fw =  
        List.filter_map (fun (mn,_)->
          if not(check_ending_in_at_module Dfa_ocaml_ending_t.Ml fw mn)
          then None
          else 
          let hm=endingless_at_module fw mn in
          let mlx=Dfn_join.to_ending hm Dfa_ending.ml in
          Some(Dfn_full.to_absolute_path mlx)
        ) (mod_order fw);;

    let all_mlx_files fw=
        let mods=Image.image fst (mod_order fw) in
        List.flatten(Image.image(acolytes_at_module fw) mods);;    

    let all_endinglesses fw=
        Image.image (fun (mn,_)->endingless_at_module fw mn) (mod_order fw);;    
        
    
let test_for_foreign root ap =
  match (
    try Some(Dfn_common.decompose_absolute_path_using_root ap root) with 
             _->None 
  ) with 
  None -> true 
  |Some(rootless) ->
     (
      not(List.mem
         (Dfn_rootless.to_ending rootless) Dfa_ending.endings_for_readable_files)   
     )
     ;;


let check_module_sequence_for_forgettability fw l=
 let modules_below = List.filter_map (
   fun (mn,(_,ancestors_for_mn)) -> 
    if List.exists (fun mn2->
       List.mem mn2 ancestors_for_mn
     ) l 
    then Some mn 
    else None 
 )(mod_order fw) in 
 List.filter (fun mn->not(List.mem mn l)) modules_below;;      

end ;;

let above = Private.above ;;
let acolytes_at_module = Private.acolytes_at_module ;;
let all_endinglesses = Private.all_endinglesses ;;
let all_moduled_ml_absolute_paths = Private.all_moduled_ml_absolute_paths ;;
let all_moduled_mlx_files = Private.all_moduled_mlx_files ;;
let all_subdirectories fw = Private.mod_subdirs fw;;
let ancestors_for_module fw mn = snd (List.assoc mn (Private.mod_order fw)) ;;
let below = Private.below ;;
let below_several = Private.below_several ;;
let check_ending_on_module = Private.check_ending_on_module ;;
let check_module_sequence_for_forgettability = Private.check_module_sequence_for_forgettability ;;
let decipher_module = Private.decipher_module ;;
let decipher_path = Private.decipher_path ;;
let dep_ordered_modules fw = Image.image fst (Private.mod_order fw);;
let directly_below = Private.directly_below ;;
let direct_fathers_for_module fw mn = fst (List.assoc mn (Private.mod_order fw)) ;;
let duplicate_module = Private.duplicate_module ;;
let endingless_at_module = Private.endingless_at_module ;;
let find_subdir_from_suffix = Private.find_subdir_from_suffix ;;
let forget_modules = Private.Core.forget_modules ;;
let inspect_and_update = Private.Core.inspect_and_update ;;
let list_values_from_module = Private.list_values_from_module ;; 
let modules_using_value = Private.modules_using_value ;;
let modules_with_their_ancestors = Private.modules_with_their_ancestors ;;
let needed_libs_for_module fw mn = List.assoc mn (Private.mod_libs fw) ;;
let number_of_modules = Private.number_of_modules ;;
let of_configuration = Private.Core.of_configuration ;;
let of_configuration_and_list = Private.Core.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.Core.overwrite_file_if_it_exists ;;
let parent = Fwg_with_modular_infrastructure.parent ;;
let plunge_fw_configuration = Private.Core.plunge_fw_configuration ;;
let principal_ending_for_module fw mn = Fw_module_details.principal_ending (Private.details_for_module fw mn) ;;
let printer_equipped_types fw = Private.mod_printables fw;;
let register_rootless_paths = Private.Core.register_rootless_paths ;;
let relocate_module_to = Private.Core.relocate_module_to ;;
let remove_files = Private.Core.remove_files ;;
let rename_module_on_filename_level_and_in_files = Private.Core.rename_module_on_filename_level_and_in_files ;;
let rename_subdirectory_as = Private.Core.rename_subdirectory_as ;;
let replace_string = Private.Core.replace_string ;;
let replace_value = Private.Core.replace_value ;;
let show_value_occurrences = Private.show_value_occurrences ;;
let subdir_for_module fw mn = Fw_module_details.subdirectory (Private.details_for_module fw mn) ;;
let usual_compilable_files fw = Fwc_with_small_details.usual_compilable_files (Private.parent fw) ;;
