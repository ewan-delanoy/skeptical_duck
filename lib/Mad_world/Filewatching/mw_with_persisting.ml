(* 

#use"lib/Mad_world/Filewatching/mw_with_persisting.ml";;

*)

module Private=struct

  let building_site =  Coma_constant.usual_build_subdir ;;

  let loadings (main_root,rootless_path_for_loadingsfile) (dirs,hms)=
      let path_for_loadingsfile = Dfn_rootless.to_line rootless_path_for_loadingsfile in 
      let s_root=Dfa_root.connectable_to_subpath main_root in
      let part1="\n(*\n #use\""^s_root^(path_for_loadingsfile)^"\";"^";\n*)\n\n" in
      let temp5=Image.image (
        fun sd->
        "#directory\""^s_root^(Dfa_subdirectory.connectable_to_subpath sd)^"\";"^";"
      ) (building_site::dirs) in
      let part2=String.concat "\n" temp5 
      and part3="\n\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
      let temp2=Image.image (
        function hm->
          let nm = Dfn_endingless.to_module hm in  
          let s=Cull_string.after_rightmost (Dfa_module.to_line nm) '/' in
          "#load\""^s^".cmo\";"^";"
      ) hms in
      let temp3="\n\n\n"::temp2 in
      let part4=String.concat "\n" temp3 
      and part5="\n\n\n" in
      part1^part2^part3^part4^part5;; 
          
    
    let instructions_for_merlinfile main_root dirs=
      let s_root=Dfa_root.connectable_to_subpath main_root in
      let temp1=Image.image 
        (fun sdir->"S "^s_root^(Dfa_subdirectory.connectable_to_subpath sdir) )
      dirs in
      let temp2=("B "^s_root^(Dfa_subdirectory.connectable_to_subpath building_site))::temp1 in
      "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

    let instructions_for_printersfile printer_equipped_types=
        let temp2=List.rev_map (
          function (x,compiled_correctly)->
          if compiled_correctly 
          then let modname=Dfn_endingless.to_module x in 
               "#install_printer "^(Dfa_module.capitalized_form modname)^".print_out;"^";"
          else ""
        ) printer_equipped_types in
        let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
        let part2=String.concat "\n" temp3 in
        part2;;  

    let save_loadingsfile (root,rootless_path_for_loadingsfile) (dirs,hms)=
       let s=loadings (root,rootless_path_for_loadingsfile)
        (dirs,hms)
       and lm=Dfn_join.root_to_rootless root rootless_path_for_loadingsfile in
       Io.overwrite_with (Dfn_full.to_absolute_path lm) s;;
    
    let save_merlinfile (root,rootless_path_for_merlinfile) dirs=
        let s=instructions_for_merlinfile root dirs 
        and lm=Dfn_join.root_to_rootless root rootless_path_for_merlinfile in
        Io.overwrite_with (Dfn_full.to_absolute_path lm) s;;
  
    let save_printersfile (root,rootless_path_for_printersfile) printer_equipped_types=
       let s=instructions_for_printersfile printer_equipped_types
       and lm=Dfn_join.root_to_rootless root rootless_path_for_printersfile in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Replace_inside.overwrite_between_markers_inside_file
       ~overwriter:s
       (beg_mark,end_mark)
       (Dfn_full.to_absolute_path lm);;
    
  
    let save_targetfile rootless_path_for_targetfile root_dir crobj_form=
      let s1=Crobj_parsing.unparse crobj_form in
      let lt=Dfn_join.root_to_rootless root_dir rootless_path_for_targetfile in
      Io.overwrite_with (Dfn_full.to_absolute_path lt) s1;;
    
    
    
    let write_all 
    (
      rootless_path_for_targetfile,
      rootless_path_for_loadingsfile,
      rootless_path_for_printersfile
      )
      (root_dir,elesses,crobj_form,directories,printer_equipped_types) = 
       (
        save_loadingsfile (root_dir,rootless_path_for_loadingsfile) (directories,elesses);
        save_targetfile rootless_path_for_targetfile root_dir crobj_form;
        save_printersfile (root_dir,rootless_path_for_printersfile) printer_equipped_types;
       );;

       


  let save_all cs=
      let root_dir = Mw_poly.root cs 
      and elesses = Mw_with_batch_compilation.up_to_date_elesses cs
      and crobj_form = Mw_poly.to_concrete_object cs 
      and directories = Mw_with_dependencies.all_subdirectories cs 
      and printer_equipped_types = Mw_with_batch_compilation.preq_types_with_extra_info cs 
        in
       write_all 
      (
        Coma_constant.rootless_path_for_targetfile,
        Coma_constant.rootless_path_for_loadingsfile,
        Coma_constant.rootless_path_for_printersfile
      )
      
	      (root_dir,elesses,crobj_form,directories,printer_equipped_types)
      ;;

    let read_persistent_version fw=
      let full_path=Dfn_join.root_to_rootless (Mw_poly.root fw)  Coma_constant.rootless_path_for_targetfile in
      let ap= Dfn_full.to_absolute_path full_path in
      let the_archive=Io.read_whole_file ap in
      let archived_object = Crobj_parsing.parse the_archive in 
      Mw_poly.of_concrete_object archived_object;;   


end ;;  


module Regress = struct 

let to_fw_configuration (Mw_with_persisting_t.Subclass fw)= 
   Mw_poly.to_fw_configuration fw ;;

let to_github_configuration (Mw_with_persisting_t.Subclass fw)= 
   Mw_poly.to_github_configuration fw ;;


end ;;

module Constructor = struct 

let of_fw_config_and_github_config fw_config github_config = 
  let fw_with_githubbing = 
    Mw_with_githubbing.of_fw_config_and_github_config fw_config github_config in 
  Mw_with_persisting_t.Subclass fw_with_githubbing ;;

let of_fw_with_batch_compilation fw =
  Mw_with_persisting_t.Subclass fw ;;

let plunge_fw_config_with_github_config fw_config github_config = 
  let fw_with_githubbing = 
     Mw_with_githubbing.plunge_fw_config_with_github_config fw_config github_config in 
  Mw_with_persisting_t.Subclass fw_with_githubbing ;;

end ;;  

 

let all_endinglesses (Mw_with_persisting_t.Subclass fw) =
  Mw_with_dependencies.all_endinglesses fw ;;

let ancestors_for_module (Mw_with_persisting_t.Subclass fw) mn=
  Mw_with_dependencies.ancestors_for_module fw mn;;

let below (Mw_with_persisting_t.Subclass fw) mn =
    Mw_with_dependencies.below fw mn ;;

let check_that_no_change_has_occurred (Mw_with_persisting_t.Subclass fw)= 
    Mw_file_watcher.check_that_no_change_has_occurred fw ;;

let clean_debug_dir (Mw_with_persisting_t.Subclass fw) = Mw_with_batch_compilation.clean_debug_dir fw ;;


let clean_exec_dir (Mw_with_persisting_t.Subclass fw) = Mw_with_batch_compilation.clean_exec_dir fw ;;

let decipher_module (Mw_with_persisting_t.Subclass fw) mn=
    Mw_with_dependencies.decipher_module fw mn;;

let decipher_path (Mw_with_persisting_t.Subclass fw) path=
    Mw_with_dependencies.decipher_path fw path;;    

let direct_fathers_for_module (Mw_with_persisting_t.Subclass fw) mn =
    Mw_with_dependencies.direct_fathers_for_module fw mn ;;

let directly_below (Mw_with_persisting_t.Subclass fw) mn =
    Mw_with_dependencies.directly_below fw mn ;;

let duplicate_module (Mw_with_persisting_t.Subclass fw) old_t1 old_t2=
  Mw_with_dependencies.duplicate_module fw old_t1 old_t2 ;;

let endingless_at_module (Mw_with_persisting_t.Subclass fw) mn=
  Mw_with_dependencies.endingless_at_module fw mn;;

let find_subdir_from_suffix (Mw_with_persisting_t.Subclass fw) possibly_slashed_suffix =
    Mw_with_dependencies.find_subdir_from_suffix fw possibly_slashed_suffix ;; 

let forget_modules (Mw_with_persisting_t.Subclass fw) mods= 
Mw_with_persisting_t.Subclass(Mw_with_githubbing.forget_modules fw mods);;

let forget_nonmodular_rootlesses (Mw_with_persisting_t.Subclass fw) rls= 
Mw_with_persisting_t.Subclass(
  Mw_with_githubbing.forget_nonmodular_rootlesses fw rls);;

let gitpush_after_backup (Mw_with_persisting_t.Subclass fw) = 
   fw.Mw_poly_t.gitpush_after_backup ;;

let latest_changes (Mw_with_persisting_t.Subclass fw) =
    Mw_with_archives.latest_changes fw ;;

let list_values_from_module (Mw_with_persisting_t.Subclass fw) mn =
    Mw_with_dependencies.list_values_from_module fw mn ;;

let modules_using_value (Mw_with_persisting_t.Subclass fw) value_name =
    Mw_with_dependencies.modules_using_value fw value_name ;;

let noncompilable_files (Mw_with_persisting_t.Subclass fw) = 
   Mw_with_archives.noncompilable_files fw ;;

let number_of_modules (Mw_with_persisting_t.Subclass fw) = 
   Mw_with_dependencies.number_of_modules fw ;;

let register_rootless_paths (Mw_with_persisting_t.Subclass fw) rootless_paths = 
  Mw_with_persisting_t.Subclass(
    Mw_with_githubbing.register_rootless_paths fw rootless_paths);;

let relocate_module_to (Mw_with_persisting_t.Subclass fw) mod_name new_subdir = 
  Mw_with_persisting_t.Subclass(
      Mw_with_githubbing.relocate_module_to fw mod_name new_subdir);;

let rename_module (Mw_with_persisting_t.Subclass fw) old_middle_name new_nonslashed_name = 
    Mw_with_persisting_t.Subclass(
      Mw_with_githubbing.rename_module fw old_middle_name new_nonslashed_name);;

let rename_subdirectory_as (Mw_with_persisting_t.Subclass fw) subdir_pair = 
    Mw_with_persisting_t.Subclass(
      Mw_with_githubbing.rename_subdirectory_as fw subdir_pair);;

let replace_string (Mw_with_persisting_t.Subclass fw) old_s new_s = 
        Mw_with_persisting_t.Subclass(
          Mw_with_githubbing.replace_string fw old_s new_s);;

let replace_value (Mw_with_persisting_t.Subclass fw) data = 
  Mw_with_persisting_t.Subclass(
    Mw_with_githubbing.replace_value fw data);;

let root (Mw_with_persisting_t.Subclass fw) = fw.Mw_poly_t.root ;;

let set_gitpush_after_backup (Mw_with_persisting_t.Subclass fw) gab= 
Mw_with_persisting_t.Subclass(
  Mw_poly.set_gitpush_after_backup fw gab);;

let show_value_occurrences (Mw_with_persisting_t.Subclass fw) mn = 
  Mw_with_dependencies.show_value_occurrences fw mn ;;  

let start_debugging (Mw_with_persisting_t.Subclass fw) =
  Mw_with_batch_compilation.start_debugging fw ;;

let start_executing (Mw_with_persisting_t.Subclass fw) short_path=
 Mw_with_batch_compilation.start_executing fw short_path ;;

let overwrite_file_if_it_exists (Mw_with_persisting_t.Subclass fw) pair =  
   Mw_with_dependencies.overwrite_file_if_it_exists fw pair ;;

let persist (Mw_with_persisting_t.Subclass fw)= Private.save_all fw;;

let read_persistent_version (Mw_with_persisting_t.Subclass fw) = 
     Mw_with_persisting_t.Subclass(Private.read_persistent_version fw);;
   
  
let usual_compilable_files (Mw_with_persisting_t.Subclass fw) = 
    Mw_with_archives.usual_compilable_files fw ;;
 

let usual_recompile (Mw_with_persisting_t.Subclass fw) opt_comment=
Mw_with_persisting_t.Subclass(
  Mw_with_githubbing.usual_recompile fw opt_comment);;




