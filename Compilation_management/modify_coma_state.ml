
(* 

#use"Compilation_management/modify_coma_state.ml";;

*)



module Physical = struct 

let forget_module cs mod_name=
   let new_fw=Fw_wrapper.forget_module (cs.Coma_state_t.frontier_with_unix_world) mod_name in   
   Coma_state_field.set_frontier_with_unix_world cs new_fw ;;

let forget_rootless_path cs rootless_path=
   let new_fw=Fw_wrapper.remove_compilable_files (cs.Coma_state_t.frontier_with_unix_world) [rootless_path] in   
   Coma_state_field.set_frontier_with_unix_world cs new_fw ;;   


let recompile cs =
   let (new_fw,(changed_rootlesses,_))=Fw_wrapper.inspect_and_update (cs.Coma_state_t.frontier_with_unix_world) in   
   let new_cs= Coma_state_field.set_frontier_with_unix_world cs new_fw in 
   (new_cs,changed_rootlesses);;

let refresh (root,backup_dir,g_after_b) =
    let _=(More_unix.create_subdirs_and_fill_files_if_necessary root
      Coma_constant.git_ignored_subdirectories 
        Coma_constant.conventional_files_with_usual_content) in 
   let config = Fw_configuration.constructor (root,backup_dir,g_after_b) in 
   let fw = Fw_initialize.init config in
   let cs0 = Coma_state_field.empty_one root backup_dir g_after_b in  
   Coma_state_field.set_frontier_with_unix_world cs0 fw;;

let register_rootless_path cs rp=
   let new_fw=Fw_wrapper.register_rootless_path (cs.Coma_state_t.frontier_with_unix_world) rp in   
   Coma_state_field.set_frontier_with_unix_world cs new_fw ;;

let relocate_module_to cs mod_name new_subdir=
   let new_fw=Fw_wrapper.relocate_module_to (cs.Coma_state_t.frontier_with_unix_world) mod_name new_subdir in   
   Coma_state_field.set_frontier_with_unix_world cs new_fw ;;

let rename_module cs old_middle_name new_nonslashed_name=
  let old_nm=Dfn_middle.to_module old_middle_name in
  let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in  
  let old_acolytes=Coma_state.acolytes_at_module cs old_nm in
  let separated_acolytes_below=Option.filter_and_unpack(
    fun mn->
     if List.mem old_nm (Coma_state.ancestors_at_module cs mn)
    then Some(Image.image (Dfn_full.to_rootless) (Coma_state.acolytes_at_module cs mn))
    else None
) (Coma_state.ordered_list_of_modules cs) in
  let all_acolytes_below=List.flatten separated_acolytes_below in
  let old_acolyte_paths=Image.image Dfn_full.to_rootless old_acolytes in 
  let old_fw = Coma_state.frontier_with_unix_world cs in 
  let new_fw = Fw_wrapper.rename_module old_fw old_acolyte_paths new_nm all_acolytes_below in 
  Coma_state.set_frontier_with_unix_world cs new_fw ;;

let rename_subdirectory cs (old_subdir,new_subdir)=
   let new_fw=Fw_wrapper.rename_subdirectory_as (cs.Coma_state_t.frontier_with_unix_world) (old_subdir,new_subdir) in   
   Coma_state_field.set_frontier_with_unix_world cs new_fw ;;


exception Rename_string_or_value_exn of string ;;


let rename_string_or_value cs old_sov new_sov =
   let old_fw = Coma_state.frontier_with_unix_world cs in 
   let (new_fw,(changed_compilable_files,changed_noncompilable_files))=(
      if not(String.contains old_sov '.')
      then let (fw,(changed_c_files,changed_nc_files))= Fw_wrapper.replace_string old_fw (old_sov,new_sov) in 
           (fw,(changed_c_files,changed_nc_files))
      else 
           let j=Substring.leftmost_index_of_in "." old_sov in
           if j<0 
           then raise(Rename_string_or_value_exn(old_sov))
           else let module_name=Cull_string.beginning (j-1) old_sov in
                let endingless=Coma_state.decipher_module cs  module_name 
                and path=Coma_state.decipher_path cs  module_name in 
                let nm=Dfn_endingless.to_module endingless in
                let pre_temp2=(Coma_state.ancestors_at_module cs nm)@[nm] in
                let temp2=Image.image (Coma_state.endingless_at_module cs) pre_temp2 in
                let preceding_files=Image.image  (fun eless2->
   	               Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
                ) temp2 in
                Fw_wrapper.replace_value old_fw (preceding_files,path) (old_sov,new_sov)
   ) in 
   (Coma_state.set_frontier_with_unix_world cs new_fw,(changed_compilable_files,changed_noncompilable_files));;       



end;;

module Internal = struct

exception ModuleWithDependenciesDuringForgetting of Dfn_endingless_t.t *
            Dfa_module_t.t list;;

let forget_module cs mn=
  let old_endingless = Coma_state.endingless_at_module cs mn in   
  let bel=Coma_state.below cs old_endingless in 
  if bel<>[]
  then raise(ModuleWithDependenciesDuringForgetting(old_endingless,bel))
  else 
  let (cs2,rootless_paths)=Coma_state.unregister_module_on_monitored_modules  cs old_endingless in
  let new_dirs=Coma_state.compute_subdirectories_list cs2  in
  let sfn=Dfa_module.to_line mn in
  let _=Image.image
               (fun edg->
                let cmd="rm -f _build/"^sfn^edg in
                Unix_command.uc(cmd))
               [".cm*";".d.cm*";".caml_debuggable"] in
  let cs3=Coma_state.set_directories cs2 new_dirs in 
  let ordered_paths=Set_of_strings.forget_order(Set_of_strings.safe_set(rootless_paths)) in
  let diff=
      Dircopy_diff.constructor
      (Recently_deleted.of_string_list ordered_paths)
      (Recently_changed.of_string_list [])
      (Recently_created.of_string_list []) in
   (cs3,diff);;    

let forget_rootless_path cs rootless_path=
   let the_root = Coma_state.root cs in 
   let full_path = Dfn_join.root_to_rootless the_root rootless_path in  
   let cut_ap=Dfn_rootless.to_line rootless_path in
   let diff=
    Dircopy_diff.constructor
    (Recently_deleted.of_string_list [cut_ap])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list []) in 
   let (cs2,new_dirs)=Coma_state.unregister_mlx_file_on_targets the_root cs full_path in   
   let cs3=Coma_state.set_directories cs2 new_dirs in 
   (cs3,diff);; 


let recompile (cs,changed_rootlesses) = 
   let new_fw = cs.Coma_state_t.frontier_with_unix_world in 
   let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun nm->
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (Coma_state.rootless_paths_at_module cs nm))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun mname->
    match Coma_state.Late_Recompilation.quick_update (!cs_walker) (new_fw,changed_rootlesses) mname with
    None->()
    |Some(pr_modif_time,mli_modif_time,direct_fathers)->
    (
    declare_changed(mname);
    cs_walker:=Coma_state.set_principal_mt_at_module (!cs_walker) mname pr_modif_time;
    cs_walker:=Coma_state.set_mli_mt_at_module (!cs_walker) mname mli_modif_time;
    cs_walker:=Coma_state.set_direct_fathers_at_module (!cs_walker) mname direct_fathers;
    cs_walker:=Coma_state.set_product_up_to_date_at_module (!cs_walker) mname false;
    )
)(Coma_state.ordered_list_of_modules cs) in
let changed_modules=List.rev(!ref_for_changed_modules) in
let diff_veiler =(fun paths->
       Dircopy_diff.constructor
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list paths)
    (Recently_created.of_string_list [])
   ) in    
if changed_modules=[] then (!cs_walker,diff_veiler []) else
let _=Coma_state.PrivateThree.announce_changed_modules changed_modules in
let ((cs2,nms_to_be_updated),rootless_paths)= 
 (Coma_state.PrivateThree.put_md_list_back_in_order false 
  (!cs_walker) changed_modules,
(!ref_for_changed_shortpaths))  in   
   if nms_to_be_updated=[] then (cs2,diff_veiler []) else
   (
   let new_dirs=Coma_state.compute_subdirectories_list cs2  in
   let (cs3,rejected_pairs,accepted_pairs)=
       Coma_state.Ocaml_target_making.usual_feydeau cs2 nms_to_be_updated in 
   let rejected_mns=Image.image snd rejected_pairs in  
   let new_preqt=Image.image(
        fun (mn,_)->(mn,not(List.mem mn rejected_mns))
      )  (Coma_state_field.preq_types cs3) in   
   let cs4=Coma_state_field.set_directories cs3 new_dirs in 
   let cs5=Coma_state_field.set_preq_types cs4 new_preqt in 
   let changed_paths=Ordered.sort 
      Total_ordering.silex_for_strings rootless_paths in 
   let the_diff = diff_veiler changed_paths in    
     (cs5,the_diff) 
   );;


let refresh cs = 
        let dir =Coma_state_field.root cs 
        and backup_dir = Coma_state_field.backup_dir cs in 
        let fw1 = cs.Coma_state_t.frontier_with_unix_world in 
        let temp1=Fw_wrapper.nonspecial_absolute_paths fw1 in
        let temp2=Coma_state.Target_system_creation.clean_list_of_files dir temp1 in
        let temp3=Coma_state.Target_system_creation.compute_dependencies temp2 in
        let (failures,cs1)=Coma_state.Target_system_creation.from_prepared_list cs temp3 in
        let pre_preqt=Coma_state.printer_equipped_types_from_data cs1 in
        let l_mod=Coma_state_field.ordered_list_of_modules cs1 in 
        let (cs2,rejected_pairs,_)=
          Coma_state.Ocaml_target_making.usual_feydeau 
          cs1 l_mod in
        let rejected_endinglesses=Image.image snd rejected_pairs in 
        let new_ptypes=Image.image (fun mn->(mn,not(List.mem mn rejected_endinglesses))) pre_preqt in 
        let new_dirs=Coma_state.compute_subdirectories_list cs2 in
        let new_diff=Coma_state.Target_system_creation.delchacre_from_scratch (dir,backup_dir) cs2 in
        let cs3=Coma_state_field.set_directories cs2 new_dirs in 
        let cs4=Coma_state_field.set_preq_types cs3 new_ptypes in
        (cs4,new_diff)    ;;


let register_rootless_path cs rp_line=
  let rootless_path = Dfn_rootless.of_line rp_line in 
  let mlx=Dfn_join.root_to_rootless (Coma_state.root cs) rootless_path in
  let diff=
    Dircopy_diff.constructor
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list [rp_line]) in
  let cs2=Coma_state.register_mlx_file cs mlx in 
  (cs2,diff);;

let relocate_module_to cs mn new_subdir=
  let old_endingless = Coma_state.endingless_at_module cs mn in  
  let old_subdir = Dfn_endingless.to_subdirectory old_endingless in 
  let root_dir = Coma_state.root cs in 
  let mn=Dfn_endingless.to_module old_endingless in
  let old_acolytes= Coma_state.acolytes_at_module cs mn in
  let new_acolytes=Image.image 
    (fun mlx->Dfn_full.relocate mlx new_subdir) old_acolytes in
  let new_name=Dfn_full.to_endingless
   (List.hd new_acolytes) in
  let s_root=Dfa_root.connectable_to_subpath root_dir in     
  let old_middle = Dfn_endingless.to_middle old_endingless in
    let _=Unix_command.uc
     ("rm -f "^s_root^"_build/"^(Dfn_middle.to_line old_middle)^".cm* ") in
  let principal_mt=Coma_state.md_compute_modification_time new_name 
                         (Coma_state.principal_ending_at_module cs mn)
  and mli_mt=Coma_state.md_compute_modification_time new_name Dfa_ending.mli in
  let s_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
  let cs2=Coma_state.set_subdir_at_module cs mn new_subdir in 
  let cs3=Coma_state.set_principal_mt_at_module cs2 mn principal_mt in 
  let cs4=Coma_state.set_mli_mt_at_module cs3 mn mli_mt in 
  let old_preq_types = Coma_state.preq_types cs4 in 
  let new_preq_types=Image.image (fun (h,bowl)->
     (Dfn_endingless.rename_endsubdirectory (old_subdir,s_subdir) h,bowl)) old_preq_types in 
  let cs5=Coma_state.set_preq_types cs4 new_preq_types in 
  cs5;;   


let rename_module cs2 old_middle_name new_nonslashed_name=
  let root_dir=Coma_state.root cs2 in 
  let old_nm=Dfn_middle.to_module old_middle_name in
  let s_root=Dfa_root.connectable_to_subpath root_dir in   
  let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.build_subdir) in  
  let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in
  let old_acolytes=Coma_state.acolytes_at_module cs2 old_nm in
  let new_acolytes=Image.image (
    fun (Dfn_full_t.J(r,s,m,e))->Dfn_full_t.J(r,s,new_nm,e)
  ) old_acolytes in 
  let new_eless=Dfn_full.to_endingless(List.hd new_acolytes) in
  let _=Unix_command.uc
      ("rm -f "^s_root^s_build_dir^
      (Dfa_module.to_line old_nm)^
      ".cm* ") in     
  let principal_mt=Coma_state.md_compute_modification_time new_eless (Coma_state.principal_ending_at_module cs2 old_nm)
  and mli_mt=Coma_state.md_compute_modification_time new_eless Dfa_ending.mli in
  let cs3=Coma_state_field.change_one_module_name cs2 old_nm new_nm in 
  let cs4=Coma_state.set_principal_mt_at_module cs3 new_nm principal_mt in 
  let cs5=Coma_state.set_mli_mt_at_module cs4 new_nm mli_mt in 
  let cs6=Coma_state.set_product_up_to_date_at_module cs5 new_nm false in 
  let replacer=Image.image(function x->if x=old_nm then new_nm else x) in
  let old_eless = Dfn_join.root_to_middle root_dir old_middle_name in
  let eless_replacer=(fun x->if x=old_eless then new_eless else x) in 
  let old_preq_types=Coma_state.preq_types cs6 in 
  let new_preq_types=Image.image (fun (h,bowl)->(eless_replacer h,bowl)) old_preq_types in 
  let cs7=Coma_state.set_preq_types cs6 new_preq_types in 
  let cs_walker=ref(cs7) in 
  let _=List.iter(fun mn->
      let old_dirfath=Coma_state.direct_fathers_at_module (!cs_walker) mn
      and old_ancestors=Coma_state.ancestors_at_module (!cs_walker) mn in
      (
      cs_walker:=(Coma_state.set_direct_fathers_at_module (!cs_walker) mn (replacer old_dirfath)) ;
      cs_walker:=(Coma_state.set_ancestors_at_module (!cs_walker) mn (replacer old_ancestors)); 
      )
  )(Coma_state.follows_it cs2 old_nm) in
  let cs8=(!cs_walker) in    
  let (cs9,_)=recompile (cs8,[]) in 
  cs9;;

let rename_subdirectory cs old_subdir new_subdir=
  let rename_in_sd=(fun sd -> 
     match Dfa_subdirectory.soak (old_subdir,new_subdir) sd with 
     Some(new_sd) -> new_sd 
     |None -> sd
   ) in 
  let cs1=Coma_state_field.modify_all_subdirs cs rename_in_sd in 
  let cs2=Coma_state_field.modify_all_needed_dirs cs1 rename_in_sd in 
   let new_dirs=Image.image rename_in_sd (Coma_state.directories cs2)
   and new_peqt=Image.image (fun (eless,is_compiled_correctly)->
       let final_eless = (
           match Dfn_endingless.soak (old_subdir,new_subdir) eless with 
        Some(new_eless) -> new_eless
        |None -> eless
       ) in 
       (final_eless,is_compiled_correctly)
   )(Coma_state.preq_types cs2) in
   let cs3= Coma_state.set_directories cs2 new_dirs in 
   let cs4= Coma_state.set_preq_types cs3 new_peqt in 
   cs4;; 


let rename_string_or_value cs = recompile (cs,[]);; 

end;;

module Physical_followed_by_internal = struct

let forget_module cs mod_name= 
  let cs2=Physical.forget_module cs mod_name  in
  Internal.forget_module cs2 mod_name;;

let forget_rootless_path cs rootless_path= 
  let cs2=Physical.forget_rootless_path cs rootless_path  in
  Internal.forget_rootless_path cs2 rootless_path;;


let recompile cs = 
  let (cs2,changed_rootlesses)=Physical.recompile cs  in
  Internal.recompile (cs2,changed_rootlesses);;
  
let refresh cs =
   let cs2=Physical.refresh 
     (Coma_state.root cs,Coma_state.backup_dir cs,Coma_state.gitpush_after_backup cs)  in
   Internal.refresh cs2;;

let register_rootless_path cs rp_line= 
   let cs2=Physical.register_rootless_path cs (Dfn_rootless.of_line rp_line) in
   Internal.register_rootless_path cs2 rp_line;;

let relocate_module_to cs mod_name new_subdir= 
  let cs2=Physical.relocate_module_to cs mod_name  new_subdir  in
  Internal.relocate_module_to cs2 mod_name  new_subdir;;


let rename_module cs old_middle_name new_nonslashed_name=
   let cs2=Physical.rename_module cs old_middle_name new_nonslashed_name in
   Internal.rename_module cs2 old_middle_name new_nonslashed_name;;

let rename_subdirectory cs old_subdir new_subdir=
   let cs2=Physical.rename_subdirectory cs (old_subdir,new_subdir) in
   Internal.rename_subdirectory cs2 old_subdir new_subdir;;

let rename_string_or_value cs old_sov new_sov =
   let (cs2,_)=Physical.rename_string_or_value cs old_sov new_sov in
   Internal.rename_string_or_value cs2;;

end;;



module After_checking = struct

      let forget_module cs mod_name=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.forget_module cs mod_name;; 

      let forget_rootless_path cs rootless_path=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.forget_rootless_path cs rootless_path;;    

      (* No check needed before recompiling *)

      (* No check needed before refreshing *)

      let register_rootless_path cs rp=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.register_rootless_path cs rp;; 

      let relocate_module_to cs old_module new_subdir=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.relocate_module_to cs old_module new_subdir;; 

      let rename_subdirectory  cs old_subdir new_subdir=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_subdirectory  cs old_subdir new_subdir;; 

      let rename_module cs old_middle_name new_nonslashed_name=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_module cs old_middle_name new_nonslashed_name;; 

      let rename_string_or_value cs old_sov new_sov=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_string_or_value cs old_sov new_sov;; 

end;;

module And_backup = struct

      module Private = struct

            let backup cs diff opt= 
            if not(Dircopy_diff.is_empty diff) 
            then Reflect_change_in_github.backup
                  (Coma_state.root cs,Coma_state.backup_dir cs,Coma_state.gitpush_after_backup cs) 
                  diff opt
            else (print_string "No recent changes to commit ...";flush stdout);;

      end;;    

      let forget_module cs mod_name=
         let (cs2,diff)=After_checking.forget_module cs mod_name in 
         let _=Private.backup cs2 diff None in 
         cs2;; 

      let forget_rootless_path cs rootless_path=
         let (cs2,diff)=After_checking.forget_rootless_path cs rootless_path in 
         let _=Private.backup cs2 diff None in 
         cs2;; 


      let recompile cs opt_comment=
         let (cs2,diff)=Physical_followed_by_internal.recompile cs  in 
         let _=Private.backup cs2 diff opt_comment in 
         cs2;; 

      (* No backup during refresh *)   

      let register_rootless_path cs x=
         let (cs2,diff)=After_checking.register_rootless_path cs x  in 
         let msg="register "^x in 
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

      let relocate_module_to cs old_module new_subdir=
         let cs2=After_checking.relocate_module_to cs old_module new_subdir  in
         let msg="move "^(Dfa_module.to_line old_module)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
         Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 


      let rename_module cs old_middle_name new_nonslashed_name=
         let cs2=After_checking.rename_module cs old_middle_name new_nonslashed_name  in 
         let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
                 " as "^(No_slashes.to_string new_nonslashed_name) in       
         Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

      let rename_subdirectory  cs old_subdir new_subdir=
         let cs2=After_checking.rename_subdirectory  cs old_subdir new_subdir  in 
         let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
                    " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
         Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

      let rename_string_or_value cs old_sov new_sov=
         let (cs2,diff)=After_checking.rename_string_or_value cs old_sov new_sov  in 
         let msg="rename "^old_sov^" as "^new_sov in 
         Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

end;;


module And_save = struct 

      let forget_module cs mod_name=
         let cs2=And_backup.forget_module cs mod_name in 
         let _=Save_coma_state.save cs2 in 
         cs2;;

      let forget_rootless_path cs rootless_path=
         let cs2=And_backup.forget_rootless_path cs rootless_path in 
         let _=Save_coma_state.save cs2 in 
         cs2;;

      let internet_access cs bowl=   
         let cs2=Coma_state_field.set_push_after_backup cs bowl in 
         let _=Save_coma_state.save cs2 in 
         cs2;;
      
      let recompile cs opt_comment=
         let cs2=And_backup.recompile cs opt_comment in 
         let _=Save_coma_state.save cs2 in 
         cs2;;
      

      let refresh cs =
         let (cs2,_)=Physical_followed_by_internal.refresh cs  in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

      let register_rootless_path cs rootless_path=
         let cs2=And_backup.register_rootless_path cs rootless_path in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

      let relocate_module_to cs old_module new_subdir=
      let cs2 = And_backup.relocate_module_to cs old_module new_subdir in 
      let _=Save_coma_state.save cs2 in 
      cs2;;   

      let rename_subdirectory cs old_subdir new_subdir=
         let cs2=And_backup.rename_subdirectory cs old_subdir new_subdir in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  


      let rename_module cs old_middle_name new_nonslashed_name=
         let cs2=And_backup.rename_module cs old_middle_name new_nonslashed_name in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  


      let rename_string_or_value cs old_sov new_sov=
         let cs2=And_backup.rename_string_or_value cs old_sov new_sov in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

end ;;

module Reference = struct 

      let forget_module pcs mod_name=
         let new_cs = And_save.forget_module (!pcs) mod_name in 
         pcs:=new_cs;;

      let forget_rootless_path pcs rootless_path=
         let new_cs = And_save.forget_rootless_path (!pcs) rootless_path in 
         pcs:=new_cs;; 

      let initialize pcs =
      let new_cs = Coma_state.read_persistent_version (!pcs) in 
      pcs:=new_cs;;

      let initialize_if_empty pcs =
         if Coma_state.system_size (!pcs)  = 0 
         then initialize pcs;;

      let internet_access pcs bowl=
         let new_cs = And_save.internet_access (!pcs) bowl in 
          pcs:=new_cs;;

      let recompile pcs opt_comment=
         let new_cs = And_save.recompile (!pcs) opt_comment in 
         pcs:=new_cs;;


      let refresh pcs =
         let new_cs = And_save.refresh (!pcs)  in 
         pcs:=new_cs;;

      let register_rootless_path pcs rootless_path=
         let new_cs = And_save.register_rootless_path (!pcs) rootless_path in 
         pcs:=new_cs;;


      let relocate_module_to pcs old_module new_subdir=
         let new_cs = And_save.relocate_module_to (!pcs) old_module new_subdir in 
         pcs:=new_cs;;  


      let rename_subdirectory pcs old_subdir new_subdir=
         let new_cs = And_save.rename_subdirectory (!pcs) old_subdir new_subdir in 
         pcs:=new_cs;;
         

      let rename_module pcs old_middle_name new_nonslashed_name=
         let new_cs = And_save.rename_module (!pcs) old_middle_name new_nonslashed_name in 
         pcs:=new_cs;;


      let rename_string_or_value pcs old_sov new_sov=
         let new_cs = And_save.rename_string_or_value (!pcs) old_sov new_sov in 
         pcs:=new_cs;;


end ;;


module Syntactic_sugar = struct 

let rename_module cs_ref old_module_name new_name=
   let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
   let old_eless = Coma_state.endingless_at_module (!cs_ref) mn in
   let old_middle_name = Dfn_endingless.to_middle old_eless in    
   let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
   Reference.rename_module cs_ref old_middle_name new_nonslashed_name;; 

let relocate_module_to cs_ref old_module_name new_subdir=
    let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
    Reference.relocate_module_to cs_ref mn new_subdir ;;

let forget cs text = 
      if String.contains text '.'
      then Reference.forget_rootless_path cs (Dfn_rootless.of_line text)
      else Reference.forget_module cs (Dfa_module.of_line text) ;;

let rename_subdirectory cs_ref old_subdirname new_subdir_short_name=
    let old_subdir = Coma_state.find_subdir_from_suffix (!cs_ref) old_subdirname  in
    let new_subdir = Coma_state.compute_long_subdir_name (!cs_ref) old_subdir new_subdir_short_name  in 
    Reference.rename_subdirectory cs_ref old_subdir new_subdir ;;


end;;

