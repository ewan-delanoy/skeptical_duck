(* 

#use"Compilation_management/modify_coma_state.ml";;

*)



module Physical = struct 

   let forget_modules cs mod_names=
      let new_fw=
         Fw_with_small_details.forget_modules (cs.Coma_state_t.frontier_with_unix_world) mod_names in   
      Coma_state.set_frontier_with_unix_world cs new_fw;;
   
   let forget_rootless_paths cs rootless_paths=
      let new_fw=Fw_with_small_details.remove_files (cs.Coma_state_t.frontier_with_unix_world) rootless_paths in   
      Coma_state.set_frontier_with_unix_world cs new_fw ;;   
   

   let recompile cs =
      let (new_fw,(changed_archived_compilables,changed_usual_compilables,changed_noncompilables))
         =Fw_with_small_details.inspect_and_update (cs.Coma_state_t.frontier_with_unix_world) in   
      let new_cs= Coma_state.set_frontier_with_unix_world cs new_fw in 
      (new_cs,changed_archived_compilables,changed_usual_compilables,changed_noncompilables);;
   
   let refresh config =
      let root = config.Fw_configuration_t.root in 
      let _=(More_unix.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) in 
      let fw = Fw_with_small_details.of_configuration config in
      let cs0 = Coma_state.empty_one config in  
      Coma_state.set_frontier_with_unix_world cs0 fw;;
   
   let register_rootless_paths cs rps=
      let (new_fw,(ac_paths,uc_paths,nc_paths))=Fw_with_small_details.register_rootless_paths (cs.Coma_state_t.frontier_with_unix_world) rps in   
      (Coma_state.set_frontier_with_unix_world cs new_fw,ac_paths,uc_paths) ;;
   
   let relocate_module_to cs mod_name new_subdir=
      let new_fw=Fw_with_small_details.relocate_module_to cs.Coma_state_t.frontier_with_unix_world (mod_name,new_subdir) in   
      Coma_state.set_frontier_with_unix_world cs new_fw ;;
   
   let rename_module cs old_middle_name new_nonslashed_name=
     let old_nm=Dfn_middle.to_module old_middle_name in
     let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in  
     let separated_acolytes_below=Option.filter_and_unpack(
       fun mn->
        if List.mem old_nm (Coma_state.ancestors_at_module cs mn)
       then Some(Image.image (Dfn_full.to_rootless) (Coma_state.acolytes_at_module cs mn))
       else None
   ) (Coma_state.ordered_list_of_modules cs) in
     let all_acolytes_below=List.flatten separated_acolytes_below in
     let old_fw = Coma_state.frontier_with_unix_world cs in 
     let (new_fw,changed_dependencies) = Fw_with_small_details.rename_module_on_filename_level_and_in_files old_fw (old_nm,new_nm,all_acolytes_below) in 
     (Coma_state.set_frontier_with_unix_world cs new_fw,changed_dependencies) ;;
   
   let rename_subdirectory cs (old_subdir,new_subdir)=
      let new_fw=Fw_with_small_details.rename_subdirectory_as (cs.Coma_state_t.frontier_with_unix_world) (old_subdir,new_subdir) in   
      Coma_state.set_frontier_with_unix_world cs new_fw ;;
   
   
   exception Rename_string_or_value_exn of string ;;
   
   
   let rename_string_or_value cs old_sov new_sov =
      let old_fw = Coma_state.frontier_with_unix_world cs in 
      let (new_fw,(changed_ac_files,changed_uc_files,changed_noncompilable_files))=(
         if not(String.contains old_sov '.')
         then let (fw,(changed_ac_files,changed_uc_files,changed_nc_files))= Fw_with_small_details.replace_string old_fw (old_sov,new_sov) in 
              (fw,(changed_ac_files,changed_uc_files,changed_nc_files))
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
                   Fw_with_small_details.replace_value old_fw ((preceding_files,path),(old_sov,new_sov))
      ) in 
      (Coma_state.set_frontier_with_unix_world cs new_fw,(changed_ac_files,changed_uc_files,changed_noncompilable_files));;       
   
   
   
   end;;
   
   module Internal = struct
   
   let forget_modules cs mns =
     let old_endinglesses = Image.image (Coma_state.endingless_at_module cs) mns in   
     let cs2=Coma_state.unregister_modules  cs old_endinglesses in
     let new_dirs=Coma_state.compute_subdirectories_list cs2  in 
     let temp1 = Image.image Dfa_module.to_line mns in 
     let temp2 = Cartesian.product temp1 [".cm*";".d.cm*";".caml_debuggable"] in 
     let _=Image.image
                  (fun (mname,edg)->
                   let cmd="rm -f _build/"^mname^edg in
                   Unix_command.uc(cmd))
                  temp2 in
     let cs3=Coma_state.set_directories cs2 new_dirs in 
     cs3;;    
   
   
   let forget_rootless_paths cs rootless_paths=
      let compilable_paths = List.filter Dfn_rootless.is_compilable rootless_paths in 
      let the_root = Coma_state.root cs in 
      let full_paths = Image.image (Dfn_join.root_to_rootless the_root) compilable_paths in  
      Coma_state.unregister_mlx_files cs full_paths ;; 
   
   
   let recompile (cs,changed_ac,changed_uc,changed_noncompilables) = 
      let new_fw = cs.Coma_state_t.frontier_with_unix_world in 
      let ref_for_changed_modules=ref[] 
     and ref_for_changed_shortpaths=ref[] in
     let declare_changed=(fun nm->
       ref_for_changed_modules:=nm::(!ref_for_changed_modules);
       ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                           (Coma_state.rootless_lines_at_module cs nm))
       ) in
     let cs_walker=ref(cs) in   
     let _=List.iter (fun mname->
       match Coma_state.Late_Recompilation.quick_update (!cs_walker) (new_fw,changed_uc) mname with
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
   let _=Coma_state.PrivateThree.announce_changed_archived_compilables changed_ac in
   let _=Coma_state.PrivateThree.announce_changed_noncompilables changed_noncompilables in
   let changed_modules=List.rev(!ref_for_changed_modules) in 
   if changed_modules=[] then (!cs_walker) else
   let _=Coma_state.PrivateThree.announce_changed_modules changed_modules in
   let ((cs2,nms_to_be_updated),rootless_paths)= 
    (Coma_state.PrivateThree.put_md_list_back_in_order false 
     (!cs_walker) changed_modules,
   (!ref_for_changed_shortpaths))  in   
      if nms_to_be_updated=[] then cs2 else
   let new_dirs=Coma_state.compute_subdirectories_list cs2  in
   let (cs3,rejected_pairs,accepted_pairs)=
          Coma_state.Ocaml_target_making.usual_feydeau cs2 nms_to_be_updated in 
   let rejected_mns=Image.image snd rejected_pairs in  
   let new_preqt=Image.image(
           fun (mn,_)->(mn,not(List.mem mn rejected_mns))
         )  (Coma_state.preq_types cs3) in   
   let cs4=Coma_state.set_directories cs3 new_dirs in 
   let cs5=Coma_state.set_preq_types cs4 new_preqt in 
   cs5 ;;
   
   
   let refresh cs = 
           let dir =Coma_state.root cs  in 
           let fw1 = cs.Coma_state_t.frontier_with_unix_world in 
           let temp1=Image.image (fun x->(x,()) ) (Fw_with_small_details.usual_compilable_files fw1) in
           let temp2=Coma_state.Simplified_ts_creation.classify_according_to_module dir temp1 in
           let temp3=Coma_state.Simplified_ts_creation.compute_dependencies temp2 in
           let temp4=Image.image (fun (mname,_)->
              let (opt,opt_ap,pr_rless,pr_ap) = List.assoc mname temp2 in 
               match opt with 
                None -> [pr_rless]
               |Some(mli_rless) -> [mli_rless;pr_rless]
            ) temp3 in 
           let rlesses_in_good_order=List.flatten temp4 in 
           let (failures,cs1)=Coma_state.Try_to_register.mlx_files cs rlesses_in_good_order in  
           let pre_preqt=Coma_state.printer_equipped_types_from_data cs1 in
           let l_mod=Coma_state.ordered_list_of_modules cs1 in 
           let (cs2,rejected_pairs,_)=
             Coma_state.Ocaml_target_making.usual_feydeau 
             cs1 l_mod in
           let rejected_endinglesses=Image.image snd rejected_pairs in 
           let new_ptypes=Image.image (fun mn->(mn,not(List.mem mn rejected_endinglesses))) pre_preqt in 
           let new_dirs=Coma_state.compute_subdirectories_list cs2 in
           let cs3=Coma_state.set_directories cs2 new_dirs in 
           let cs4=Coma_state.set_preq_types cs3 new_ptypes in
           cs4    ;;
   
   
   let register_rootless_paths cs rootless_paths=
     Coma_state.register_mlx_files cs rootless_paths ;;
   
   let relocate_module_to cs mn new_subdir=
     let old_endingless = Coma_state.endingless_at_module cs mn in  
     let old_subdir = Dfn_endingless.to_subdirectory old_endingless in 
     let mn=Dfn_endingless.to_module old_endingless in
     let old_acolytes= Coma_state.acolytes_at_module cs mn in
     let new_acolytes=Image.image 
       (fun mlx->Dfn_full.relocate mlx new_subdir) old_acolytes in
     let new_name=Dfn_full.to_endingless
      (List.hd new_acolytes) in
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
     let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.usual_build_subdir) in  
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
     let cs3=Coma_state.change_one_module_name cs2 old_nm new_nm in 
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
     let cs9=recompile (cs8,[],[],[]) in 
     cs9;;
   
   let rename_subdirectory cs old_subdir new_subdir=
     let rename_in_sd=(fun sd -> 
        match Dfa_subdirectory.soak (old_subdir,new_subdir) sd with 
        Some(new_sd) -> new_sd 
        |None -> sd
      ) in 
     let cs1=Coma_state.modify_all_subdirs cs rename_in_sd in 
     let cs2=Coma_state.modify_all_needed_dirs cs1 rename_in_sd in 
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
   
   
   let rename_string_or_value cs = recompile (cs,[],[],[]);; 
   
   end;;
   
   module Physical_followed_by_internal = struct
   
   exception Forget_modules_exn of Dfa_module_t.t  list ;;
   
   let forget_modules cs mod_names= 
     let check = Coma_state.check_module_sequence_for_forgettability cs mod_names in 
     if check <> []
     then raise(Forget_modules_exn(check))
     else 
     let cs2=Physical.forget_modules cs mod_names  in
     Internal.forget_modules cs2 mod_names ;;
   
   exception Forget_rootless_paths_exn of Dfa_module_t.t list ;;
   
   let forget_rootless_paths cs rootless_paths= 
     let check = Coma_state.check_rootless_path_sequence_for_forgettability cs rootless_paths in 
     if check <> []
     then raise(Forget_rootless_paths_exn(check))
     else 
     let cs2=Physical.forget_rootless_paths cs rootless_paths  in
     Internal.forget_rootless_paths cs2 rootless_paths;;
   
   
   let recompile cs = 
     let (cs2,changed_ac,changed_uc,changed_noncompilables)=Physical.recompile cs  in
     Internal.recompile (cs2,changed_ac,changed_uc,changed_noncompilables);;
     
   let refresh cs =
      let cs2=Physical.refresh (Coma_state.configuration cs)  in
      Internal.refresh cs2;;
   
   let register_rootless_paths cs rootless_paths= 
      let (cs2,ac_paths,uc_paths)=Physical.register_rootless_paths cs rootless_paths in
      Internal.register_rootless_paths cs2 uc_paths;;
   
   let relocate_module_to cs mod_name new_subdir= 
     let cs2=Physical.relocate_module_to cs mod_name  new_subdir  in
     Internal.relocate_module_to cs2 mod_name  new_subdir;;
   
   
   let rename_module cs old_middle_name new_nonslashed_name=
      let (cs2,_)=Physical.rename_module cs old_middle_name new_nonslashed_name in
      Internal.rename_module cs2 old_middle_name new_nonslashed_name;;
   
   let rename_subdirectory cs old_subdir new_subdir=
      let cs2=Physical.rename_subdirectory cs (old_subdir,new_subdir) in
      Internal.rename_subdirectory cs2 old_subdir new_subdir;;
   
   let rename_string_or_value cs old_sov new_sov =
      let (cs2,_)=Physical.rename_string_or_value cs old_sov new_sov in
      Internal.rename_string_or_value cs2;;
   
   end;;
   
   
   
   module After_checking = struct
   
         let forget_modules cs mod_names=
            let _=Coma_state.Recent_changes.check_for_changes cs in 
            Physical_followed_by_internal.forget_modules cs mod_names;; 
   
         let forget_rootless_paths cs rootless_paths=
            let _=Coma_state.Recent_changes.check_for_changes cs in 
            Physical_followed_by_internal.forget_rootless_paths cs rootless_paths;;    
   
         (* No check needed before recompiling *)
   
         (* No check needed before refreshing *)
   
         let register_rootless_paths cs rootless_paths=
            let _=Coma_state.Recent_changes.check_for_changes cs in 
            Physical_followed_by_internal.register_rootless_paths cs rootless_paths;; 
   
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
                     (Coma_state.configuration cs) 
                     diff opt
               else (print_string "No recent changes to commit ...";flush stdout);;
   
         end;;    
   
         let forget_modules cs mod_names=
            let  cs2=After_checking.forget_modules cs mod_names in 
            Coma_state.reflect_latest_changes_in_github cs2 None;; 
   
         let forget_rootless_paths cs rootless_paths=
            let cs2=After_checking.forget_rootless_paths cs rootless_paths in 
            Coma_state.reflect_latest_changes_in_github cs2 None ;; 
   
   
         let recompile cs opt_comment=
            let cs2=Physical_followed_by_internal.recompile cs  in 
            Coma_state.reflect_latest_changes_in_github cs2 opt_comment ;; 
   
         (* No backup during refresh *)   
   
         let register_rootless_paths cs rootless_paths=
            let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
            let cs2=After_checking.register_rootless_paths cs rootless_paths  in 
            let msg="register "^descr in 
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
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
            let cs2=After_checking.rename_string_or_value cs old_sov new_sov  in 
            let msg="rename "^old_sov^" as "^new_sov in 
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
   end;;
   
   
   
   module And_save = struct 
   
         let forget_modules cs mod_names=
            let cs2=And_backup.forget_modules cs mod_names in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
   
         let forget_rootless_paths cs rootless_paths=
            let cs2=And_backup.forget_rootless_paths cs rootless_paths in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
   
         let internet_access cs bowl=   
            let cs2=Coma_state.set_push_after_backup cs bowl in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
         
         let recompile cs opt_comment=
            let cs2=And_backup.recompile cs opt_comment in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
         
   
         let refresh cs =
            let cs2=Physical_followed_by_internal.refresh cs  in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
         let register_rootless_paths cs rootless_path=
            let cs2=And_backup.register_rootless_paths cs rootless_path in 
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
   
         let forget_modules pcs mod_names=
            let new_cs = And_save.forget_modules (!pcs) mod_names in 
            pcs:=new_cs;;
   
         let forget_rootless_paths pcs rootless_paths=
            let new_cs = And_save.forget_rootless_paths (!pcs) rootless_paths in 
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
   
         let register_rootless_paths pcs rootless_paths=
            let new_cs = And_save.register_rootless_paths (!pcs) rootless_paths in 
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
   
   let forget cs_ref data = 
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
      let _=(if all_paths=[] then () else Reference.forget_rootless_paths cs_ref all_paths) in 
      let _=(if all_modules=[] then () else Reference.forget_modules cs_ref all_modules) in 
      ();;
   
   
   let register_several cs_ref lines =
      let rootless_paths = Image.image Dfn_rootless.of_line lines in 
      Reference.register_rootless_paths cs_ref  rootless_paths ;;
   
   let register_one cs_ref line = register_several cs_ref [line];;
   
   let relocate_module_to cs_ref old_module_name new_subdir=
       let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
       Reference.relocate_module_to cs_ref mn new_subdir ;;
   
   let rename_module cs_ref old_module_name new_name=
      let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
      let old_eless = Coma_state.endingless_at_module (!cs_ref) mn in
      let old_middle_name = Dfn_endingless.to_middle old_eless in    
      let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
      Reference.rename_module cs_ref old_middle_name new_nonslashed_name;; 
   
   
   let rename_subdirectory cs_ref old_subdirname new_subdir_short_name=
       let old_subdir = Coma_state.find_subdir_from_suffix (!cs_ref) old_subdirname  in
       let new_subdir = Coma_state.compute_long_subdir_name (!cs_ref) old_subdir new_subdir_short_name  in 
       Reference.rename_subdirectory cs_ref old_subdir new_subdir ;;
   
   
   end;;
   
   