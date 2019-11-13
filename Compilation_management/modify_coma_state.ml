
(* 

#use"Compilation_management/modify_coma_state.ml";;

*)



module Physical = struct 


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

exception Rename_string_or_value_exn of string ;;


let rename_string_or_value cs old_sov new_sov =
   let old_fw = Coma_state.frontier_with_unix_world cs in 
   let (new_fw,(changed_w_files,changed_sw_files))=(
      if not(String.contains old_sov '.')
      then Fw_wrapper.replace_string old_fw (old_sov,new_sov)
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
   (Coma_state.set_frontier_with_unix_world cs new_fw,(changed_w_files,changed_sw_files));;       



end;;

module Internal = struct

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
  let old_files=Image.image (fun mlx->Dfn_full.to_rootless_line mlx) old_acolytes in   
  let new_files=Image.image (fun mlx->Dfn_full.to_rootless_line mlx) 
     new_acolytes in 
  let new_eless=Dfn_full.to_endingless(List.hd new_acolytes) in
  let separated_acolytes_below=Option.filter_and_unpack(
    fun mn->
     if List.mem old_nm (Coma_state.ancestors_at_module cs2 mn)
    then Some(Image.image (Dfn_full.to_rootless) (Coma_state.acolytes_at_module cs2 mn))
    else None
) (Coma_state.ordered_list_of_modules cs2) in
  let all_acolytes_below=List.flatten separated_acolytes_below in
  let modified_files=Image.image Dfn_rootless.to_line all_acolytes_below in 
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
  let (cs9,_,_)=Coma_state.recompile cs8 in 
   let diff=Dircopy_diff.veil
    (Recently_deleted.of_string_list old_files)
    (Recently_changed.of_string_list modified_files)
    (Recently_created.of_string_list new_files) in
   (cs9,diff);;

let rename_string_or_value cs (changed_w_files,changed_sw_files)=
   let unordered_changed_paths = Image.image Dfn_rootless.to_line changed_w_files in 
   let changed_paths = Ordered.sort Total_ordering.silex_for_strings unordered_changed_paths in 
   let diff = Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list changed_paths)
    (Recently_created.of_string_list []) in 
   diff;; 

end;;

module Physical_followed_by_internal = struct

(*
let forget cs  x=
   let old_fw = Coma_state_field.frontier_with_unix_world cs in 
   let new_fw = Fw_wrapper.forget old_fw x in 
   let cs1 = Coma_state_field.set_frontier_with_unix_world cs new_fw in 
   Coma_state.Almost_concrete.forget cs1 x;; 


let register_rootless_path cs  x=
   let old_fw = Coma_state_field.frontier_with_unix_world cs in 
   let new_fw = Fw_wrapper.forget old_fw x in 
   let cs1 = Coma_state_field.set_frontier_with_unix_world cs new_fw in 
   Coma_state.Almost_concrete.register_rootless_path cs1 x;; 
*)

let rename_module cs old_middle_name new_nonslashed_name=
   let cs2=Physical.rename_module cs old_middle_name new_nonslashed_name in
   Internal.rename_module cs2 old_middle_name new_nonslashed_name;;

end;;



module After_checking = struct

      let forget cs x=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Coma_state.Almost_concrete.forget cs x;; 

      (* No check needed before recompiling *)

      (* No check needed before refreshing *)

      let register_rootless_path cs x=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Coma_state.Almost_concrete.register_rootless_path cs x;; 

      let relocate_module_to cs old_hm_name new_subdir=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Coma_state.Almost_concrete.local_relocate_module cs old_hm_name new_subdir;; 

      let rename_directory  cs old_subdir new_subdirname=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Coma_state.Almost_concrete.local_rename_directory  cs old_subdir new_subdirname;; 

      let rename_module cs old_middle_name new_nonslashed_name=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_module cs old_middle_name new_nonslashed_name;; 

      let rename_string_or_value cs old_hm_name new_name=
         let _=Coma_state.Recent_changes.check_for_changes cs in 
         Coma_state.Almost_concrete.rename_string_or_value cs old_hm_name new_name;; 

end;;

module And_backup = struct

      module Private = struct

            let backup cs diff opt= 
            if not(Dircopy_diff.is_empty diff) 
            then Backup_coma_state.backup
                  (Coma_state.root cs,Coma_state.backup_dir cs,Coma_state.gitpush_after_backup cs) 
                  diff opt
            else (print_string "No recent changes to commit ...";flush stdout);;

      end;;    

      let forget cs x=
         let (cs2,diff)=After_checking.forget cs x in 
         let _=Private.backup cs2 diff None in 
         cs2;; 

      let recompile cs opt_comment=
         let (cs2,diff)=Coma_state.Almost_concrete.recompile cs  in 
         let _=Private.backup cs2 diff opt_comment in 
         cs2;; 

      (* No backup during refresh *)   

      let register_rootless_path cs x=
         let (cs2,diff)=After_checking.register_rootless_path cs x  in 
         let msg="register "^x in 
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

      let relocate_module_to cs old_hm_name new_subdir=
         let (cs2,diff)=After_checking.relocate_module_to cs old_hm_name new_subdir  in
         let msg="move "^old_hm_name^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
         let _=Private.backup cs2 diff (Some msg) in  
         cs2;; 

      let rename_directory  cs old_subdir new_subdirname=
         let (cs2,diff)=After_checking.rename_directory  cs old_subdir new_subdirname  in 
         let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^" as "^new_subdirname in 
         let _=Private.backup cs2 diff (Some msg) in  
         cs2;; 


      let rename_module cs old_middle_name new_nonslashed_name=
         let (cs2,diff)=After_checking.rename_module cs old_middle_name new_nonslashed_name  in 
         let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
                 " as "^(No_slashes.to_string new_nonslashed_name) in       
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

      let rename_string_or_value cs old_name new_name=
         let (cs2,diff)=After_checking.rename_string_or_value cs old_name new_name  in 
         let msg="rename "^old_name^" as "^new_name in 
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

end;;


module And_save = struct 

      let forget cs x=
         let cs2=And_backup.forget cs x in 
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
         let (cs2,_)=Coma_state.refresh cs  in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

      let register_rootless_path cs rootless_path=
         let cs2=And_backup.register_rootless_path cs rootless_path in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

      let relocate_module_to cs old_hm_name new_subdir=
      let cs2 = And_backup.relocate_module_to cs old_hm_name new_subdir in 
      let _=Save_coma_state.save cs2 in 
      cs2;;   

      let rename_directory cs old_subdir new_subdirname=
         let cs2=And_backup.rename_directory cs old_subdir new_subdirname in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  


      let rename_module cs old_middle_name new_nonslashed_name=
         let cs2=And_backup.rename_module cs old_middle_name new_nonslashed_name in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  


      let rename_string_or_value cs old_name new_name=
         let cs2=And_backup.rename_string_or_value cs old_name new_name in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

end ;;

module Reference = struct 

      let forget pcs x=
         let new_cs = And_save.forget (!pcs) x in 
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


      let relocate_module_to pcs old_hm_name new_subdir=
         let new_cs = And_save.relocate_module_to (!pcs) old_hm_name new_subdir in 
         pcs:=new_cs;;  


      let rename_directory pcs old_subdir new_subdirname=
         let new_cs = And_save.rename_directory (!pcs) old_subdir new_subdirname in 
         pcs:=new_cs;;
         

      let rename_module pcs old_middle_name new_nonslashed_name=
         let new_cs = And_save.rename_module (!pcs) old_middle_name new_nonslashed_name in 
         pcs:=new_cs;;


      let rename_string_or_value pcs old_name new_name=
         let new_cs = And_save.rename_string_or_value (!pcs) old_name new_name in 
         pcs:=new_cs;;


end ;;


module Syntactic_sugar = struct 

let rename_module cs_ref old_module_name new_name=
   let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
   let old_eless = Coma_state.endingless_at_module (!cs_ref) mn in
   let old_middle_name = Dfn_endingless.to_middle old_eless in    
   let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
   Reference.rename_module cs_ref old_middle_name new_nonslashed_name;; 

end;;

