(*

#use"Filewatching/file_watcher.ml";;

*)

exception Register_rootless_path_exn of string list;;

module Automatic = struct 

   exception Rootless_not_found of Dfn_rootless_t.t;;


   module Private = struct 
   
   let pair_of_crobj crobj=
      let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
     (
       Dfn_rootless.of_concrete_object arg1,
       Crobj_converter.string_of_concrete_object arg2
     );;
   
   let pair_to_crobj (watched_file,modif_date)=
     Concrete_object_t.Variant("Dfn_"^"rootless.J",
        [
           
           Dfn_rootless.to_concrete_object watched_file;
           Crobj_converter.string_to_concrete_object(modif_date);
        ]
      ) ;;
   
   let salt = "Fw_"^"nonmodular_wrapper_t.";;
   
   let configuration_label        = salt ^ "configuration";;
   let watched_files_label        = salt ^ "watched_files";;
   let last_noticed_changes_label = salt ^ "last_noticed_changes";;
   
   let of_concrete_object ccrt_obj = 
      let g=Concrete_object.get_record ccrt_obj in
      {
         File_watcher_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
         watched_files = Crobj_converter_combinator.to_list pair_of_crobj (g watched_files_label);
         last_noticed_changes = Dircopy_diff.of_concrete_object (g last_noticed_changes_label);
      };; 
   
   let to_concrete_object fw=
      let items= 
      [
       configuration_label, Fw_configuration.to_concrete_object fw.File_watcher_t.configuration;
       watched_files_label, Crobj_converter_combinator.of_list pair_to_crobj fw.File_watcher_t.watched_files;
       last_noticed_changes_label, Dircopy_diff.to_concrete_object fw.File_watcher_t.last_noticed_changes
      ]  in
      Concrete_object_t.Record items;;
   
   
   end ;;
   
   let configuration fw = fw.File_watcher_t.configuration ;;
   
   let get_content fw rootless = 
       let root = Fw_configuration.root (fw.File_watcher_t.configuration) in 
       let s_ap = Dfn_common.recompose_potential_absolute_path root rootless in 
       Io.read_whole_file(Absolute_path.of_string s_ap);;     
           
   let get_mtime_or_zero_if_file_is_nonregistered fw rootless =
      match Option.seek (fun (rootless1,_)->rootless1=rootless) 
       (fw.File_watcher_t.watched_files) with 
      None -> "0."
     |Some(_,mtime)-> mtime  ;; 
   
       
   
   let get_mtime fw rootless  =
     match Option.seek (fun (rootless1,_)->rootless1=rootless) 
     (fw.File_watcher_t.watched_files) with 
      None -> raise (Rootless_not_found(rootless))
     |Some(_,mtime)-> mtime  ;; 
   
   let last_noticed_changes fw = fw.File_watcher_t.last_noticed_changes ;;

   let of_concrete_object = Private.of_concrete_object;;
  
   let reflect_changes_in_diff fw l= {
      fw with 
      File_watcher_t.last_noticed_changes = 
        Dircopy_diff.add_changes 
          (fw.File_watcher_t.last_noticed_changes) l
   } ;;

   let reflect_creations_in_diff fw created_ones= {
      fw with 
      File_watcher_t.last_noticed_changes = 
        Dircopy_diff.create 
          (fw.File_watcher_t.last_noticed_changes) created_ones
   } ;;
   
   
   let reflect_destructions_in_diff fw destroyed_ones = {
      fw with 
      File_watcher_t.last_noticed_changes = 
        Dircopy_diff.destroy  
          (fw.File_watcher_t.last_noticed_changes) destroyed_ones 
   } ;;
   
   
   let reflect_replacements_in_diff fw reps= {
      fw with 
      File_watcher_t.last_noticed_changes = 
        Dircopy_diff.replace 
          (fw.File_watcher_t.last_noticed_changes) reps
   } ;;
   
   let root fw = Fw_configuration.root (fw.File_watcher_t.configuration);;

   let set_gitpush_after_backup fw new_gab = 
      let old_config = fw.File_watcher_t.configuration in 
      let new_config = {
         old_config with 
         Fw_configuration_t.gitpush_after_backup = new_gab
      } in
      {
      fw with 
       File_watcher_t.configuration = new_config ;
   } ;;

   let set_last_noticed_changes fw new_lnc = {
      fw with 
       File_watcher_t.last_noticed_changes = new_lnc ;
   } ;;

   let to_concrete_object = Private.to_concrete_object;;
   
  
   let watched_files fw = fw.File_watcher_t.watched_files ;;

end ;;   

module Private = struct

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
     let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     mtime file;;

let recompute_mtime_opt fw path =
    try Some(recompute_mtime fw path) with _ -> None ;;

let recompute_all_info fw path =
     let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     (path,mtime file);;

let update_in_list_of_pairs fw  to_be_updated pairs  =
   Image.image (
      fun pair -> 
        let (rootless,mtime)=pair in 
        if List.mem rootless to_be_updated 
        then recompute_all_info fw rootless 
        else pair
   ) pairs;;

let update_some_files fw (w_files,sw_files) = {
    fw with 
      File_watcher_t.watched_files = update_in_list_of_pairs fw w_files 
      (fw.File_watcher_t.watched_files) ;
} ;;


let remove_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
    let fw2 ={
      fw with 
      File_watcher_t.watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.File_watcher_t.watched_files)  ;
   } in 
   Automatic.reflect_destructions_in_diff fw2 rootless_paths ;;


let register_rootless_paths fw rootless_paths= 
   let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in
   let bad_paths = Option.filter_and_unpack (
     fun rp-> let s_full_path = s_root^(Dfn_rootless.to_line rp)  in 
     if not(Sys.file_exists s_full_path)
     then Some(s_full_path)
     else None
   ) rootless_paths in 
   if bad_paths<>[]
   then raise(Register_rootless_path_exn(bad_paths))
   else 
   let fw2=  {
      fw with 
      File_watcher_t.watched_files =  
        (fw.File_watcher_t.watched_files)@
          (Image.image (recompute_all_info fw) rootless_paths)  ;
    }  in 
    Automatic.reflect_creations_in_diff fw2 rootless_paths;;

let deal_with_initial_comment_if_needed fw rless =
   if (Dfn_rootless.to_ending rless)<> Dfa_ending.ml 
   then ()
   else
      let root = Automatic.root fw in 
      let full = Dfn_join.root_to_rootless root rless in 
      let ap = Dfn_full.to_absolute_path full in 
      Put_use_directive_in_initial_comment.put_usual root ap
   ;;    





let helper_during_string_replacement fw (old_string,new_string) accu old_list=
    let new_list =Image.image (
       fun pair->
         let (old_path,_)=pair in 
         if not(Supstring.contains (Automatic.get_content fw old_path) old_string)
         then pair 
         else 
            let ap = Dfn_full.to_absolute_path (Dfn_join.root_to_rootless (Automatic.root fw) old_path) in 
            let _=(
             Replace_inside.replace_inside_file (old_string,new_string) ap;
             accu:=old_path::(!accu)
            ) in 
            recompute_all_info fw old_path 
         ) old_list in 
    (new_list,List.rev(!accu));;

let replace_string fw (old_string,new_string)=
    let ref_for_changed_files=ref[]  in 
    let (new_files,changed_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_changed_files fw.File_watcher_t.watched_files in 
    let new_fw ={
       fw with
       File_watcher_t.watched_files = new_files ;
    }  in 
    (new_fw,changed_files);;         
       
let rename_value_inside_rootless fw (old_name,new_name) preceding_files rootless_path=
   let full_path = Dfn_join.root_to_rootless (Automatic.root fw) rootless_path in 
   let absolute_path=Dfn_full.to_absolute_path  full_path in 
   let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name absolute_path in 
   let new_watched_files =Image.image (
       fun pair->
         let (path,_)=pair in 
         if path = rootless_path 
         then recompute_all_info fw path
         else pair 
   ) (fw.File_watcher_t.watched_files) in 
   {
      fw with 
       File_watcher_t.watched_files = new_watched_files
   };;  

let ref_for_subdirectory_renaming = ref [];;

let remember_during_subdirectory_renaming pair =
   (ref_for_subdirectory_renaming := pair :: (!ref_for_subdirectory_renaming) );;

let rename_subdirectory_on_pair fw (old_subdir,new_subdir) pair=
   let (rootless_path,_)=pair in 
   match Dfn_rootless.soak (old_subdir,new_subdir) rootless_path with 
   Some(new_rootless_path) -> 
        let _=(
           remember_during_subdirectory_renaming (rootless_path,new_rootless_path)
        ) in 
        recompute_all_info fw new_rootless_path
   |None -> pair;;

let rename_subdirectory_on_pairs fw (old_subdir,new_subdir) l_pairs =
     let _=(ref_for_subdirectory_renaming := []) in 
     let comp=Image.image (rename_subdirectory_on_pair fw (old_subdir,new_subdir)) l_pairs in 
     (comp,List.rev(!ref_for_subdirectory_renaming));;

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw)  in 
    let s_old_subdir = Dfa_subdirectory.without_trailing_slash old_subdir 
    and s_new_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
    let old_full_path = s_root^s_old_subdir 
    and new_full_path = s_root^s_new_subdir in 
    let cmd=" mv "^old_full_path^" "^new_full_path in 
        let _=Unix_command.hardcore_uc cmd in 
    let (files,reps)   =  rename_subdirectory_on_pairs fw (old_subdir,new_subdir) (fw.File_watcher_t.watched_files) in 
   let fw2 = {
      fw with
      File_watcher_t.watched_files = files  ;
   } in 
   Automatic.reflect_replacements_in_diff fw2 reps;;   

let message_about_missing_files missing_files=
   let temp1=Image.image Dfn_rootless.to_line missing_files in
   "\n\n"^
   "The following files have been deleted without warning :\n"^
   (String.concat "\n" temp1)^
   "\n\n"
 ;;    

let announce_missing_files missing_files=
     if missing_files=[]
     then ()
     else (print_string(message_about_missing_files missing_files);flush stdout);;
            

let helper1_during_inspection fw accu (rl,old_mtime,new_mtime)=
   let _ = (if new_mtime <> old_mtime then accu:=rl::(!accu)) in 
   (rl,new_mtime);;

let helper2_during_inspection fw accu l_pairs =
   let temp1 = Image.image (fun (rl,old_mtime)->
      (rl,old_mtime,recompute_mtime_opt fw rl)
      ) l_pairs in 
   let (good_temp1,bad_temp1) =  List.partition (
       fun (_,_,opt) -> opt <> None 
   ) temp1 in
   let good_pairs = Image.image (fun 
     (rl,old_mtime,opt) -> (rl,old_mtime,Option.unpack opt)
   ) good_temp1 
   and missing_files = Image.image (fun (rl,_,_)->rl) bad_temp1 in
   let _ = announce_missing_files missing_files in 
   let new_l_pairs = Image.image (helper1_during_inspection fw accu) good_pairs in 
   (new_l_pairs,List.rev(!accu));;

let inspect_and_update fw = 
    let ref_for_files=ref[]  in 
    let (new_files,changed_files)=
        helper2_during_inspection fw ref_for_files fw.File_watcher_t.watched_files in 
    let fw2 ={
       fw with
       File_watcher_t.watched_files         = new_files ;
    }  in 
    let new_fw = Automatic.reflect_changes_in_diff fw2 changed_files in 
    (new_fw,changed_files);;         

let adhoc_membership path selected_files_opt=
   match selected_files_opt with 
   None -> true 
   |Some selected_files -> List.mem path selected_files ;;

let apply_text_transformation_on_pair fw tr changed_files selected_files_opt pair=
   let (path,_) = pair in 
   if not(adhoc_membership path selected_files_opt)
   then pair 
   else    
   let old_content = Automatic.get_content fw path in 
   let new_content = tr old_content in
   if new_content = old_content   
   then pair 
   else 
   let _=(changed_files:= path:: (!changed_files)) in 
   let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
   and s_path=Dfn_rootless.to_line path in 
   let file = s_root^s_path in  
   let ap=Absolute_path.of_string file in 
   let _=(Io.overwrite_with ap new_content) in 
   recompute_all_info fw path ;;

let apply_text_transformation_on_adhoc_option fw tr selected_files_opt=
   let changed_files=ref[]  in 
   let new_files = Image.image (
      apply_text_transformation_on_pair fw tr changed_files selected_files_opt
   )  fw.File_watcher_t.watched_files  in 
   let fw2 ={
      fw with
      File_watcher_t.watched_files = new_files;
   } in 
   let fw3 = Automatic.reflect_changes_in_diff fw2 (!changed_files) in 
   (fw3,!changed_files);;    

let apply_text_transformation_on_some_files fw tr l=
      apply_text_transformation_on_adhoc_option fw tr (Some l) ;;   

let apply_text_transformation_on_all_files fw tr =
   apply_text_transformation_on_adhoc_option fw tr None ;;
                


let replace_string fw (replacee,replacer) =
   apply_text_transformation_on_all_files fw (
      Replace_inside.replace_inside_string (replacee,replacer)
   ) ;;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path (Automatic.root fw)  in 
    let fw2= update_some_files fw ([rootless],[]) in 
    let (fw3,changed_files)=replace_string fw2 (replacee,replacer) in 
    let fw4 =  Automatic.reflect_changes_in_diff fw3 (rootless::changed_files) in         
    (fw4,(rootless::changed_files));;

   let overwrite_file_if_it_exists fw rootless new_content =
      let root = Automatic.root fw in 
      if List.exists ( fun (r,_)->r=rootless ) fw.File_watcher_t.watched_files 
      then let ap = Absolute_path.of_string (Dfn_common.recompose_potential_absolute_path root rootless) in 
           let _=Io.overwrite_with ap new_content in 
           ({
              fw with 
              File_watcher_t.watched_files = update_in_list_of_pairs fw [rootless] (fw.File_watcher_t.watched_files);
           },true)
      else (fw,false);;

   let rename_files fw renaming_schemes =
      let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw)  in 
      let displacements_to_be_made = Image.image (
        fun (path1,path2)->" mv "^s_root^(Dfn_rootless.to_line path1)^" "^
        s_root^(Dfn_rootless.to_line path2)
      ) renaming_schemes in 
      let _=Unix_command.conditional_multiple_uc displacements_to_be_made in 
      let fw2 = {
        fw with 
        File_watcher_t.watched_files = Image.image (fun pair->
           let (path,_)=pair in 
           (match List.assoc_opt path renaming_schemes with
           Some(new_path) -> 
                let _ = (
                  if  (Dfn_rootless.to_ending new_path) = Dfa_ending.ml
                  then deal_with_initial_comment_if_needed fw new_path
                ) in 
                (new_path,recompute_mtime fw new_path)
           | None -> pair)
        ) (fw.File_watcher_t.watched_files)  
     } in 
     Automatic.reflect_replacements_in_diff fw2 renaming_schemes ;;

   let relocate_files_to fw rootless_paths new_subdir=
     let renaming_schemes = Image.image (fun path->
       (path,Dfn_rootless.relocate_to path new_subdir)
     ) rootless_paths in 
     rename_files fw renaming_schemes ;;

   
        
        let first_init config =
           let the_root = config.Fw_configuration_t.root in 
           let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
           let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir config.Fw_configuration_t.ignored_subdirectories in 
           let list2 = Option.filter_and_unpack(
             fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
                      _->None 
           ) list1 in
           List.filter (Fw_configuration.test_for_admissibility config) list2 ;;
        
        
        let of_configuration_and_list config to_be_watched =
            let the_root = config.Fw_configuration_t.root in  
            let compute_info=( fun path->
              let s_root = Dfa_root.connectable_to_subpath the_root
              and s_path=Dfn_rootless.to_line path in 
              let file = s_root^s_path in 
              let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
              (path,mtime)
           ) in 
             {
               File_watcher_t.configuration = config;
               watched_files = Image.image compute_info to_be_watched;
               last_noticed_changes = Dircopy_diff.empty_one;
             };;
        
        let of_configuration config = 
           let to_be_watched = first_init config in 
           of_configuration_and_list config to_be_watched ;;
      
      module Modular = struct

      let canonical_tripartition fw all_files =
            let (c_files,nc_files) = List.partition (
                fun rl->
                  Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
            )  all_files in 
            let config = Automatic.configuration fw in
            let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in 
            let is_archived = (fun rl->List.exists (Dfn_rootless.is_in rl) archived_subdirs) in 
            let (a_files,u_files) = List.partition is_archived  c_files in 
            (a_files,u_files,nc_files) ;;     
      
      let compilable_files fw =
         Option.filter_and_unpack (
                      fun (rl,_)->
                     if Dfa_ending.is_compilable (Dfn_rootless.to_ending rl)
                     then Some rl 
                     else None   
         )  (Automatic.watched_files fw) ;;
      
      let compute_small_details_on_one_file fw rl=
         let root = Fw_configuration.root (fw.File_watcher_t.configuration) in 
         let s_ap = Dfn_common.recompose_potential_absolute_path root rl in 
         let ap = Absolute_path.of_string s_ap in 
         Fw_file_small_details.compute ap ;;

      let compute_all_small_details fw =
         let c_files = compilable_files fw in 
         Image.image (
            fun rl ->
               (rl,compute_small_details_on_one_file fw rl)
         ) c_files ;;
           

      let forget_modules fw mod_names =
         let all_files = Image.image fst (Automatic.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         let the_files = List.filter (
                 fun path-> List.mem (Dfn_rootless.to_module path) mod_names 
         ) u_files in    
         remove_files fw the_files;;      

      let noncompilable_files fw  =
         let all_files = Image.image fst (Automatic.watched_files fw) in 
         let (_,_,nc_files) = canonical_tripartition fw all_files in 
         nc_files ;;
      
      let relocate_module_to fw mod_name new_subdir=
         let all_files = Image.image fst (Automatic.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         let the_files = List.filter (
                  fun path-> (Dfn_rootless.to_module path)=mod_name 
         ) u_files in 
         relocate_files_to fw the_files new_subdir ;;
         
      let rename_module_on_filename_level fw (old_module,new_module) =
         let all_files = Image.image fst (Automatic.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         let acolytes = List.filter (
                fun rl -> (Dfn_rootless.to_module rl) = old_module 
         ) u_files in
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
         rename_files  fw replacements  ;;  
            
      let rename_module_on_content_level fw (old_module,new_module) files_to_be_rewritten =
         apply_text_transformation_on_some_files fw
            (Look_for_module_names.change_module_name_in_ml_ocamlcode  
            old_module new_module)  files_to_be_rewritten  ;;  
               
      let rename_module_on_filename_level_and_in_files fw old_module new_module files_to_be_rewritten=
         let fw2=rename_module_on_filename_level fw (old_module,new_module) in 
         let fw3=rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
         fw3;;
         

      let usual_compilable_files fw  =
         let all_files = Image.image fst (Automatic.watched_files fw) in 
         let (_,u_files,_) = canonical_tripartition fw all_files in 
         u_files ;;        

      end ;;      

      
end;;

let apply_text_transformation_on_all_files = Private.apply_text_transformation_on_all_files;;
let apply_text_transformation_on_some_files = Private.apply_text_transformation_on_some_files;;

let compilable_files = Private.Modular.compilable_files ;;

let compute_all_small_details = Private.Modular.compute_all_small_details ;;

let compute_small_details_on_one_file = Private.Modular.compute_small_details_on_one_file ;;

let empty_one config= {
   File_watcher_t.configuration = config;
   watched_files = [];
   last_noticed_changes = Dircopy_diff.empty_one;
};; 

let forget_modules = Private.Modular.forget_modules ;;

let get_content = Automatic.get_content ;;
let get_mtime   = Automatic.get_mtime ;;
let get_mtime_or_zero_if_file_is_nonregistered  = Automatic.get_mtime_or_zero_if_file_is_nonregistered ;;

let inspect_and_update = Private.inspect_and_update;;

let noncompilable_files = Private.Modular.noncompilable_files ;;

let of_concrete_object = Automatic.of_concrete_object ;;
let of_configuration = Private.of_configuration ;;
let of_configuration_and_list = Private.of_configuration_and_list ;;

let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;

let partition_for_singles = Private.Modular.canonical_tripartition ;; 

let reflect_latest_changes_in_github fw opt_msg=
   let config = fw.File_watcher_t.configuration in 
   let _= Reflect_change_in_github.backup config fw.File_watcher_t.last_noticed_changes opt_msg in 
   {fw with File_watcher_t.last_noticed_changes = Dircopy_diff.empty_one} ;; 


let register_rootless_paths = Private.register_rootless_paths;;

let relocate_files_to = Private.relocate_files_to;;

let relocate_module_to = Private.Modular.relocate_module_to ;;

let remove_files = Private.remove_files;;

let rename_files = Private.rename_files;;

let rename_module_on_filename_level_and_in_files = Private.Modular.rename_module_on_filename_level_and_in_files ;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;

let to_concrete_object = Automatic.to_concrete_object ;;

let usual_compilable_files = Private.Modular.usual_compilable_files ;;