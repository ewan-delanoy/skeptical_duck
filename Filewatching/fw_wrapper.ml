(*

#use"Filewatching/fw_wrapper.ml";;

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
   
   let salt = "Fw_"^"wrapper_t.";;
   
   let configuration_label             = salt ^ "configuration";;
   let archived_compilable_files_label = salt ^ "archived_compilable_files";;
   let usual_compilable_files_label    = salt ^ "usual_compilable_files";;
   let noncompilable_files_label       = salt ^ "noncompilable_files";;
   let last_noticed_changes_label      = salt ^ "last_noticed_changes";;
   
   let of_concrete_object ccrt_obj = 
      let g=Concrete_object.get_record ccrt_obj in
      {
         Fw_wrapper_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
         archived_compilable_files = Crobj_converter_combinator.to_list pair_of_crobj (g archived_compilable_files_label);
         usual_compilable_files = Crobj_converter_combinator.to_list pair_of_crobj (g usual_compilable_files_label);
         noncompilable_files = Crobj_converter_combinator.to_list pair_of_crobj (g noncompilable_files_label);
         last_noticed_changes = Dircopy_diff.of_concrete_object (g last_noticed_changes_label);
      };; 
   
   let to_concrete_object fw=
      let items= 
      [
       configuration_label, Fw_configuration.to_concrete_object fw.Fw_wrapper_t.configuration;
       archived_compilable_files_label, Crobj_converter_combinator.of_list pair_to_crobj fw.Fw_wrapper_t.archived_compilable_files;
       usual_compilable_files_label, Crobj_converter_combinator.of_list pair_to_crobj fw.Fw_wrapper_t.usual_compilable_files;
       noncompilable_files_label, Crobj_converter_combinator.of_list pair_to_crobj fw.Fw_wrapper_t.noncompilable_files;
       last_noticed_changes_label, Dircopy_diff.to_concrete_object fw.Fw_wrapper_t.last_noticed_changes
      ]  in
      Concrete_object_t.Record items;;
   
   let all_watched_files fw = 
     (fw.Fw_wrapper_t.archived_compilable_files)
     @(fw.Fw_wrapper_t.usual_compilable_files)
     @(fw.Fw_wrapper_t.noncompilable_files) ;;
   
       
   
   end ;;
   
   let reflect_changes_in_diff fw l= {
      fw with 
      Fw_wrapper_t.last_noticed_changes = 
        Dircopy_diff.add_changes 
          (fw.Fw_wrapper_t.last_noticed_changes) l
   } ;;
   
   let get_content fw rootless = 
       let root = Fw_configuration.root (fw.Fw_wrapper_t.configuration) in 
       let s_ap = Dfn_common.recompose_potential_absolute_path root rootless in 
       Io.read_whole_file(Absolute_path.of_string s_ap);;     
           
   let get_mtime_or_zero_if_file_is_nonregistered fw rootless =
      match Option.seek (fun (rootless1,_)->rootless1=rootless) 
       (Private.all_watched_files fw) with 
      None -> "0."
     |Some(_,mtime)-> mtime  ;; 
   
   
   let get_mtime fw rootless  =
     match Option.seek (fun (rootless1,_)->rootless1=rootless) 
       (Private.all_watched_files fw) with 
      None -> raise (Rootless_not_found(rootless))
     |Some(_,mtime)-> mtime  ;; 
   
   let of_concrete_object = Private.of_concrete_object;;
   let to_concrete_object = Private.to_concrete_object;;
   
   let reflect_creations_in_diff fw created_ones= {
      fw with 
      Fw_wrapper_t.last_noticed_changes = 
        Dircopy_diff.create 
          (fw.Fw_wrapper_t.last_noticed_changes) created_ones
   } ;;
   
   
   let reflect_destructions_in_diff fw destroyed_ones = {
      fw with 
      Fw_wrapper_t.last_noticed_changes = 
        Dircopy_diff.destroy  
          (fw.Fw_wrapper_t.last_noticed_changes) destroyed_ones 
   } ;;
   
   
   let reflect_replacements_in_diff fw reps= {
      fw with 
      Fw_wrapper_t.last_noticed_changes = 
        Dircopy_diff.replace 
          (fw.Fw_wrapper_t.last_noticed_changes) reps
   } ;;
   
   let root fw = Fw_configuration.root (fw.Fw_wrapper_t.configuration);;
   
end ;;   


module Private = struct

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
     let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     mtime file;;


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

let update_some_files fw (ac_files,uc_files,nc_files) = {
    fw with 
      Fw_wrapper_t.archived_compilable_files = update_in_list_of_pairs fw ac_files (fw.Fw_wrapper_t.archived_compilable_files) ;
      usual_compilable_files = update_in_list_of_pairs fw uc_files (fw.Fw_wrapper_t.usual_compilable_files) ;
      noncompilable_files = update_in_list_of_pairs fw nc_files (fw.Fw_wrapper_t.noncompilable_files) ;
} ;;

let remove_nonnoncompilable_files fw rootless_paths =
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.archived_compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.archived_compilable_files)  ;
      Fw_wrapper_t.usual_compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.usual_compilable_files)  ;
   };;

let remove_noncompilable_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.noncompilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.noncompilable_files)  
   };;

let remove_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
    let fw2 ={
      fw with 
      Fw_wrapper_t.archived_compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.archived_compilable_files)  ;
      usual_compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.usual_compilable_files)  ;
      Fw_wrapper_t.noncompilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.noncompilable_files)  
   } in 
   Automatic.reflect_destructions_in_diff fw2 rootless_paths ;;



let forget_modules fw mod_names =
   let the_files = Option.filter_and_unpack (
      fun (path,_)-> 
        if List.mem (Dfn_rootless.to_module path) mod_names 
        then Some path
        else None
   ) fw.Fw_wrapper_t.usual_compilable_files in 
   remove_files fw the_files;;

let is_archived fw rootless_path = 
    let config = fw.Fw_wrapper_t.configuration in
    let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in 
    List.exists (Dfn_rootless.is_in rootless_path) archived_subdirs ;; 


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
   let (c_paths,nc_paths) = List.partition Dfn_rootless.is_compilable rootless_paths in 
   let (ac_paths,uc_paths) = List.partition (is_archived fw) rootless_paths in 
   let fw2=  {
      fw with 
      Fw_wrapper_t.archived_compilable_files =  
        (fw.Fw_wrapper_t.archived_compilable_files)@
          (Image.image (recompute_all_info fw) ac_paths)  ;
      usual_compilable_files =  
          (fw.Fw_wrapper_t.usual_compilable_files)@
            (Image.image (recompute_all_info fw) uc_paths)  ;    
      Fw_wrapper_t.noncompilable_files =  
        (fw.Fw_wrapper_t.noncompilable_files)@
         (Image.image (recompute_all_info fw) nc_paths)  
    }  in 
    (Automatic.reflect_creations_in_diff fw2 rootless_paths,(ac_paths,uc_paths,nc_paths));;

let deal_with_initial_comment_if_needed fw rless =
   if (Dfn_rootless.to_ending rless)<> Dfa_ending.ml 
   then ()
   else
      let root = Automatic.root fw in 
      let full = Dfn_join.root_to_rootless root rless in 
      let ap = Dfn_full.to_absolute_path full in 
      Put_use_directive_in_initial_comment.put_usual root ap
   ;;    

let relocate_usual_compilable_files_to fw rootless_paths new_subdir=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
    and s_subdir = Dfa_subdirectory.connectable_to_subpath new_subdir in 
    let displacements_to_be_made = Image.image (
      fun path->" mv "^s_root^(Dfn_rootless.to_line path)^" "^
      s_root^s_subdir 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc displacements_to_be_made in 
    let reps = Image.image (fun path->
      (path,Dfn_rootless.relocate_to path new_subdir)
    ) rootless_paths in 
    let fw2 = {
      fw with 
      Fw_wrapper_t.usual_compilable_files = Image.image (fun pair->
         let (path,_)=pair in 
         if(List.mem path rootless_paths) 
         then let new_path = Dfn_rootless.relocate_to path new_subdir in 
              let _ = deal_with_initial_comment_if_needed fw new_path in 
              (new_path,recompute_mtime fw new_path)
         else pair
      ) (fw.Fw_wrapper_t.usual_compilable_files)  
   } in 
   Automatic.reflect_replacements_in_diff fw2 reps ;;

let relocate_module_to fw mod_name new_subdir=
   let the_files = Option.filter_and_unpack (
      fun (path,_)-> 
        if (Dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Fw_wrapper_t.usual_compilable_files in 
   relocate_usual_compilable_files_to fw the_files new_subdir;;

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
       

let ref_for_subdirectory_renaming = ref [];;

let remember_during_subdirectory_renaming pair =
   (ref_for_subdirectory_renaming := pair :: (!ref_for_subdirectory_renaming) );;

let helper1_during_subdirectory_renaming fw (old_subdir,new_subdir) pair=
   let (rootless_path,_)=pair in 
   match Dfn_rootless.soak (old_subdir,new_subdir) rootless_path with 
   Some(new_rootless_path) -> 
        let _=(
           remember_during_subdirectory_renaming (rootless_path,new_rootless_path)
        ) in 
        recompute_all_info fw new_rootless_path
   |None -> pair;;

let helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) l_pairs =
     let _=(ref_for_subdirectory_renaming := []) in 
     let comp=Image.image (helper1_during_subdirectory_renaming fw (old_subdir,new_subdir)) l_pairs in 
     (comp,List.rev(!ref_for_subdirectory_renaming));;

let helper_during_archived_subdirectory_renaming fw (old_subdir,new_subdir)=
   let (files,reps)   =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_wrapper_t.archived_compilable_files) in 
   let fw2 = {
      fw with
      Fw_wrapper_t.archived_compilable_files = files  ;
   } in 
   Automatic.reflect_replacements_in_diff fw2 reps;;  
   
let helper_during_nonarchived_subdirectory_renaming fw (old_subdir,new_subdir)=
   let (uc_files,uc_reps)   =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_wrapper_t.usual_compilable_files) 
   and (nc_files,nc_reps) =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_wrapper_t.noncompilable_files) in    
   let fw2 = {
      fw with
      Fw_wrapper_t.usual_compilable_files = uc_files  ;
      Fw_wrapper_t.noncompilable_files = nc_files  ;
   } in 
   Automatic.reflect_replacements_in_diff fw2 (uc_reps@nc_reps);;

let helper3_during_subdirectory_renaming fw (old_subdir,new_subdir) = 
   let config = fw.Fw_wrapper_t.configuration in
   let archived_subdirs = config.Fw_configuration_t.subdirs_for_archived_mlx_files in 
   if List.exists (Dfa_subdirectory.begins_with old_subdir) archived_subdirs 
   then helper_during_archived_subdirectory_renaming fw (old_subdir,new_subdir) 
   else helper_during_nonarchived_subdirectory_renaming fw (old_subdir,new_subdir) ;;   

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw)  in 
    let s_old_subdir = Dfa_subdirectory.without_trailing_slash old_subdir 
    and s_new_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
    let old_full_path = s_root^s_old_subdir 
    and new_full_path = s_root^s_new_subdir in 
    let cmd=" mv "^old_full_path^" "^new_full_path in 
    let _=Unix_command.hardcore_uc cmd in 
    helper3_during_subdirectory_renaming fw (old_subdir,new_subdir) ;;   

let helper1_during_inspection fw accu pair=
   let (rootless_path,old_mtime)=pair in 
   let new_mtime = recompute_mtime fw rootless_path in 
   if new_mtime <> old_mtime
   then let _=(accu:=rootless_path::(!accu)) in 
        recompute_all_info fw rootless_path
   else pair;;

let helper2_during_inspection fw accu l_pairs =
   let new_l_pairs = Image.image (helper1_during_inspection fw accu) l_pairs in 
   (new_l_pairs,List.rev(!accu));;

let inspect_and_update fw = 
    let ref_for_archived_compilable_ones=ref[] 
    and ref_for_usual_compilable_ones=ref[] 
    and ref_for_noncompilable_ones=ref[] in 
    let (new_ac_files,changed_ac_files)=
        helper2_during_inspection fw ref_for_archived_compilable_ones fw.Fw_wrapper_t.archived_compilable_files 
    and (new_uc_files,changed_uc_files)=
         helper2_during_inspection fw ref_for_usual_compilable_ones fw.Fw_wrapper_t.usual_compilable_files     
    and  (new_nc_files,changed_nc_files)=
        helper2_during_inspection fw ref_for_noncompilable_ones fw.Fw_wrapper_t.noncompilable_files   in 
    let fw2 ={
       fw with
       Fw_wrapper_t.archived_compilable_files = new_ac_files ;
       usual_compilable_files  = new_uc_files ;
       Fw_wrapper_t.noncompilable_files = new_nc_files ;
    }  in 
    let new_fw = Automatic.reflect_changes_in_diff fw2 
      (changed_ac_files@changed_uc_files@changed_nc_files) in 
    (new_fw,(changed_ac_files,changed_uc_files,changed_nc_files));;         


let helper1_inside_module_renaming_in_filename fw s_new_module rootless_to_be_renamed =
  let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
   and (Dfn_rootless_t.J(s,m,e))=rootless_to_be_renamed in 
   let s_old_ap=s_root^(Dfn_rootless.to_line rootless_to_be_renamed)
   and s_new_ap=s_root^(Dfa_subdirectory.connectable_to_subpath s)
                ^s_new_module^(Dfa_ending.connectable_to_modulename e) in 
   "mv "^s_old_ap^" "^s_new_ap;;

let helper2_inside_module_renaming_in_filename fw new_module rootless_to_be_renamed =
  let (Dfn_rootless_t.J(s,m,e))=rootless_to_be_renamed in 
  (rootless_to_be_renamed,Dfn_rootless_t.J(s,new_module,e));;

let rename_module_in_filename_only fw old_module new_module =
   let acolytes = Option.filter_and_unpack (
       fun (rl,_) -> if (Dfn_rootless.to_module rl) = old_module then Some rl else None 
   ) fw.Fw_wrapper_t.usual_compilable_files in
   let s_new_module = Dfa_module.to_line new_module in 
   let l_cmds = Image.image (helper1_inside_module_renaming_in_filename fw s_new_module) acolytes in 
   let replacements = Image.image (helper2_inside_module_renaming_in_filename fw new_module) acolytes  in            
   let _ =Unix_command.conditional_multiple_uc l_cmds in  
   let old_compilable_files = fw.Fw_wrapper_t.usual_compilable_files  in    
   let new_compilable_files = Image.image (
     fun pair->
       let (rootless,_)=pair in 
       match Option.seek (fun (old_one,_)->old_one=rootless) replacements with  
       Some(_,new_rootless_path)-> recompute_all_info fw new_rootless_path  
       |None -> pair 
   )  old_compilable_files in 
   let fw2 ={
      fw with 
      Fw_wrapper_t.usual_compilable_files = new_compilable_files
   }   in 
   Automatic.reflect_replacements_in_diff fw2 replacements;;
    
let rename_module_in_files fw (old_module,new_module) files_to_be_rewritten =
  let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
  let _=List.iter (
    fun rootless_path->
      let ap=Absolute_path.of_string (s_root^(Dfn_rootless.to_line rootless_path)) in 
      Look_for_module_names.change_module_name_in_mlx_file old_module new_module ap 
  ) files_to_be_rewritten in 
  let selector = Image.image (
   fun pair->
     let (rootless,_)=pair in 
     if List.mem rootless files_to_be_rewritten
     then recompute_all_info fw rootless 
     else pair 
  )  in
 let fw2 ={
      fw with 
      Fw_wrapper_t.archived_compilable_files = selector fw.Fw_wrapper_t.archived_compilable_files ;
      usual_compilable_files = selector fw.Fw_wrapper_t.usual_compilable_files ;
   }    in 
   Automatic.reflect_changes_in_diff fw2 files_to_be_rewritten
    ;;
      


let rename_module_everywhere fw old_module new_module files_to_be_rewritten=
   let fw2=rename_module_in_filename_only fw old_module new_module in 
   let fw3=rename_module_in_files fw2 (old_module,new_module) files_to_be_rewritten in 
   fw3;;

let replace_string_in_list_of_pairs fw (replacee,replacer) l=
   let changed_ones=ref[]  in 
   let new_l=Image.image (
      fun pair ->
        let (rootless,mtime)=pair in 
        let content = Automatic.get_content fw rootless in 
        if Substring.is_a_substring_of replacee content 
        then let _=(changed_ones:= rootless:: (!changed_ones)) in 
             let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) 
             and s_path=Dfn_rootless.to_line rootless in 
             let file = s_root^s_path in  
             let ap=Absolute_path.of_string file in 
             let _=(Replace_inside.replace_inside_file (replacee,replacer) ap;
                   ) in 
             recompute_all_info fw rootless 
        else pair     
   ) l in 
   (new_l,List.rev(!changed_ones));;

let replace_string fw (replacee,replacer) =
   let rep = replace_string_in_list_of_pairs fw (replacee,replacer)  in 
   let (new_ac_files,changed_ac_files) =  rep fw.Fw_wrapper_t.archived_compilable_files 
   and (new_uc_files,changed_uc_files) =  rep fw.Fw_wrapper_t.usual_compilable_files 
   and (new_nc_files,changed_nc_files) =  rep fw.Fw_wrapper_t.noncompilable_files in 
   let fw2 ={
       fw with
       Fw_wrapper_t.archived_compilable_files = new_ac_files;
       usual_compilable_files = new_uc_files;
       noncompilable_files = new_nc_files;
   } in 
   let fw3 = Automatic.reflect_changes_in_diff fw2 
     (changed_ac_files @changed_uc_files @ changed_nc_files) in 
   (fw3,(changed_ac_files,changed_uc_files,changed_nc_files));;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path (Automatic.root fw)  in 
    let fw2= update_some_files fw ([],[rootless],[]) in 
    let (fw3,(changed_ac_files,changed_uc_files,changed_nc_files))=replace_string fw2 (replacee,replacer) in 
    let fw4 =  Automatic.reflect_changes_in_diff fw3 (rootless::(changed_ac_files @changed_uc_files@changed_nc_files)) in         
    (fw4,(changed_ac_files,rootless::changed_uc_files,changed_nc_files));;


let usual_compilable_absolute_paths fw= 
   let root = Automatic.root fw in 
   Image.image (
     fun (rootless,_)-> 
        Absolute_path.of_string (
           Dfn_common.recompose_potential_absolute_path root rootless
        )
   ) fw.Fw_wrapper_t.usual_compilable_files;;
   
let overwrite_usual_compilable_file_if_it_exists fw rootless new_content =
   let root = Automatic.root fw in 
   if List.exists ( fun (r,_)->r=rootless ) fw.Fw_wrapper_t.usual_compilable_files 
   then let ap = Absolute_path.of_string (Dfn_common.recompose_potential_absolute_path root rootless) in 
        let _=Io.overwrite_with ap new_content in 
        {
           fw with 
           Fw_wrapper_t.usual_compilable_files = update_in_list_of_pairs fw [rootless] (fw.Fw_wrapper_t.usual_compilable_files);
        }
   else fw;;


end;;


let empty_one config= {
   Fw_wrapper_t.configuration = config;
   archived_compilable_files = [];
   usual_compilable_files = [];
   noncompilable_files = [];
   last_noticed_changes = Dircopy_diff.empty_one;
};; 

let forget_modules = Private.forget_modules;;

let get_content = Automatic.get_content ;;

let get_mtime = Automatic.get_mtime ;;

let get_mtime_or_zero_if_file_is_nonregistered = Automatic.get_mtime_or_zero_if_file_is_nonregistered ;;

let inspect_and_update = Private.inspect_and_update;;

let of_concrete_object = Automatic.of_concrete_object ;;

let overwrite_file_if_it_exists = Private.overwrite_usual_compilable_file_if_it_exists;;

let reflect_latest_changes_in_github fw opt_msg=
   let config = fw.Fw_wrapper_t.configuration in 
   let _= Reflect_change_in_github.backup config fw.Fw_wrapper_t.last_noticed_changes opt_msg in 
   {fw with Fw_wrapper_t.last_noticed_changes = Dircopy_diff.empty_one} ;; 


let register_rootless_paths = Private.register_rootless_paths;;

let relocate_module_to = Private.relocate_module_to;;

let remove_files = Private.remove_files;;

let rename_module = Private.rename_module_everywhere;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;

let root = Automatic.root ;;

let to_concrete_object = Automatic.to_concrete_object ;;

let usual_compilable_absolute_paths = Private.usual_compilable_absolute_paths;;
