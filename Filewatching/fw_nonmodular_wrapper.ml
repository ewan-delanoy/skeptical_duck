(*

#use"Filewatching/fw_nonmodular_wrapper.ml";;

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
         Fw_nonmodular_wrapper_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
         watched_files = Crobj_converter_combinator.to_list pair_of_crobj (g watched_files_label);
         last_noticed_changes = Dircopy_diff.of_concrete_object (g last_noticed_changes_label);
      };; 
   
   let to_concrete_object fw=
      let items= 
      [
       configuration_label, Fw_configuration.to_concrete_object fw.Fw_nonmodular_wrapper_t.configuration;
       watched_files_label, Crobj_converter_combinator.of_list pair_to_crobj fw.Fw_nonmodular_wrapper_t.watched_files;
       last_noticed_changes_label, Dircopy_diff.to_concrete_object fw.Fw_nonmodular_wrapper_t.last_noticed_changes
      ]  in
      Concrete_object_t.Record items;;
   
   
   end ;;
   
   let configuration fw = fw.Fw_nonmodular_wrapper_t.configuration ;;
   
   let get_content fw rootless = 
       let root = Fw_configuration.root (fw.Fw_nonmodular_wrapper_t.configuration) in 
       let s_ap = Dfn_common.recompose_potential_absolute_path root rootless in 
       Io.read_whole_file(Absolute_path.of_string s_ap);;     
           
   let get_mtime_or_zero_if_file_is_nonregistered fw rootless =
      match Option.seek (fun (rootless1,_)->rootless1=rootless) 
       (fw.Fw_nonmodular_wrapper_t.watched_files) with 
      None -> "0."
     |Some(_,mtime)-> mtime  ;; 
   
   
   let get_mtime fw rootless  =
     match Option.seek (fun (rootless1,_)->rootless1=rootless) 
     (fw.Fw_nonmodular_wrapper_t.watched_files) with 
      None -> raise (Rootless_not_found(rootless))
     |Some(_,mtime)-> mtime  ;; 
   
   let of_concrete_object = Private.of_concrete_object;;
   let to_concrete_object = Private.to_concrete_object;;
   
   let reflect_changes_in_diff fw l= {
      fw with 
      Fw_nonmodular_wrapper_t.last_noticed_changes = 
        Dircopy_diff.add_changes 
          (fw.Fw_nonmodular_wrapper_t.last_noticed_changes) l
   } ;;

   let reflect_creations_in_diff fw created_ones= {
      fw with 
      Fw_nonmodular_wrapper_t.last_noticed_changes = 
        Dircopy_diff.create 
          (fw.Fw_nonmodular_wrapper_t.last_noticed_changes) created_ones
   } ;;
   
   
   let reflect_destructions_in_diff fw destroyed_ones = {
      fw with 
      Fw_nonmodular_wrapper_t.last_noticed_changes = 
        Dircopy_diff.destroy  
          (fw.Fw_nonmodular_wrapper_t.last_noticed_changes) destroyed_ones 
   } ;;
   
   
   let reflect_replacements_in_diff fw reps= {
      fw with 
      Fw_nonmodular_wrapper_t.last_noticed_changes = 
        Dircopy_diff.replace 
          (fw.Fw_nonmodular_wrapper_t.last_noticed_changes) reps
   } ;;
   
   let root fw = Fw_configuration.root (fw.Fw_nonmodular_wrapper_t.configuration);;
   
   let watched_files fw = fw.Fw_nonmodular_wrapper_t.watched_files ;;

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
      Fw_nonmodular_wrapper_t.watched_files = update_in_list_of_pairs fw w_files 
      (fw.Fw_nonmodular_wrapper_t.watched_files) ;
} ;;


let remove_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
    let fw2 ={
      fw with 
      Fw_nonmodular_wrapper_t.watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_nonmodular_wrapper_t.watched_files)  ;
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
      Fw_nonmodular_wrapper_t.watched_files =  
        (fw.Fw_nonmodular_wrapper_t.watched_files)@
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
           fw (old_string,new_string) ref_for_changed_files fw.Fw_nonmodular_wrapper_t.watched_files in 
    let new_fw ={
       fw with
       Fw_nonmodular_wrapper_t.watched_files = new_files ;
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
   ) (fw.Fw_nonmodular_wrapper_t.watched_files) in 
   {
      fw with 
       Fw_nonmodular_wrapper_t.watched_files = new_watched_files
   };;  

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

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Automatic.root fw)  in 
    let s_old_subdir = Dfa_subdirectory.without_trailing_slash old_subdir 
    and s_new_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
    let old_full_path = s_root^s_old_subdir 
    and new_full_path = s_root^s_new_subdir in 
    let cmd=" mv "^old_full_path^" "^new_full_path in 
        let _=Unix_command.hardcore_uc cmd in 
    let (files,reps)   =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_nonmodular_wrapper_t.watched_files) in 
   let fw2 = {
      fw with
      Fw_nonmodular_wrapper_t.watched_files = files  ;
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
        helper2_during_inspection fw ref_for_files fw.Fw_nonmodular_wrapper_t.watched_files in 
    let fw2 ={
       fw with
       Fw_nonmodular_wrapper_t.watched_files         = new_files ;
    }  in 
    let new_fw = Automatic.reflect_changes_in_diff fw2 changed_files in 
    (new_fw,changed_files);;         

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
   let (new_files,changed_files) =  rep fw.Fw_nonmodular_wrapper_t.watched_files  in 
   let fw2 ={
       fw with
       Fw_nonmodular_wrapper_t.watched_files = new_files;
   } in 
   let fw3 = Automatic.reflect_changes_in_diff fw2 changed_files in 
   (fw3,changed_files);;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path (Automatic.root fw)  in 
    let fw2= update_some_files fw ([rootless],[]) in 
    let (fw3,changed_files)=replace_string fw2 (replacee,replacer) in 
    let fw4 =  Automatic.reflect_changes_in_diff fw3 (rootless::changed_files) in         
    (fw4,(rootless::changed_files));;

   module Initialization = struct 

      module Private = struct 
        
        let first_init config =
           let the_root = config.Fw_configuration_t.root in 
           let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
           let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir config.Fw_configuration_t.ignored_subdirectories in 
           let list2 = Option.filter_and_unpack(
             fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
                      _->None 
           ) list1 in
           List.filter (Fw_configuration.test_for_admissibility config) list2 ;;
        
        end ;;
        
        let compute_and_store_modification_times config to_be_watched =
            let the_root = config.Fw_configuration_t.root in  
            let compute_info=( fun path->
              let s_root = Dfa_root.connectable_to_subpath the_root
              and s_path=Dfn_rootless.to_line path in 
              let file = s_root^s_path in 
              let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
              (path,mtime)
           ) in 
             {
               Fw_nonmodular_wrapper_t.configuration = config;
               watched_files = Image.image compute_info to_be_watched;
               last_noticed_changes = Dircopy_diff.empty_one;
             };;
        
        let init config = 
           let to_be_watched = Private.first_init config in 
           compute_and_store_modification_times config to_be_watched ;;
        
        
    
    end ;;  


end;;


let empty_one config= {
   Fw_nonmodular_wrapper_t.configuration = config;
   watched_files = [];
   last_noticed_changes = Dircopy_diff.empty_one;
};; 

let get_content = Automatic.get_content ;;
let get_mtime   = Automatic.get_mtime ;;
let get_mtime_or_zero_if_file_is_nonregistered  = Automatic.get_mtime_or_zero_if_file_is_nonregistered ;;

let initialize = Private.Initialization.init ;;

let inspect_and_update = Private.inspect_and_update;;

let of_concrete_object = Automatic.of_concrete_object ;;

let reflect_latest_changes_in_github fw opt_msg=
   let config = fw.Fw_nonmodular_wrapper_t.configuration in 
   let _= Reflect_change_in_github.backup config fw.Fw_nonmodular_wrapper_t.last_noticed_changes opt_msg in 
   {fw with Fw_nonmodular_wrapper_t.last_noticed_changes = Dircopy_diff.empty_one} ;; 


let register_rootless_paths = Private.register_rootless_paths;;

let remove_files = Private.remove_files;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;

let to_concrete_object = Automatic.to_concrete_object ;;

