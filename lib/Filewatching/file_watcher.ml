(*

#use"lib/Filewatching/file_watcher.ml";;

*)

exception Register_rootless_path_exn of string list;;
exception Already_registered_rootless_paths_exn of string list;;
exception Change_has_occurred ;;

module Private = struct


(* Start of level 4 *)

  

(* End of level 4 *)

(* Start of level 3 *)

let message_about_missing_files missing_files=
   let temp1=Image.image Dfn_rootless.to_line missing_files in
   "\n\n"^
   "The following files have been deleted without warning :\n"^
   (String.concat "\n" temp1)^
   "\n\n"
 ;;    

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
      let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw) 
      and s_path=Dfn_rootless.to_line path in 
      let file = s_root^s_path in 
      mtime file;;
 
let recompute_mtime_opt fw path =
     try Some(recompute_mtime fw path) with _ -> None ;;
 


(* End of level 3 *)


(* Start of level 2 *)


let announce_missing_files missing_files=
     if missing_files=[]
     then ()
     else (print_string(message_about_missing_files missing_files);flush stdout);;
            

let helper1_during_inspection _fw accu (rl,old_mtime,new_mtime)=
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
     (rl,old_mtime,opt) -> (rl,old_mtime,Option.get opt)
   ) good_temp1 
   and missing_files = Image.image (fun (rl,_,_)->rl) bad_temp1 in
   let _ = announce_missing_files missing_files in 
   let new_l_pairs = Image.image (helper1_during_inspection fw accu) good_pairs in 
   (new_l_pairs,List.rev(!accu));;

  let recompute_all_info fw path =
    let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw) 
    and s_path=Dfn_rootless.to_line path in 
    let file = s_root^s_path in 
    (path,mtime file);;


(* End of level 2 *)


(* Start of level 1 *)

let compute_changes_and_announce_them fw ~verbose=
   let ref_for_files=ref[]  in 
   let (new_files,changed_files)=
       helper2_during_inspection fw ref_for_files (Fw_poly.watched_files fw) in 
   let _ = (
     if verbose 
     then Strung.announce 
            ~trailer:"The following files have been changed :"
               ~printer:Dfn_rootless.to_line ~items:changed_files 
               ~separator: ", "
   ) in
   (new_files,changed_files);;   

(* let configuration fw = fw.File_watcher_t.configuration ;; *)

let get_content fw rootless = 
  let root = Fw_poly.root fw in 
  let s_ap = Dfn_common.recompose_potential_absolute_path root rootless in 
  Io.read_whole_file(Absolute_path.of_string s_ap);;     
    

let of_configuration_and_list config to_be_watched =
  let the_root = Fw_poly.root config in  
  let compute_info=( fun path->
    let s_root = Dfa_root.connectable_to_subpath the_root
    and s_path=Dfn_rootless.to_line path in 
    let file = s_root^s_path in 
    let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
    (path,mtime)
 ) in 
 Fw_poly.extend_fw_configuration_to_file_watcher config 
 ~watched_files:(Image.image compute_info to_be_watched) ;;
 

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
   
   

let update_in_list_of_pairs fw  to_be_updated pairs  =
Image.image (
   fun pair -> 
     let (rootless,_mtime)=pair in 
     if List.mem rootless to_be_updated 
     then recompute_all_info fw rootless 
     else pair
) pairs;;

let update_some_files fw w_files = 
   let new_watched_files = update_in_list_of_pairs fw w_files 
   (Fw_poly.watched_files fw) in 
   Fw_poly.set_watched_files fw new_watched_files ;;

   

(* End of level 1 *)



let adhoc_membership path selected_files_opt=
   match selected_files_opt with 
   None -> true 
   |Some selected_files -> List.mem path selected_files ;;

let apply_text_transformation_on_pair fw tr changed_files_ref selected_files_opt pair=
   let (path,_) = pair in 
   if not(adhoc_membership path selected_files_opt)
   then pair 
   else    
   let old_content = get_content fw path in 
   let new_content = tr old_content in
   if new_content = old_content   
   then pair 
   else 
   let _=(changed_files_ref:= path:: (!changed_files_ref)) in 
   let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw) 
   and s_path=Dfn_rootless.to_line path in 
   let file = s_root^s_path in  
   let ap=Absolute_path.of_string file in 
   let _=(Io.overwrite_with ap new_content) in 
   recompute_all_info fw path ;;



let apply_text_transformation_on_some_files fw tr l=
   let changed_files_ref=ref[]  in 
   let new_files = Image.image (
      apply_text_transformation_on_pair fw tr changed_files_ref (Some l)
   )  (Fw_poly.watched_files fw)  in 
   let fw2 = Fw_poly.set_watched_files fw new_files in 
  (fw2,!changed_files_ref);;  



   
let check_that_no_change_has_occurred fw =
  let (_new_files,changed_files)= compute_changes_and_announce_them fw ~verbose:true in
  if changed_files <> []
  then raise(Change_has_occurred)
  else () ;;       

  let deal_with_initial_comment_if_needed fw rless =
    if (Dfn_rootless.to_ending rless)<> Dfa_ending.ml 
    then ()
    else
       let root = Fw_poly.root fw in 
       let full = Dfn_join.root_to_rootless root rless in 
       let ap = Dfn_full.to_absolute_path full in 
       Put_use_directive_in_initial_comment.put_usual root ap
    ;;    
         

let first_init config =
   let the_root = Fw_poly.root config in 
   let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
   let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir (Fw_poly.ignored_subdirectories config) false in 
   let list2 = More_option.filter_and_unpack(
            fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
                     _->None 
   ) list1 in
   List.filter (Fw_configuration.test_for_admissibility config) list2 ;;
      
let inspect_and_update fw ~verbose = 
   let (new_files,changed_files)= compute_changes_and_announce_them fw ~verbose in 
   let fw2 = Fw_poly.set_watched_files fw new_files in
   (fw2,changed_files);;         
        
let latest_changes fw ~verbose =
   let (_,changed_files) = compute_changes_and_announce_them fw ~verbose  in 
   changed_files ;;
         
let of_configuration config = 
   let to_be_watched = first_init config in 
   of_configuration_and_list config to_be_watched ;;
        
         
let overwrite_file_if_it_exists fw rootless new_content =
   let root = Fw_poly.root fw in 
   if List.exists ( fun (r,_)->r=rootless ) (Fw_poly.watched_files fw)
   then let ap = Absolute_path.of_string (Dfn_common.recompose_potential_absolute_path root rootless) in 
         let _=Io.overwrite_with ap new_content in 
         let new_watched_files = update_in_list_of_pairs fw [rootless] (Fw_poly.watched_files fw) in 
         (Fw_poly.set_watched_files fw new_watched_files,true)
   else (fw,false);;


let register_rootless_paths fw rootless_paths= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw) in
   let nonexistent_paths = More_option.filter_and_unpack (
      fun rp-> let s_full_path = s_root^(Dfn_rootless.to_line rp)  in 
      if not(Sys.file_exists s_full_path)
      then Some(s_full_path)
      else None
   ) rootless_paths in 
   if nonexistent_paths<>[]
   then raise(Register_rootless_path_exn(nonexistent_paths))
   else 
   let old_watched_files = Fw_poly.watched_files fw in    
   let redundant_paths = List.filter (
      fun rp-> List.exists (fun (rl,_)->rl = rp) old_watched_files
   ) rootless_paths in 
   if redundant_paths<>[]
   then raise(Already_registered_rootless_paths_exn
       (Image.image Dfn_rootless.to_line redundant_paths))
   else    
   Fw_poly.set_watched_files fw (
      old_watched_files@
       (Image.image (recompute_all_info fw) rootless_paths)
   ) ;;



let remove_files fw rootless_paths=
 let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw) in 
 let removals_to_be_made = Image.image (
   fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
 ) rootless_paths in 
 let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
 Fw_poly.set_watched_files fw (
   List.filter (fun (path,_)->
      not(List.mem path rootless_paths)
   ) (Fw_poly.watched_files fw)
 ) ;;

let rename_files fw renaming_schemes =
    let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw)  in 
    let displacements_to_be_made = Image.image (
      fun (path1,path2)->" mv "^s_root^(Dfn_rootless.to_line path1)^" "^
      s_root^(Dfn_rootless.to_line path2)
    ) renaming_schemes in 
    let _=Unix_command.conditional_multiple_uc displacements_to_be_made in 
    let new_watched_files = Image.image (fun pair->
      let (path,_)=pair in 
      (match List.assoc_opt path renaming_schemes with
      Some(new_path) -> 
           let _ = (
             if  (Dfn_rootless.to_ending new_path) = Dfa_ending.ml
             then deal_with_initial_comment_if_needed fw new_path
           ) in 
           (new_path,recompute_mtime fw new_path)
      | None -> pair)
   ) (Fw_poly.watched_files fw)   in 
   Fw_poly.set_watched_files fw  new_watched_files ;;

  let rename_subdirectory_as fw (old_subdir,new_subdir)=
  let s_root = Dfa_root.connectable_to_subpath (Fw_poly.root fw)  in 
  let s_old_subdir = Dfa_subdirectory.without_trailing_slash old_subdir 
  and s_new_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
  let old_full_path = s_root^s_old_subdir 
  and new_full_path = s_root^s_new_subdir in 
  let cmd=" mv "^old_full_path^" "^new_full_path in 
      let _=Unix_command.hardcore_uc cmd in 
  let (files,reps)   =  rename_subdirectory_on_pairs fw (old_subdir,new_subdir) 
     (Fw_poly.watched_files fw) in 
  (Fw_poly.set_watched_files fw files,reps);;   
          
     
let plunge_fw_configuration config= 
   Fw_poly.extend_fw_configuration_to_file_watcher config 
   ~watched_files:[] ;;
 
end;;


let apply_text_transformation_on_some_files = Private.apply_text_transformation_on_some_files;;
let check_that_no_change_has_occurred = Private.check_that_no_change_has_occurred ;;
let inspect_and_update = Private.inspect_and_update;; 
let latest_changes = Private.latest_changes ;;
let of_configuration = Private.of_configuration ;;
let of_configuration_and_list = Private.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
let plunge_fw_configuration = Private.plunge_fw_configuration ;;
let register_rootless_paths = Private.register_rootless_paths;;
let remove_files = Private.remove_files;;
let rename_files = Private.rename_files;;
let rename_subdirectory_as = Private.rename_subdirectory_as;;
let update_some_files = Private.update_some_files ;; 
