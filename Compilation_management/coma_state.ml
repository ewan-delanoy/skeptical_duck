(* 
#use"Compilation_management/coma_state.ml";;
*)

(* Inherited values *)

let frontier_with_unix_world = Coma_state_field.frontier_with_unix_world;;
let root =Coma_state_field.root;;
let backup_dir =Coma_state_field.backup_dir;;
let gitpush_after_backup =Coma_state_field.gitpush_after_backup;;


let subdir_at_module = Coma_state_field.subdir_at_module ;;
let principal_ending_at_module = Coma_state_field.principal_ending_at_module ;;
let mli_presence_at_module = Coma_state_field.mli_presence_at_module ;;
let principal_mt_at_module = Coma_state_field.principal_mt_at_module ;;
let mli_mt_at_module = Coma_state_field.mli_mt_at_module ;;
let needed_libs_at_module  = Coma_state_field.needed_libs_at_module ;;
let direct_fathers_at_module = Coma_state_field.direct_fathers_at_module ;;
let ancestors_at_module = Coma_state_field.ancestors_at_module ;; 
let needed_dirs_at_module  = Coma_state_field.needed_dirs_at_module ;;
let product_up_to_date_at_module = Coma_state_field.product_up_to_date_at_module ;;
let directories = Coma_state_field.directories;;
let preq_types = Coma_state_field.preq_types;;


let set_frontier_with_unix_world = Coma_state_field.set_frontier_with_unix_world;;
let set_subdir_at_module = Coma_state_field.set_subdir_at_module ;;
let set_principal_ending_at_module = Coma_state_field.set_principal_ending_at_module ;;
let set_mli_presence_at_module = Coma_state_field.set_mli_presence_at_module ;;
let set_principal_mt_at_module = Coma_state_field.set_principal_mt_at_module ;;
let set_mli_mt_at_module = Coma_state_field.set_mli_mt_at_module ;;
let set_needed_libs_at_module  = Coma_state_field.set_needed_libs_at_module ;;
let set_direct_fathers_at_module = Coma_state_field.set_direct_fathers_at_module ;;
let set_ancestors_at_module = Coma_state_field.set_ancestors_at_module ;; 

let set_needed_dirs_at_module  = Coma_state_field.set_needed_dirs_at_module ;;
let set_product_up_to_date_at_module = Coma_state_field.set_product_up_to_date_at_module ;;
let set_directories = Coma_state_field.set_directories;;
let set_preq_types = Coma_state_field.set_preq_types;;


let ordered_list_of_modules = Coma_state_field.ordered_list_of_modules;;
let follows_it = Coma_state_field.follows_it_but_does_not_necessarily_depend_on_it;;
let all_used_subdirs = Coma_state_field.all_used_subdirs;;




(* End of inherited values *)


let endingless_at_module cs mn=
   Dfn_endingless_t.J(
        root cs,
        subdir_at_module cs mn,
        mn
    );;


let endingless_from_mildly_capitalized_module_name cs mname=
    endingless_at_module cs (Dfa_module.of_line(String.capitalize_ascii mname));;

let check_ending_in_at_module edg cs mn=
   if edg=principal_ending_at_module cs mn
   then true 
   else 
   if edg=Dfa_ending.mli
   then mli_presence_at_module cs mn
   else false;;



let acolytes_at_module cs mn=
  let eless = endingless_at_module cs mn in
  Option.filter_and_unpack (fun 
  edg->
     if check_ending_in_at_module edg cs mn
     then Some(Dfn_join.to_ending eless edg)
     else None
) Dfa_ending.all_ocaml_endings;;



let rootless_paths_at_module cs mn=
   Image.image Dfn_full.to_rootless_line (acolytes_at_module cs mn);;
  

  
let registered_endings_at_module cs mn=
  List.filter (fun edg->
  check_ending_in_at_module edg cs mn 
  ) Dfa_ending.all_ocaml_endings;;



let check_for_single_ending_at_module cs mn=
  if mli_presence_at_module cs mn
  then (principal_ending_at_module cs mn)=(Dfa_ending.mli)
  else true ;;



let size cs = List.length (ordered_list_of_modules cs);;      


let up_to_date_elesses cs =
   Option.filter_and_unpack (
     fun mn->
       if product_up_to_date_at_module cs mn
       then Some(endingless_at_module cs mn)
       else None
   )(ordered_list_of_modules cs);;

let modules_with_their_ancestors cs l=
   let temp1=List.filter (
     fun nm->List.mem nm l 
     ) (ordered_list_of_modules cs )   in 
   let temp2=Image.image (
     fun nm->
       (ancestors_at_module cs nm)@[nm] 
   ) temp1 in 
   let temp3=List.flatten temp2 in 
   Listennou.nonredundant_version temp3;;

let find_needed_data_for_file cs fn=
      let temp1=Look_for_module_names.names_in_ml_file fn in
      List.filter (
         fun mn->List.mem mn temp1  
      )(ordered_list_of_modules cs);;

let  find_needed_data cs mlx=
      let fn=Dfn_full.to_absolute_path mlx in
      find_needed_data_for_file cs fn;;    

 

let needed_dirs_and_libs_in_command cmod cs mn=
   let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
   let s_root=Dfa_root.connectable_to_subpath(root cs) in
   let dirs=
   "-I "^s_root^(Dfa_subdirectory.connectable_to_subpath(Compilation_mode.workspace cmod))
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    (needed_libs_at_module cs mn)) in
    String.concat " " ["";dirs;libs;""];;

let all_modules cs=
  Image.image (endingless_at_module cs) (ordered_list_of_modules cs);; 

let get_modification_time cs mn edg=
  if edg=principal_ending_at_module cs mn then principal_mt_at_module cs mn else 
  if edg=Dfa_ending.mli then mli_mt_at_module cs mn else 
  "0.";;

exception Non_existent_mtime of Dfn_full_t.t;;

let force_modification_time root_dir cs mlx=
      let edg=Dfn_full.to_ending mlx in
      let nm=Dfn_full.to_module mlx in
      let file=Dfn_full.to_line mlx in 
      let new_val=string_of_float((Unix.stat file).Unix.st_mtime)  in
      let cs2=(
        if edg=principal_ending_at_module cs nm 
        then set_principal_mt_at_module cs nm new_val
        else cs
      ) in
      let cs3=(
        if edg=Dfa_ending.mli
        then set_mli_mt_at_module cs2 nm new_val
        else cs2
      ) in     
      cs3;;


exception Non_registered_module of Dfn_endingless_t.t;;  
exception Derelict_children of Dfa_module_t.t*(Dfa_module_t.t list);;  
           
            
let unregister_module_on_monitored_modules cs eless=
  let nm=Dfn_endingless.to_module eless in
  let pre_desc=List.filter(
      fun mn7->
       List.mem nm ( ancestors_at_module cs mn7 )
  ) (ordered_list_of_modules cs) in
   if pre_desc<>[]
   then raise(Derelict_children(nm,pre_desc))
   else
   let acolytes=acolytes_at_module cs nm  in
   let cs2=Coma_state_field.remove_in_each_at_module cs nm in
   let old_preqtypes = Coma_state_field.preq_types cs2 in 
   let new_preqtypes = List.filter (fun (eless2,_)->eless2<>eless ) old_preqtypes in 
   let cs3=(
     if new_preqtypes <> old_preqtypes 
     then Coma_state_field.set_preq_types cs2 new_preqtypes
     else cs2
   ) in 
   let rootless_paths=Image.image Dfn_full.to_rootless_line acolytes in
   (cs3,rootless_paths);;     
                    

exception Non_registered_file of Dfn_full_t.t;;  
exception Abandoned_children of Dfn_full_t.t * (Dfa_module_t.t list);;
                      
                     
let unregister_mlx_file_on_monitored_modules cs mlxfile=
    let eless=Dfn_full.to_endingless mlxfile
    and nm=Dfn_full.to_module mlxfile in
    let pre_desc=List.filter(
      fun mn7->
      List.mem nm ( ancestors_at_module cs mn7)
    ) (ordered_list_of_modules cs) in
    if pre_desc<>[]
    then raise(Abandoned_children(mlxfile,pre_desc))
    else
    let edg=Dfn_full.to_ending mlxfile in
    if (not(check_ending_in_at_module edg cs nm))
    then raise(Non_registered_file(mlxfile))
    else if check_for_single_ending_at_module cs nm
         then let cs5=Coma_state_field.remove_in_each_at_module cs nm in 
              let old_preqtypes = Coma_state_field.preq_types cs5 in 
              let new_preqtypes = List.filter (fun (eless2,_)->eless2<>eless ) old_preqtypes in 
              let cs6=(
                if new_preqtypes <> old_preqtypes 
                then Coma_state_field.set_preq_types cs5 new_preqtypes
                else cs5
              ) in 
              cs6
         else (* if we get here, there are two registered endings, one of which
              is the mli *) 
              if edg=(Dfa_ending.mli)
              then (
                       let cs3=set_mli_presence_at_module cs nm false in 
                       set_mli_mt_at_module cs3 nm "0."
                   )
               else 
                     let old_mt=principal_mt_at_module cs nm in
                     (
                      let cs4=set_principal_ending_at_module cs nm (Dfa_ending.mli) in 
                      set_principal_mt_at_module cs4 nm old_mt
                    );;
            


let compute_subdirectories_list cs=
  let temp1=Image.image Dfa_subdirectory.without_trailing_slash (all_used_subdirs cs) in
    let temp2=Set_of_strings.sort temp1 in
    let temp3=Set_of_strings.forget_order temp2 in
    Image.image Dfa_subdirectory.of_line temp3;;

let  check_registrations cs eless=
   let mn=Dfn_endingless.to_module eless in 
   Dfa_ending.compute_on_all_ocaml_endings 
      (fun edg->check_ending_in_at_module edg cs mn);;


module PrivateTwo=struct


let find_needed_libraries cs mlx ordered_ancestors=
  let fn=Dfn_full.to_absolute_path mlx in
  let temp1=Look_for_module_names.names_in_ml_file fn in
  List.filter
  (
    fun lib->
      if List.exists 
         (fun mdl->List.mem(Dfa_module.of_line mdl)(temp1))
           (Ocaml_library.modules_telling_a_library_away lib)
      then true
      else List.exists 
           (fun mn->
            List.mem lib (needed_libs_at_module cs mn) ) 
           ordered_ancestors
  )
  Ocaml_library.all_libraries;;


let find_needed_directories cs mlx ordered_ancestors=
  let temp1=Image.image (fun mn->
    Set_of_polys.sort(needed_dirs_at_module cs mn)) ordered_ancestors in
  let subdir_in_mlx=Dfn_full.to_subdirectory mlx in
  let temp2=(
      if subdir_in_mlx<>Dfa_subdirectory.main 
      then Set_of_polys.singleton(subdir_in_mlx)::temp1
      else temp1
  ) in    
  let temp3=Set_of_polys.fold_merge temp2 in
  Set_of_polys.forget_order temp3;;              
                    
end;;  

let compute_principal_ending (mlr,mlir,mllr,mlyr)=
    let temp1=List.combine
      [mlr;mllr;mlyr]
      [Dfa_ending.ml;Dfa_ending.mll;Dfa_ending.mly] in
    let temp2=Option.filter_and_unpack (
       fun (bowl,ending)->if bowl then Some(ending) else None 
    ) temp1 in
    if temp2=[] then Dfa_ending.mli else List.hd temp2;;

let md_compute_modification_time hm edg=
  let mlx=Dfn_join.to_ending hm edg in
  let file=Dfn_full.to_line mlx in 
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;

let md_compute_modification_times hm=
      Dfa_ending.compute_on_all_ocaml_endings (md_compute_modification_time hm);;
    
let md_associated_modification_time  (ml_mt,mli_mt,mll_mt,mly_mt) edg=
  match Dfa_ending.restrict_to_ocaml_ending edg with
     Dfa_ocaml_ending_t.Ml->ml_mt
    |Mli->mli_mt
    |Mll->mll_mt
    |Mly->mly_mt;;  

let complete_info cs  mlx=
  let hm=Dfn_full.to_endingless mlx  in
  let modules_written_in_file=find_needed_data cs mlx in
  let (mlr,mlir,mllr,mlyr)=check_registrations cs hm
  and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
  let pr_end=compute_principal_ending (mlr,mlir,mllr,mlyr) in
  let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let temp1=Image.image 
          (fun mn->
           Set_of_polys.sort(ancestors_at_module cs mn)) 
          modules_written_in_file in
  let temp2=Set_of_polys.fold_merge ((Set_of_polys.sort(modules_written_in_file) )::temp1) in
  let tempf=(fun mn->
              if Set_of_polys.mem mn temp2
              then Some(mn)
              else None) in
  let allanc=Option.filter_and_unpack tempf (ordered_list_of_modules cs) in
  let libned=PrivateTwo.find_needed_libraries cs mlx modules_written_in_file
  and dirned=PrivateTwo.find_needed_directories cs mlx modules_written_in_file in
  (hm,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;

let update_just_one_module cs rootless =
    let mn = Dfn_rootless.to_module rootless in 
    if not(List.mem mn (ordered_list_of_modules cs))
    then cs 
    else let mlx=Dfn_join.root_to_rootless (root cs) rootless in 
         let (_,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=complete_info cs  mlx in 
         Coma_state_field.set_in_each cs mn (pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated);;


  let check_unix_presence eless edg=
    let full_path=Dfn_join.to_ending eless edg in 
    Sys.file_exists(Dfn_full.to_line full_path);;

let  check_unix_presences hm=
    Dfa_ending.compute_on_all_ocaml_endings (fun edg->check_unix_presence hm edg);;  

let registrations_for_lonely_ending old_edg =
   let edg = Dfa_ending.restrict_to_ocaml_ending old_edg in 
    (
      edg=Dfa_ocaml_ending_t.Ml,
      edg=Dfa_ocaml_ending_t.Mli,
      edg=Dfa_ocaml_ending_t.Mll,
      edg=Dfa_ocaml_ending_t.Mly
     );;

     
let complete_id_during_new_module_registration cs  mlx=
    let eless=Dfn_full.to_endingless mlx 
    and edg=Dfn_full.to_ending mlx in
    let modules_written_in_file=find_needed_data cs mlx in
    let (mlp,mlir,mllr,mlyr)=registrations_for_lonely_ending edg
    and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times eless in
    let pr_end=edg in
    let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let temp1=Image.image 
          (fun mn->
           Set_of_polys.sort(ancestors_at_module cs mn)) 
          modules_written_in_file in
    let temp2=Set_of_polys.fold_merge ((Set_of_polys.sort(modules_written_in_file) )::temp1) in
    let tempf=(fun mn->
              if Set_of_polys.mem mn temp2
              then Some(mn)
              else None) in
    let allanc=Option.filter_and_unpack tempf (ordered_list_of_modules cs) in
    let libned=PrivateTwo.find_needed_libraries cs mlx modules_written_in_file
    and dirned=PrivateTwo.find_needed_directories cs mlx modules_written_in_file in
    (eless,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;
    

let above cs eless=
  let nm=Dfn_endingless.to_module eless in
  ancestors_at_module cs nm;;
 

let below cs eless=
        let mn0=Dfn_endingless.to_module eless  in
        Option.filter_and_unpack(fun mn->
            if List.mem mn0 (ancestors_at_module cs mn)
            then Some(mn)
            else None) (ordered_list_of_modules cs);;    

let directly_above cs eless=
    let nm=Dfn_endingless.to_module eless in
     direct_fathers_at_module cs nm;;     

let directly_below cs eless=
        let mn0=Dfn_endingless.to_module eless  in
        Option.filter_and_unpack(fun mn->
            if List.mem mn0 (direct_fathers_at_module cs mn)
            then Some(mn)
            else None) (ordered_list_of_modules cs);;        

let ordered_as_in_coma_state cs l=
   List.filter (fun x->List.mem x l) (ordered_list_of_modules cs);;

let above_one_in_several_or_inside cs l=
  let temp1=Image.image (ancestors_at_module cs) l in
  let temp2=List.flatten (l::temp1) in
  ordered_as_in_coma_state cs  temp2;;


let all_mlx_files cs=
  let mods=ordered_list_of_modules cs in
  List.flatten(Image.image(acolytes_at_module cs) mods);;                
      
let all_mlx_paths cs=Image.image Dfn_full.to_absolute_path 
        (all_mlx_files cs);;  

let all_rootless_paths cs=
    let mods=ordered_list_of_modules cs in
    List.flatten(Image.image(rootless_paths_at_module cs) mods);;  
     

let short_paths_inside_subdirectory cs subdir =
   let s_root = Dfa_root.connectable_to_subpath (root cs) in 
   let s_subdir_full_name=s_root^(Dfa_subdirectory.connectable_to_subpath subdir) in 
   let the_subdir=Directory_name.of_string s_subdir_full_name in 
   let temp1=More_unix.complete_ls_with_nondirectories_only the_subdir in 
   let n=String.length s_root in 
   Image.image (
    fun ap->let s_ap=Absolute_path.to_string ap in 
    Cull_string.cobeginning n s_ap
   ) temp1;;


let files_containing_string cs some_string=
let temp1=all_mlx_paths cs in
List.filter (fun ap->Substring.is_a_substring_of 
  some_string (Io.read_whole_file ap)) temp1;;


let system_size cs=List.length(ordered_list_of_modules cs);;

exception Inconsistent_constraints of Dfa_module_t.t*Dfa_module_t.t;;
exception Bad_upper_constraint of Dfa_module_t.t;;  


exception Nonregistered_module_during_reposition of Dfn_endingless_t.t;;  

 
let reposition_module cs eless (l_before,l_after)=
    let l_mods = ordered_list_of_modules cs in 
    let n=List.length(l_mods) in 
    let find_idx=(fun mn->Listennou.find_index mn l_mods) 
    and get=(fun j->List.nth l_mods (j-1)) in
    let indices_before=Image.image find_idx l_before
    and indices_after=Image.image find_idx l_after in
    let max_before=(if indices_before=[] then 1 else Max.list indices_before)
    and min_after=(if indices_after=[] then n else Min.list indices_after)
    in
    let pivot=get max_before in 
    if max_before>min_after
    then raise(Inconsistent_constraints(pivot,get min_after))
    else 
    if max_before>(find_idx eless)
    then raise(Bad_upper_constraint(pivot))
    else 
    Coma_state_field.reposition_in_each cs pivot eless;;  

let rename_directory_on_data (old_subdir,new_subdirname) cs= 
  let ren_sub=Dfa_subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) in 
  let cs2=Coma_state_field.modify_all_subdirs cs ren_sub in 
  Coma_state_field.modify_all_needed_dirs cs2 ren_sub;;



let find_value_definition cs s= 
  if not(String.contains s '.')
  then None
  else
  let j1=String.index(s)('.')+1 in
  let module_name=Cull_string.beginning (j1-1) s in
  let nm=Dfa_module.of_line(String.uncapitalize_ascii(module_name)) in
  let hm1=endingless_at_module cs nm in
  let ap1=Dfn_full.to_absolute_path(Dfn_join.to_ending hm1 
     Dfa_ending.ml) in
  let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
  Option.seek (
     fun itm->Ocaml_gsyntax_item.name(itm)=s
  ) temp1;;
     

let all_ml_absolute_paths cs=  
Option.filter_and_unpack (fun mn->
  if not(check_ending_in_at_module Dfa_ending.ml cs mn)
  then None
  else 
  let hm=endingless_at_module cs mn in
  let mlx=Dfn_join.to_ending hm Dfa_ending.ml in
  Some(Dfn_full.to_absolute_path mlx)
) (ordered_list_of_modules cs);;

let modules_using_value cs value_name =
  Option.filter_and_unpack (fun mn->
  let eless=endingless_at_module cs mn
  and pr_end=principal_ending_at_module cs mn in
  let mlx=Dfn_join.to_ending eless pr_end in
   let ap=Dfn_full.to_absolute_path mlx in
   if Substring.is_a_substring_of 
       value_name (Io.read_whole_file ap)
   then Some eless
   else None ) (ordered_list_of_modules cs);;





let update_ancs_libs_and_dirs_at_module cs mn=
  let eless=endingless_at_module cs mn  
  and pr_end=principal_ending_at_module cs mn in
  let mlx=Dfn_join.to_ending eless pr_end in 
  let fathers=direct_fathers_at_module cs mn in
  let separated_ancestors=Image.image 
  (fun nm2->
    Set_of_polys.safe_set(ancestors_at_module cs nm2)
  ) fathers in
  let ancestors_with_wrong_order=Set_of_polys.fold_merge((Set_of_polys.safe_set fathers)::separated_ancestors) in
  let ordered_ancestors=List.filter (
    fun mn->Set_of_polys.mem mn ancestors_with_wrong_order
  ) (ordered_list_of_modules cs) in
  let new_libs=PrivateTwo.find_needed_libraries cs mlx ordered_ancestors
  and new_dirs=PrivateTwo.find_needed_directories cs mlx ordered_ancestors in
  let cs2=set_ancestors_at_module cs mn ordered_ancestors in 
  let cs3=set_needed_libs_at_module cs2 mn new_libs in
  set_needed_dirs_at_module cs3 mn new_dirs;;


let update_ancs_libs_and_dirs cs=
  let cs_walker=ref(cs) in 
  let _=List.iter(fun mn->cs_walker:=update_ancs_libs_and_dirs_at_module (!cs_walker) mn)(ordered_list_of_modules cs) in
  (!cs_walker);;  


module PrivateThree=struct

    let message_about_circular_dependencies printer cycles= 
      if cycles=[]
      then ""
      else
      let temp1=Image.image(fun cycle->
        let ttemp1=Image.image printer cycle in
         String.concat " -> " ttemp1 
      ) cycles in
      let temp2=String.concat "\n\n" temp1 in
      temp2;;
    
    exception Circular_dependencies of string;;
    
    let treat_circular_dependencies tolerate_cycles printer cycles=
      if cycles=[]
      then ()
      else let msg=message_about_circular_dependencies printer cycles in  
           if tolerate_cycles
           then (print_string msg;flush stdout)     
           else raise(Circular_dependencies(msg));; 
           
    let message_about_changed_modules changed_modules=
      let temp1=Image.image Dfa_module.to_line changed_modules in
      "\n\n\n"^
      "The following modules have been directly changed :\n"^
      (String.concat "\n" temp1)^
      "\n\n\n"
    ;;       
           
    let announce_changed_modules changed_modules=
      if changed_modules=[]
      then ()
      else (print_string(message_about_changed_modules changed_modules);flush stdout);;
             
    
    let put_md_list_back_in_order tolerate_cycles 
      cs initially_active_nms=
      let md_list=ordered_list_of_modules cs in
      let coat=Memoized.make (fun nm->direct_fathers_at_module cs nm) in
      let (cycles,reordered_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles (
        (fun nm->
           let middle = Dfn_endingless.to_middle ( endingless_at_module cs nm) in 
           Dfn_middle.to_line middle )
      ) cycles in     
      let cs2=Coma_state_field.reorder cs (Image.image fst reordered_list) in    
      let cs3=update_ancs_libs_and_dirs cs2 in 
      let active_descendants=Option.filter_and_unpack (
          fun nm->
            if List.mem nm initially_active_nms
            then Some(nm)
            else
            if List.exists (fun nm2->List.mem nm2 initially_active_nms) 
                 (ancestors_at_module cs nm)
            then Some(nm)
            else None
      ) (ordered_list_of_modules cs) in  
      (cs3,active_descendants);;
     
end;; 
     
let md_recompute_modification_time eless edg=
  let mlx=Dfn_join.to_ending eless edg in
  let file=Dfn_full.to_line mlx in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;
  

let quick_update cs mn=
  let eless =endingless_at_module cs mn 
  and pr_ending=principal_ending_at_module cs mn in
  let middle = Dfn_endingless.to_middle eless in 
  if (Dfn_middle.to_line middle)=Coma_constant.name_for_debugged_module
  then None
  else
  let mli_modif_time=md_recompute_modification_time eless Dfa_ending.mli 
  and pr_modif_time=md_recompute_modification_time eless pr_ending 
  and old_mli_modif_time=mli_mt_at_module cs mn
  and old_pr_modif_time=principal_mt_at_module cs mn 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  let mn = Dfn_endingless.to_module eless in 
  if (old_values=new_values)&&(product_up_to_date_at_module cs mn)
  then None
  else
  let mlx=Dfn_join.to_ending eless pr_ending in
  let direct_fathers=find_needed_data cs mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;
    
let latest_changes cs = 
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun nm->
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (rootless_paths_at_module cs nm))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun mname->
    match quick_update (!cs_walker) mname with
    None->()
    |_->
    (
    declare_changed(mname);
    )
)(ordered_list_of_modules cs) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then [] else
let _=PrivateThree.announce_changed_modules changed_modules in
(!ref_for_changed_shortpaths);; 



let printer_equipped_types_from_data cs=
  Option.filter_and_unpack (
    fun mn->
    let eless=endingless_at_module cs mn
    and pr_end=principal_ending_at_module cs mn in
    let mlx=Dfn_join.to_ending eless pr_end in
    let ap=Dfn_full.to_absolute_path mlx in
    let text=Io.read_whole_file ap in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(eless)
    else None
  ) (ordered_list_of_modules cs);;
 



exception Already_registered_file of Dfn_full_t.t;;  
exception Overcrowding of Dfn_full_t.t*(Dfa_ending_t.t list);;
exception Bad_pair of Dfn_full_t.t*Dfa_ending_t.t;; 


let register_mlx_file_on_monitored_modules cs mlx_file =
          let eless=Dfn_full.to_endingless mlx_file
          and ending=Dfn_full.to_ending mlx_file in 
          let nm=Dfn_full.to_module mlx_file in
          if not(Coma_state_field.test_module_for_registration cs nm)
          then  let info=complete_id_during_new_module_registration cs mlx_file in
                Coma_state_field.push_right_in_each cs info 
          else
          let edgs=registered_endings_at_module cs nm in
          if List.length(edgs)>1
          then  raise(Overcrowding(mlx_file,edgs))
          else  
          if List.mem ending edgs
          then raise(Already_registered_file(mlx_file))
          else
          if (not(List.mem Dfa_ending.mli (ending::edgs)))
          then raise(Bad_pair(mlx_file,List.hd edgs))
          else 
          if ending = Dfa_ending.mli
          then let old_pr_end = List.hd edgs in
               let old_mlx_file =
                Dfn_join.to_ending eless old_pr_end in
              let (eless,_,old_mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=
                 complete_info cs old_mlx_file in
               let new_mlimt = md_compute_modification_time eless ending in
               let new_dt=(old_pr_end,true,prmt,new_mlimt,libned,dirfath,allanc,dirned,false) in
               Coma_state_field.set_in_each cs nm new_dt
          else
          let new_dt=complete_id_during_new_module_registration cs mlx_file in 
          let (_,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=new_dt in
          let temp3=List.rev(dirfath) in
          if temp3=[]
          then Coma_state_field.set_in_each cs nm (pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated) 
          else  
          let last_father=List.hd(temp3) in
          let nm=Dfn_endingless.to_module eless in 
          let cs_walker=ref(cs) in 
          let _=List.iter(
                 fun current_module ->
              let current_anc= ancestors_at_module (!cs_walker) current_module in  
              if not(List.mem nm current_anc)
              then ()
              else  
                   let current_libs= needed_libs_at_module cs current_module in
                   let new_ancestors=Option.filter_and_unpack(
                      fun nm2->
                      if (List.mem nm2 allanc)||(List.mem nm2 current_anc)
                      then Some(nm2)
                      else None
                    ) (ordered_list_of_modules (!cs_walker)) 
                    and new_libs=List.filter (
                      fun lib->(List.mem lib libned)||(List.mem lib current_libs)
                    ) Ocaml_library.all_libraries in  
                    let ordered_dirs=Set_of_polys.merge
                       (Set_of_polys.safe_set(needed_dirs_at_module (!cs_walker) current_module))
                       (Set_of_polys.safe_set (dirned)) in
                    let new_dirs=Set_of_polys.forget_order(ordered_dirs) in
                    cs_walker:=set_ancestors_at_module (!cs_walker) current_module new_ancestors;
                    cs_walker:=set_needed_libs_at_module (!cs_walker) current_module new_libs;
                    cs_walker:=set_needed_dirs_at_module (!cs_walker) current_module new_dirs;
          )(follows_it cs last_father) in 
          let _=
            ( 
              cs_walker:=Coma_state_field.remove_in_each_at_module (!cs_walker) nm;
              cs_walker:=Coma_state_field.push_after_module_in_each (!cs_walker) last_father new_dt;  
            )
          in
          (!cs_walker);;

module Modern = struct 
(*
exception Unregistered_cmi of Dfn_endingless_t.t;;
exception Unregistered_cmo of Dfn_endingless_t.t;;
*)
let command_for_cmi (cmod:Compilation_mode_t.t) dir cs hm=
    let nm=Dfn_endingless.to_module hm in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=Dfn_endingless.to_line hm in
    let mli_reg=check_ending_in_at_module Dfa_ending.mli cs nm in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                       then Some("mv "^s_fhm^".o "^s_root^workdir) 
                       else None) in 
    let central_cmd=
        (Compilation_mode.executioner cmod)^
        (needed_dirs_and_libs_in_command cmod cs nm)^
            " -c "^s_fhm^ending in
            let full_mli=s_fhm^".mli" in
            let almost_full_answer=(
            if (not mli_reg)
               &&(Sys.file_exists(full_mli))
            then (* 
                   in this situation the mli file exists but is not registered.
                   So the compilation manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
                  *)
                  let dummy_mli=s_root^"uvueaoqhkt" in
                  [
                   "mv "^full_mli^" "^dummy_mli;
                   central_cmd;
                   "mv "^s_fhm^".cm* "^s_root^workdir;
                   "mv "^dummy_mli^" "^full_mli
                  ] 
            else  [
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^workdir
                   ]
            ) in 
            Option.add_element_on_the_right almost_full_answer opt_exec_move;;
   
  let command_for_cmo (cmod:Compilation_mode_t.t) dir cs eless=
    let nm=Dfn_endingless.to_module eless in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_eless=Dfn_endingless.to_line eless in
    let dir_and_libs=needed_dirs_and_libs_in_command cmod cs nm in
    let mli_reg=check_ending_in_at_module Dfa_ending.mli cs nm in 
    let full_mli=s_eless^".mli" in
    let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                       then Some("mv "^s_eless^".o "^s_root^workdir) 
                       else None) in 
    let central_cmds=
    [ 
      (Compilation_mode.executioner cmod)^dir_and_libs^" -c "^s_eless^".ml";
      "mv "^s_eless^".cm* "^s_root^workdir
    ] in 
    let almost_full_answer= 
    (if (not mli_reg) &&(Sys.file_exists(full_mli))
    then 
          (* 
                   in this situation the mli file exists but is not registered.
                   So the compilation manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
          *)
                  let dummy_mli=s_root^"uvueaoqhkt" in
                  [
                   "mv "^full_mli^" "^dummy_mli
                  ]
                  @ 
                   central_cmds
                  @ 
                  [ 
                   "mv "^dummy_mli^" "^full_mli
                  ] 
    else central_cmds)
    in Option.add_element_on_the_right almost_full_answer opt_exec_move;; 

exception  Unregistered_element of Dfn_endingless_t.t;;   

let command_for_module_separate_compilation cmod cs eless=
    let dir = root cs in 
    let nm=Dfn_endingless.to_module eless in
    let mli_reg=check_ending_in_at_module Dfa_ending.mli cs nm
    and ml_reg=check_ending_in_at_module Dfa_ending.ml cs nm in
    let temp2=(
    let co=command_for_cmo cmod dir cs eless in 
    if mli_reg
    then let ci=command_for_cmi cmod dir cs eless in 
         if ml_reg
         then [ci;co]
         else [ci]
    else [co]) in 
    List.flatten temp2;;

exception  Command_for_predebuggable_or_preexecutable_exn;;

let command_for_predebuggable_or_preexecutable cmod cs short_path=
    if cmod=Compilation_mode_t.Usual then raise(Command_for_predebuggable_or_preexecutable_exn) else 
    let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath(root cs))^short_path) in 
    let nm_direct_deps = Look_for_module_names.names_in_ml_file full_path in 
    let nm_deps =modules_with_their_ancestors cs nm_direct_deps in 
    let nm_deps_with_subdirs = Image.image (
       fun nm->
               let subdir=subdir_at_module cs nm in 
        (subdir,nm)
    ) nm_deps in 
    let s_root=Dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) in
    let unpointed_short_path = Cull_string.before_rightmost short_path '.' in 
    let libs_for_prow = 
      Set_of_polys.sort(
      Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Image.image Dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Image.image 
     (fun (_,nm) -> Set_of_polys.sort(needed_libs_at_module cs nm)) nm_deps_with_subdirs in
    let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let libs=String.concat(" ")
      (Image.image(fun z->Ocaml_library.file_for_library(z)^".cma") pre_libs2) in 
    Option.add_element_on_the_right   
    [ 
      (Compilation_mode.executioner cmod)^
      " -I "^s_root^workdir^" "^
      libs^" -c "^s_root^unpointed_short_path^".ml";
    ] 
    (Unix_command.mv (s_root^unpointed_short_path^".cm*") (s_root^workdir) )
    ;;          




exception  Command_for_debuggable_or_executable_exn;;

let command_for_debuggable_or_executable cmod cs rootless_path=
    if cmod=Compilation_mode_t.Usual then raise(Command_for_debuggable_or_executable_exn) else 
    let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath (root cs))^rootless_path) in 
    let nm_direct_deps = Look_for_module_names.names_in_ml_file full_path in 
    let nm_deps =modules_with_their_ancestors cs nm_direct_deps in 
    let nm_deps_with_subdirs = Image.image (
       fun nm->let subdir=subdir_at_module cs nm in 
        (subdir,nm)
    ) nm_deps in 
    let s_root=Dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) 
    and ending=Compilation_mode.ending_for_element_module cmod 
    and product_ending=Compilation_mode.ending_for_final_product cmod  in
    let cm_elements_but_the_last = Image.image (
      fun (subdir,nm)->(Dfa_module.to_line nm)^ending
    ) nm_deps_with_subdirs in 
    let unpointed_short_path = Cull_string.before_rightmost rootless_path '.' in 
    let nm_name = (Cull_string.after_rightmost unpointed_short_path '/') in 
    let last_cm_element=nm_name^ending in 
    let all_cm_elements= (cm_elements_but_the_last) @ [last_cm_element] in 
    let libs_for_prow = 
      Set_of_polys.sort(
      Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Image.image Dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Image.image 
     (fun (_,nm) -> Set_of_polys.sort(needed_libs_at_module cs nm)) nm_deps_with_subdirs in
    let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let libs=String.concat(" ")
      (Image.image(fun z->Ocaml_library.file_for_library(z)^".cma") pre_libs2) in 
    Option.add_element_on_the_right  
    [ 
      ((Compilation_mode.executioner cmod)^
       " -I "^s_root^workdir^" "^
       libs^" -o "^nm_name^product_ending^
        (String.concat " " all_cm_elements));
    ]
    (
      Unix_command.mv ((Sys.getcwd())^"/"^nm_name^product_ending) (s_root^workdir)
    )
    ;;          




end;;



module Ocaml_target_making=struct




exception Failed_during_compilation of (Dfa_module_t.t*Dfn_endingless_t.t*string);;

let rec helper_for_feydeau  (cmod:Compilation_mode_t.t) cs (rejected,treated,to_be_treated)=
     match to_be_treated with 
     []->(cs,rejected,List.rev treated)
     |triple::other_triples->
       let (nm,eless,cmd)=triple in
       if (Unix_command.uc cmd)=0
       then 
            let cs2=set_product_up_to_date_at_module cs nm true in 
            helper_for_feydeau cmod cs2 (rejected,(nm,eless)::treated,other_triples)
       else if (cmod<>Compilation_mode_t.Usual)
            then raise(Failed_during_compilation(triple))
            else 
            let triples_after=snd(Prepared.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
            let (rejected_siblings_as_triples,survivors)=List.partition
           (
              fun (nm2,_,_)->
                List.mem nm (ancestors_at_module cs nm2)
           ) triples_after in 
           let rejected_siblings_with_redundancies =  
              Image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
           let rejected_siblings = Listennou.nonredundant_version rejected_siblings_with_redundancies in    
           let newly_rejected = (nm,eless)::rejected_siblings in 
           let cs_walker=ref(cs) in 
           let _=List.iter(
              fun (nm3,hm3)->
                cs_walker:=set_product_up_to_date_at_module (!cs_walker) nm3 false
           ) newly_rejected in 
           helper_for_feydeau cmod (!cs_walker) (rejected@newly_rejected,treated,survivors) ;;




let dependencies_inside_shaft cmod cs (opt_modnames,opt_rootless_path)=
   match cmod with 
   Compilation_mode_t.Usual->Option.unpack opt_modnames
   |_->let rootless_path=Option.unpack opt_rootless_path in 
       let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath (root cs))^rootless_path) in 
       let nm_direct_deps = Look_for_module_names.names_in_ml_file full_path in 
       let nm_deps=modules_with_their_ancestors cs nm_direct_deps in 
       List.filter (fun mn->List.mem mn nm_deps) (ordered_list_of_modules cs);;



let list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_modulenames,opt_rootless_path)=
   let l=dependencies_inside_shaft cmod cs (opt_modulenames,opt_rootless_path) in 
   let temp1=Image.image (fun mn->
     let eless=endingless_at_module cs mn in 
     let cmds=Modern.command_for_module_separate_compilation cmod cs eless in 
    Image.image (fun cmd->(mn,endingless_at_module cs mn,cmd) ) cmds ) l in 
    List.flatten temp1;;



let list_of_commands_for_connecting_part_of_feydeau cmod cs (_,opt_rootless_path)=
   let cmds=(
   match cmod with 
   Compilation_mode_t.Usual->[] 
   |_->
      let rootless_path=Option.unpack opt_rootless_path in 
      Modern.command_for_predebuggable_or_preexecutable cmod cs rootless_path) in 
   cmds;;


let list_of_commands_for_end_part_of_feydeau cmod cs (_,opt_rootless_path)= 
   let cmds=(
   match cmod with 
   Compilation_mode_t.Usual->[] 
   |_->
      let rootless_path=Option.unpack opt_rootless_path in 
      Modern.command_for_debuggable_or_executable cmod cs rootless_path) in 
   cmds;;   

let list_of_commands_for_ternary_feydeau cmod cs short_path=
   let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod cs (None,Some(short_path)) in 
   let cmds1=Image.image (fun (_,_,cmd)->cmd) pre_cmds1
   and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod cs (None,Some(short_path))
   and cmds3=list_of_commands_for_end_part_of_feydeau cmod cs (None,Some(short_path)) in 
   cmds1@cmds2@cmds3;;



let shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)=
  let cmds=list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in  
  helper_for_feydeau cmod cs ([],[],cmds);; 


  
let end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)=
  match cmod with 
   Compilation_mode_t.Usual->()
   |_->
     let all_cmds=
       (list_of_commands_for_connecting_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path))@
       (list_of_commands_for_end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)) in 
     let _=Image.image  Unix_command.hardcore_uc all_cmds in 
     ()



let feydeau cmod cs (opt_modnames,opt_rootless_path)=
  let answer=shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in 
  let _=end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in 
  answer;; 


let usual_feydeau cs modnames = feydeau Compilation_mode_t.Usual cs (Some(modnames),None);;

end;;  


let add_printer_equipped_type cs mn=
  set_preq_types cs ((preq_types cs)@[mn]);;

let remove_printer_equipped_type cs mn=
  set_preq_types cs (List.filter (fun mn2->mn2<>mn) (preq_types cs));;

let uple_form cs=
  (cs,
   directories cs,
   preq_types cs
   );;


let unregister_mlx_file_on_targets root_dir cs mlx=
    let mn=Dfn_full.to_module mlx in 
    let following = mn::(follows_it cs mn) in  
    let was_lonely=
      (List.length(registered_endings_at_module cs mn)=1) in 
    let _=set_product_up_to_date_at_module cs mn false in 
    let cs2=unregister_mlx_file_on_monitored_modules cs mlx in
    let new_dirs=compute_subdirectories_list cs2 in
    let cs3=(if was_lonely 
           then cs2
           else ( fun (cs4,_,_)->cs4)
           (Ocaml_target_making.usual_feydeau 
             cs2 following) ) in 
    (cs3,new_dirs);;   

exception FileWithDependencies of 
Dfn_full_t.t*(Dfa_module_t.t list);;


let forget_file_on_targets root_dir pair ap=
  let (cs,dirs)=pair in
  let rootless_path = Dfn_common.decompose_absolute_path_using_root ap root_dir in 
  let mlx = Dfn_join.root_to_rootless root_dir rootless_path in 
  let eless=Dfn_full.to_endingless mlx  in
  let nm=Dfn_endingless.to_module eless in
  if not(Coma_state_field.test_module_for_registration cs nm)
  then pair  
  else 
   let bel=below cs eless in
    if bel=[]
    then let fn=Dfn_endingless.to_line eless  in
         let _=Image.image
         (fun edg->Unix_command.uc("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         unregister_mlx_file_on_targets root_dir cs mlx
    else raise(FileWithDependencies(mlx,bel));;



let forget_file cs ap=
    let (cs2,new_dirs)= 
     forget_file_on_targets (root cs) (cs,directories cs) ap in  
     set_directories cs2 new_dirs;;




let read_persistent_version x=
        let full_path=Dfn_join.root_to_rootless (root x)  Coma_constant.rootless_path_for_targetfile in
        let ap= Dfn_full.to_absolute_path full_path in
        let the_archive=Io.read_whole_file ap in
        let archived_object = Crobj_parsing.parse the_archive in 
        Coma_state_field.of_concrete_object archived_object;;      

module Try_to_register=struct

  let mlx_file cs mlx_file=
    try(Some(register_mlx_file_on_monitored_modules 
        cs mlx_file)) with _->None;;  

module Private=struct

exception Pusher_exn;;

let pusher  (cs,failures,yet_untreated)=
     match yet_untreated with
      []->raise(Pusher_exn)
      |mlx::others->
      (
        match mlx_file cs mlx with
        None->(cs,mlx::failures,others)
        |Some(nfs)->(nfs,failures,others)
      );; 

let rec iterator x=
   let (cs,failures,yet_untreated)=x in
   match yet_untreated with
      []->(failures,cs)
      |mlx::others->iterator(pusher x);;   

end;;

let mlx_files cs mlx_files=
   Private.iterator(cs,[],mlx_files);;
 

end;;  


module Target_system_creation=struct

  module Private=struct

    let display_circular_dependencies printer l cycles= 
      if cycles=[]
      then ()
      else
      let temp1=Image.image(fun cycle->
        let ttemp1=Image.image (fun j->printer (List.nth l (j-1))) cycle in
         String.concat " -> " ttemp1 
      ) cycles in
      let temp2="\n\n The following cycles have been detected : "^
        (String.concat "\n\n" temp1) in
      (print_string temp2;flush stdout);;

let select_good_files main_root=
       let s_main_dir = Dfa_root.without_trailing_slash main_root in 
       let ap1=Absolute_path.of_string s_main_dir in        
       let temp1=More_unix.complete_ls (Directory_name.of_string s_main_dir) in
       let s_ap1=Absolute_path.to_string ap1 in
       let n1=String.length(s_ap1) in
       let dname=Coma_constant.name_for_debugged_module in
       let selector=(
       fun ap->
         let s=Absolute_path.to_string ap in
         let t=Cull_string.cobeginning n1 s in
         (List.exists (fun edg->Supstring.ends_with s edg) [".ml";".mli";".mll";".mly"])
         &&
         (List.for_all (fun beg->not(Supstring.begins_with t beg)) 
         (Image.image Dfa_subdirectory.connectable_to_subpath 
          [
            Coma_constant.automatically_generated_subdir;
            Coma_constant.abandoned_ideas_subdir;
            Coma_constant.temporary_subdir;
            Coma_constant.githubbed_archive_subdir;
            Coma_constant.build_subdir;
            Coma_constant.debug_build_subdir;
            Coma_constant.exec_build_subdir; 
          ]
         ))
         &&
         (* When a mll or mly is present, do not register the ml *)
         (not(
               (Supstring.ends_with s ".ml")
               &&
               (List.exists (fun edg->Sys.file_exists(s_ap1^s^edg)) ["l";"y"])
         ))
         &&
         (List.for_all (fun edg->not(Supstring.ends_with s edg) ) 
         [".ocamlinit";"executable.ml"]
         )
         &&
         (t<>(dname^".ml"))
       ) in
       List.filter selector temp1;;
       
     let rec detect_identical_names (identical_names,l)=
       match l with 
       []->identical_names
      |(a,b)::others->
         let (temp1,temp2)=List.partition (fun t->snd(t)=b) others in
         if temp1<>[]
         then detect_identical_names(((a,b)::temp1)::identical_names,temp2)
         else detect_identical_names(identical_names,temp2);;  
         
     
    
    end;;   
    
exception Identical_names of (((string*string) list) list);;    
         
     let clean_list_of_files main_dir l=
      (*
         raises an exception if there are different modules with
         identical names.
         Removes the files outside main_dir.
      *)
      let s_dir=Dfa_root.connectable_to_subpath main_dir in
      let temp1=List.filter (fun ap->
        Supstring.begins_with (Absolute_path.to_string ap) s_dir
      ) l in
      let temp2=Image.image (fun ap->
        let s=Absolute_path.to_string ap in
        (ap,Cull_string.after_rightmost s '/')
      ) temp1 in
      let temp3=Private.detect_identical_names ([],temp2) in
      if temp3<>[]
      then let n1=String.length s_dir in
           let tempf1=(fun (x,y)->
               (Cull_string.cobeginning n1 (Absolute_path.to_string x),y)
            ) in
           let tempf2=Image.image (Image.image tempf1) in
           let temp4=tempf2 temp3 in
           raise(Identical_names(temp4))
      else temp2;;
      
    let compute_dependencies l=
      let temp1=Ennig.index_everything l 
      and n=List.length l in
      let rec tempf=(fun (j1,(ap1,s1))->
        let ttemp1=Look_for_module_names.names_in_ml_file ap1 in
        let ttemp2=Image.image Dfa_module.to_line ttemp1 in
        let ttempf=(fun s_nm->
          Option.filter_and_unpack (fun 
          (k,(_,s))->
          if (Cull_string.before_rightmost s '.')=s_nm
          then Some(k)
          else None ) temp1
        ) in
        let ttemp3=Image.image ttempf ttemp2 in
        List.flatten  ttemp3
      )  in
      let tempg=(fun x-> let (_,(_,s))=x in
         if Supstring.ends_with s ".mli"
         then let t=Cull_string.coending 1 s in
              match Option.seek (fun (_,(_,s1))->s1=t) temp1 with
               None->tempf x
              |Some(y)->tempf y 
         else tempf x
      ) in
      let table_for_coatoms=Image.image tempg temp1 in
      let coat=Memoized.make(fun j->List.nth table_for_coatoms (j-1)) in
      let (cycles,good_list)=
        Reconstruct_linear_poset.reconstruct_linear_poset coat 
        (Ennig.ennig 1 n) in
      let _=Private.display_circular_dependencies
      (fun (j1,(ap1,s1))->s1) temp1 cycles in
      Image.image (fun (j,_)->snd(List.nth temp1 (j-1)) ) good_list;;
      
    let from_prepared_list cs l=
       let dir = Coma_state_field.root cs in 
       let temp1=Option.filter_and_unpack (fun (ap,s)->
          try (Some(Dfn_full.from_absolute_path_with_root ap dir)) with 
          _->None
       ) l in
       Try_to_register.mlx_files cs temp1;;

let delchacre_from_scratch (source_dir,dir_for_backup) cs=
  let temp1=all_mlx_paths cs in
  let temp3=temp1 in
  let temp4=Image.image (fun ap->
     let rootless_path = Dfn_common.decompose_absolute_path_using_root ap source_dir in 
     Dfn_rootless.to_line rootless_path) temp3 in
 Prepare_dircopy_update.compute_diff
    (source_dir,temp4) dir_for_backup;;

    

end;;  

module Register_mlx_file=struct

let on_targets (cs,old_dirs) mlx=
    let new_dir=Dfn_full.to_subdirectory mlx in
   let cs2=register_mlx_file_on_monitored_modules cs mlx in
   let new_dirs=
   (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
    in
    let nm=Dfn_full.to_module mlx in 
    let (cs3,_,_)=Ocaml_target_making.usual_feydeau cs2 [nm] in 
    (cs3,new_dirs);; 
  

end;;  


let register_mlx_file cs mlx=
          let (cs2,new_dirs)= 
          Register_mlx_file.on_targets (cs,directories cs) mlx in   
           set_directories cs2 new_dirs;;            

module Raneme_directory = struct 

let on_subdirectory=Dfa_subdirectory.rename_endsubdirectory;;

let on_printer_equipped_type pair (eless,is_compiled_correctly)=
    (Dfn_endingless.rename_endsubdirectory pair eless,is_compiled_correctly);;


let on_printer_equipped_types (old_subdir,new_subdirname) l=
        Image.image (on_printer_equipped_type (old_subdir,new_subdirname)) l ;; 

 
let on_subdirectories (old_subdir,new_subdirname) l_subdir=
   Image.image (on_subdirectory (old_subdir,new_subdirname)) l_subdir;; 

end;;



let rename_directory cs (old_subdir,new_subdirname)=
      let _=Rename_endsubdirectory.in_unix_world 
       (root cs) (old_subdir,new_subdirname) in
      let pair=(old_subdir,new_subdirname) in
      let cs2=rename_directory_on_data pair cs in
      let new_dirs=Raneme_directory.on_subdirectories pair 
        (directories cs2)
      and new_peqt=Raneme_directory.on_printer_equipped_types pair 
        (preq_types cs2)
      in
      let cs3=set_directories cs2 new_dirs in 
      set_preq_types cs3 new_peqt;;
         


let clean_debug_dir cs=
  let s_root=Dfa_root.connectable_to_subpath(root cs) in
  let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir)) in 
  Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
   

let start_debugging cs=
  let  _=clean_debug_dir cs in
  let dbg_path=Dfn_short.to_line(Coma_constant.short_path_for_debugged_file) in
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Compilation_mode_t.Debug cs dbg_path in 
  let answer=Unix_command.conditional_multiple_uc cmds in 
	let msg=(
	  if answer
	  then "\n\n Now, start \n\nocamldebug _debug_build/"^(Coma_constant.name_for_debugged_module)^".ocaml_debuggable\n\nin another terminal\n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	let _=(
	  print_string msg;
	  flush stdout
	) in
	answer;;   
   
let clean_exec_dir cs=
  let s_root=Dfa_root.connectable_to_subpath(root cs) in
  let s_exec_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.exec_build_subdir)) in 
  Unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
   

let start_executing cs short_path=
  let  _=clean_exec_dir cs in
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
    Compilation_mode_t.Executable cs short_path in 
  Unix_command.conditional_multiple_uc cmds;;   


let unregister_mlx_file cs mlx=
    let (cs2,new_dirs)=unregister_mlx_file_on_targets (root cs) cs  mlx in 
    set_directories cs2 new_dirs;;

let unregister_module_on_targets root_dir cs mn=
    let (cs2,rootless_paths)=unregister_module_on_monitored_modules  cs mn in
    let new_dirs=compute_subdirectories_list cs2  in
     ((cs2,new_dirs),rootless_paths);;   
               

let unregister_module cs endingless=
        let ((cs2,new_dirs),short_paths)= 
         unregister_module_on_targets (root cs) cs  endingless in 
          set_directories cs2 new_dirs;;        

let decipher_path cs x=Find_suitable_ending.find_file_location 
   (root cs) (directories cs) x;;

let forgotten_files_in_build_subdir cs= 
   let s_root=Dfa_root.connectable_to_subpath (root cs) 
   and s_build=Dfa_subdirectory.connectable_to_subpath (Coma_constant.build_subdir) in 
   let dir=Directory_name.of_string (s_root^s_build) in 
   let temp1=More_unix.beheaded_simple_ls dir in 
   List.filter (
      fun s->
       let s_mn=Cull_string.before_rightmost_possibly_all s '.' in 
       let mn=Dfa_module.of_line s_mn in 
       not(Coma_state_field.test_module_for_registration cs mn)
       ) temp1;;

exception Absent_module of string;;

let decipher_module cs capitalized_or_not_x=
  let x=String.uncapitalize_ascii capitalized_or_not_x in 
  let s=Cull_string.before_rightmost_possibly_all x '.' in
  match (Option.find_and_stop(
      fun edg->
      let t=s^(Dfa_ending.connectable_to_modulename edg) in 
      try(Some(decipher_path cs t)) with _->None
  ) Dfa_ending.all_ocaml_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->
    let rootless_path = Dfn_common.decompose_absolute_path_using_root ap (root cs) in 
    let mlx = Dfn_join.root_to_rootless (root cs) rootless_path in 
    Dfn_full.to_endingless mlx ;;

module Local_rename_value_inside_module = struct

exception No_module_given of string;;

exception No_value_with_name of string;;

let get_module_inside_name s=
   let f=Cull_string.before_rightmost s '/' in
   if f=""
   then raise(No_module_given(s))
   else f;;
   
let rename_value_inside_module cs old_name new_name=
   let j=Substring.leftmost_index_of_in "." old_name in
   if j<0 
   then raise(No_module_given(old_name))
   else 
   let module_name=Cull_string.beginning (j-1) old_name in
   let endingless=decipher_module cs  module_name 
   and path=decipher_path cs  module_name in 
   let nm=Dfn_endingless.to_module endingless in
   let pre_temp2=(ancestors_at_module cs nm)@[nm] in
   let temp2=Image.image (endingless_at_module cs) pre_temp2 in
   let preceding_files=Image.image  (fun eless2->
   	 Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
   ) temp2 in
   Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name path;;



end;;


module Values_in_modules = struct

let replace_string cs old_string new_string=
  let temp1=files_containing_string cs old_string in
  let m=String.length(Dfa_root.connectable_to_subpath (root cs)) in
  let temp2=Image.image (fun ap->
    Cull_string.cobeginning m (Absolute_path.to_string ap)) temp1 in
  let temp3=temp2@["";""] in 
  let message="\n\n The following files will be rewritten : \n\n"^
  (String.concat "\n" temp3) in
  let _=(print_string message;flush stdout) in
  List.iter (Replace_inside.replace_inside_file (old_string,new_string)) temp1;;

(*
if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.
*)


let rename_string_or_value cs old_name new_name=
  if not(String.contains old_name '.')
  then replace_string cs old_name new_name
  else 
    let new_full_name=(Cull_string.before_rightmost old_name '.')^"."^new_name in
    (Local_rename_value_inside_module.rename_value_inside_module 
            cs old_name (Overwriter.of_string new_name); 
     replace_string cs old_name new_full_name
    );;


let list_values_from_module_in_file module_name file=
   let s=Io.read_whole_file file in
   let temp1=Look_for_module_names.indices_in_ml_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=Alternative_str_example.index_for_pointed_case)&&
     (Cull_string.interval s i j=(String.capitalize_ascii module_name))
   ) temp1 in
   let temp3=Image.image(fun (t,(i,j))->
    let opt=After.after_star 
     Charset.ocaml_modulename_nonfirst_letters
     s (j+2) in
    let end_idx=(match opt with Some(k)->k-1 |None->String.length s) in
     Cull_string.interval s (j+2) end_idx
   ) temp2 in
   Set_of_strings.sort temp3;;

let list_values_from_module_in_modulesystem cs module_name=
   let temp1=all_mlx_paths cs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Set_of_strings.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=Image.image fst temp3 in 
   let temp5=Ordered.sort Total_ordering.lex_for_strings temp4 in
   Image.image (
      fun x->(x,Option.filter_and_unpack(
        fun (y,ap)->if y=x then Some(ap) else None
      ) temp3)
   ) temp5 ;;
 
let list_value_occurrences_in_file t file=
   let s=Io.read_whole_file file in
   let temp1=Substring.occurrences_of_in t s in
   Image.image (fun j->Cull_string.closeup_around_index 
      s j
   ) temp1;; 
 

let show_value_occurrences_in_modulesystem cs t=
   let m=String.length(Dfa_root.connectable_to_subpath (root cs)) in
   let temp1=all_mlx_paths cs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_value_occurrences_in_file t ap in
    let mname=Cull_string.cobeginning(m)(Absolute_path.to_string ap) in
    Image.image (fun x->mname^":\n"^x ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=String.concat "\n\n\n" (""::temp3@[""]) in 
   print_string temp4;;

end;;

exception Module_already_exists of string;;

let duplicate_module cs old_t1 old_t2=
   let t1=String.uncapitalize_ascii old_t1
   and t2=String.uncapitalize_ascii old_t2 in 
   let ap1=decipher_path cs t1 in
   let s_ap1=Absolute_path.to_string ap1 in
   let s_ap2=(Cull_string.before_rightmost_possibly_all s_ap1 '/')^"/"^t2^".ml" in
   if Sys.file_exists s_ap2
   then raise(Module_already_exists(t2))
   else 
   let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Absolute_path.of_string s_ap2 in
   let s_cdir=Dfa_root.connectable_to_subpath (root cs) in 
   let s1=Cull_string.cobeginning (String.length s_cdir) s_ap1
   and s2=Cull_string.cobeginning (String.length s_cdir) s_ap2 in
   let txt1="\""^s1^"\";"^";"
   and txt2="\""^s2^"\";"^";" in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) ap2  in 
   Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;             


module Almost_concrete = struct 


let local_above cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in  
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle (endingless_at_module cs nm) in 
    Dfn_middle.to_line middle )
  (above cs endingless);;


let local_below cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in  
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle (endingless_at_module cs nm) in 
    Dfn_middle.to_line middle )
  (below cs endingless);;

let local_directly_above cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in   
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle (endingless_at_module cs nm) in 
    Dfn_middle.to_line middle )
  (directly_above cs endingless);;

let local_directly_below cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in   
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle (endingless_at_module cs nm) in 
    Dfn_middle.to_line middle )
  (directly_below cs endingless);;

let local_rename_directory cs old_subdir new_subdirname=
   let old_rootless_paths=short_paths_inside_subdirectory cs old_subdir in
   let cs2=rename_directory cs (old_subdir,new_subdirname) in 
   let s_old_subdir=Dfa_subdirectory.without_trailing_slash old_subdir in
   let subdir_father = Cull_string.before_rightmost s_old_subdir '/' in 
   let new_subdir=(if subdir_father="" 
                   then new_subdirname 
                   else subdir_father^"/"^new_subdirname) in 
   let new_rootless_paths=short_paths_inside_subdirectory cs2 (Dfa_subdirectory.of_line new_subdir) in
   let diff=Dircopy_diff.veil
    (Recently_deleted.of_string_list old_rootless_paths)
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list new_rootless_paths) in
   (cs2,diff);;   


end;; 




module Recent_changes = struct
           
    exception Recompilation_needed of Dfa_module_t.t list;;       

            let check_for_change_at_module_and_ending cs mn edg=
               let hm=endingless_at_module cs mn in 
               (md_recompute_modification_time hm edg)
               <>(get_modification_time cs mn edg);;

            let check_for_change_at_module  cs mn=
            List.exists
               (check_for_change_at_module_and_ending cs mn) 
            [
               Dfa_ending.mli ;
               (principal_ending_at_module cs mn)
            ] ;;

            let detect_changes cs =
            Option.filter_and_unpack (
               fun mn->
               if check_for_change_at_module cs mn 
               then Some(mn)
               else None
            ) (ordered_list_of_modules cs);;

            let check_for_changes cs = 
            let changes = detect_changes cs in 
            if changes<>[]
            then raise(Recompilation_needed(changes))
            else ();;

end;;    

module Late_Recompilation = struct 

let quick_update cs (new_fw,changed_rootlesses)  mn=
  let eless =endingless_at_module cs mn 
  and pr_ending=principal_ending_at_module cs mn in
  let middle = Dfn_endingless.to_middle eless in 
  if (Dfn_middle.to_line middle)=Coma_constant.name_for_debugged_module
  then None
  else
  let mli_modif_time=Fw_wrapper_field.get_mtime_or_zero_if_file_is_nonregistered new_fw (Dfn_join.middle_to_ending middle Dfa_ending.mli) 
  and pr_modif_time=Fw_wrapper_field.get_mtime new_fw (Dfn_join.middle_to_ending middle pr_ending)  
  and old_mli_modif_time=mli_mt_at_module cs mn
  and old_pr_modif_time=principal_mt_at_module cs mn 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  let mn = Dfn_endingless.to_module eless in 
  if (old_values=new_values)&&(product_up_to_date_at_module cs mn)&&
     (List.for_all (fun rl->(Dfn_rootless.to_middle rl)<>middle ) changed_rootlesses)  
  then None
  else
  let mlx=Dfn_join.to_ending eless pr_ending in
  let direct_fathers=find_needed_data cs mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;


end ;;


