(* 
#use"Compilation_management/coma_state.ml";;
*)

(* Inherited values *)

let root =Coma_state_field.root;;
let backup_dir =Coma_state_field.backup_dir;;
let push_after_backup =Coma_state_field.push_after_backup;;
let module_at_idx = Coma_state_field.module_at_idx ;;
let subdir_at_idx = Coma_state_field.subdir_at_idx ;;
let principal_ending_at_idx = Coma_state_field.principal_ending_at_idx ;;
let mli_presence_at_idx = Coma_state_field.mli_presence_at_idx ;;
let principal_mt_at_idx = Coma_state_field.principal_mt_at_idx ;;
let mli_mt_at_idx = Coma_state_field.mli_mt_at_idx ;;
let needed_libs_at_idx  = Coma_state_field.needed_libs_at_idx ;;
let direct_fathers_at_idx = Coma_state_field.direct_fathers_at_idx ;;
let ancestors_at_idx = Coma_state_field.ancestors_at_idx ;; 

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

let set_module_at_idx = Coma_state_field.set_module_at_idx ;;


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

let modules = Coma_state_field.modules;;


let all_used_subdirs = Coma_state_field.all_used_subdirs;;




(* End of inherited values *)

let find_module_index cs nm=
  Small_array.leftmost_index_of_in
   nm (modules cs);;   

let seek_module_index cs nm=
  try
  Some(find_module_index cs nm)
  with
  _->None;;   

let hm_at_idx cs k=
    Dfn_endingless_t.J(
        root cs,
        subdir_at_idx cs k,
        module_at_idx cs k
    );;

  
let hm_from_nm cs nm=
   let idx=find_module_index cs nm in
   hm_at_idx cs idx;;

let check_ending_in_at_idx edg cs idx=
   if edg=principal_ending_at_idx cs idx
   then true 
   else 
   if edg=Dfa_ending_t.E("mli")
   then mli_presence_at_idx cs idx
   else false;;

let acolytes_at_idx cs idx=
  let name=hm_at_idx cs idx in
  Option.filter_and_unpack (fun 
  edg->
     if check_ending_in_at_idx edg cs idx
     then Some(Dfn_join.to_ending name edg)
     else None
) Dfa_ending.all_ocaml_endings;;

let short_paths_at_idx cs idx=
   Image.image Dfn_full.to_rootless_line (acolytes_at_idx cs idx);;
  

let registered_endings_at_idx cs idx=
  List.filter (fun edg->
  check_ending_in_at_idx edg cs idx 
  ) Dfa_ending.all_ocaml_endings;;


let check_for_single_ending_at_idx cs idx=
  if mli_presence_at_idx cs idx
  then (principal_ending_at_idx cs idx)=(Dfa_ending_t.E "mli")
  else true ;;


let size cs = Small_array.size (modules cs);;      

let list_of_modules cs = Small_array.to_list (modules cs);; 

let up_to_date_hms cs =
   Option.filter_and_unpack (
     fun mn->
       if product_up_to_date_at_module cs mn
       then Some(hm_from_nm cs mn)
       else None
   )(list_of_modules cs);;

let modules_with_their_ancestors cs l=
   let unordered_temp1=Option.filter_and_unpack (
     fun nm->
       match seek_module_index cs nm with 
       None->None
       |Some(idx)->Some(idx,nm)
   ) l in 
   let temp1=Ordered.forget_order(Tidel2.diforchan unordered_temp1) in 
   let temp2=Image.image (
     fun (idx,nm)->
       (ancestors_at_idx cs idx)@[nm] 
   ) temp1 in 
   let temp3=List.flatten temp2 in 
   Listennou.nonredundant_version temp3;;



let find_needed_data_for_file cs fn=
      let temp1=Look_for_module_names.names_in_ml_file fn in
      Small_array.indices_of_property_in 
      (fun nm->List.mem nm temp1)
      (modules cs);; 

let find_needed_data cs mlx=
      let fn=Dfn_full.to_absolute_path mlx in
      find_needed_data_for_file cs fn;;         

let needed_dirs_and_libs_in_command cmod cs idx=
   let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
   let s_root=Dfa_root.connectable_to_subpath(root cs) in
   let dirs=
   "-I "^s_root^(Dfa_subdirectory.connectable_to_subpath(Compilation_mode.workspace cmod))
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    (needed_libs_at_idx cs idx)) in
    String.concat " " ["";dirs;libs;""];;



let needed_dirs_and_libs_for_several cmod cs l_idx=
   let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image 
     (fun idx->
      let mn = module_at_idx cs idx in 
      Tidel.diforchan(needed_dirs_at_module cs mn)) l_idx in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Dfa_subdirectory.connectable_to_subpath(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image 
     (fun idx->Tidel.diforchan(needed_libs_at_idx cs idx)) l_idx in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let all_modules cs=
  let n=Small_array.size((modules cs)) in
  Ennig.doyle (hm_at_idx cs) 1 n;; 

let get_modification_time cs idx edg=
  if edg=principal_ending_at_idx cs idx then principal_mt_at_idx cs idx else 
  if edg=(Dfa_ending_t.E "mli") then mli_mt_at_idx cs idx else 
  "0.";;

exception Non_existent_mtime of Dfn_full_t.t;;

let force_modification_time root_dir cs mlx=
      let edg=Dfn_full.to_ending mlx in
      let nm=Dfn_full.to_module mlx in
      let idx=
        (try Small_array.leftmost_index_of_in
          nm (modules cs) with 
        _->raise(Non_existent_mtime(mlx)) ) in
      let file=Dfn_full.to_line mlx in 
      let new_val=string_of_float((Unix.stat file).Unix.st_mtime)  in
      let cs2=(
        if edg=principal_ending_at_idx cs idx 
        then set_principal_mt_at_module cs nm new_val
        else cs
      ) in
      let cs3=(
        if edg=(Dfa_ending.mli)
        then set_mli_mt_at_module cs2 nm new_val
        else cs2
      ) in     
      cs3;;

let everyone_except_the_debugger cs=
   (* used to be a more complicated function, when the debug module
     was registered.
    *)  
        let n=Small_array.size (modules cs) in
        Image.image (hm_at_idx cs) (Ennig.ennig 1 n);;      
        


exception Non_registered_module of Dfn_endingless_t.t;;  
exception Derelict_children of Dfa_module_t.t*(Dfa_module_t.t list);;  
           
            
let unregister_module_on_monitored_modules cs hm=
  let nm=Dfn_endingless.to_module hm in
  let n=Small_array.size (modules cs) in
  let pre_desc=List.filter(
      fun idx->List.mem nm ( ancestors_at_idx cs idx )
  ) (Ennig.ennig 1 n) in
   if pre_desc<>[]
   then let temp1=Image.image ( module_at_idx cs ) pre_desc in
        raise(Derelict_children(nm,temp1))
   else
   let idx=
    (try Small_array.leftmost_index_of_in
      nm (modules cs) with 
    _->raise(Non_registered_module(hm)) ) in
    let acolytes=acolytes_at_idx cs idx  in
   let cs2=Coma_state_field.remove_in_each_at_index cs idx in
   let old_preqtypes = Coma_state_field.preq_types cs2 in 
   let new_preqtypes = List.filter (fun (hm2,_)->hm2<>hm ) old_preqtypes in 
   let cs3=(
     if new_preqtypes <> old_preqtypes 
     then Coma_state_field.set_preq_types cs2 new_preqtypes
     else cs2
   ) in 
   let short_paths=Image.image Dfn_full.to_rootless_line acolytes in
   (cs3,short_paths);;     
                    

exception Non_registered_file of Dfn_full_t.t;;  
exception Abandoned_children of Dfn_full_t.t * (Dfa_module_t.t list);;
                      
                     
let unregister_mlx_file_on_monitored_modules cs mlxfile=
    let hm=Dfn_full.to_endingless mlxfile
    and nm=Dfn_full.to_module mlxfile in
    let n=Small_array.size (modules cs) in
    let pre_desc=List.filter(
      fun idx->List.mem nm ( ancestors_at_idx cs idx )
    ) (Ennig.ennig 1 n) in
    if pre_desc<>[]
    then let temp1=Image.image ( module_at_idx cs ) pre_desc in
        raise(Abandoned_children(mlxfile,temp1))
    else
    let idx=
      (try Small_array.leftmost_index_of_in
        nm (modules cs) with 
      _->raise(Non_registered_file(mlxfile)) ) in
    let edg=Dfn_full.to_ending mlxfile in
    if (not(check_ending_in_at_idx edg cs idx))
    then raise(Non_registered_file(mlxfile))
    else if check_for_single_ending_at_idx cs idx
         then let cs5=Coma_state_field.remove_in_each_at_index cs idx in 
              let old_preqtypes = Coma_state_field.preq_types cs5 in 
              let new_preqtypes = List.filter (fun (hm2,_)->hm2<>hm ) old_preqtypes in 
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
    let temp2=Ordered_string.diforchan temp1 in
    let temp3=Ordered_string.forget_order temp2 in
    Image.image Dfa_subdirectory.of_line temp3;;

let  check_registrations cs hm=
   let nm=Dfn_endingless.to_module hm in 
    match seek_module_index cs nm with
      None->(false,false,false,false)
    |Some(idx)->
       Dfa_ending.compute_on_all_ocaml_endings 
      (fun edg->check_ending_in_at_idx edg cs idx);;


module PrivateTwo=struct

let find_needed_names cs mlx=
  let temp1=find_needed_data cs mlx in
  Image.image (Small_array.get (modules cs) ) temp1;;  

let find_needed_libraries cs mlx genealogy=
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
           (fun k->List.mem lib (needed_libs_at_idx cs k) ) 
           genealogy
  )
  Ocaml_library.all_libraries;;


let find_needed_directories cs mlx genealogy=
  let temp1=Image.image (fun idx->
    let mn = module_at_idx cs idx in 
    Tidel.diforchan(needed_dirs_at_module cs mn)) genealogy in
  let subdir_in_mlx=Dfn_full.to_subdirectory mlx in
  let temp2=(
      if subdir_in_mlx<>Dfa_subdirectory.main 
      then Tidel.singleton(subdir_in_mlx)::temp1
      else temp1
  ) in    
  let temp3=Tidel.big_teuzin temp2 in
  Ordered.forget_order temp3;;
              
                    
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
  let n=Small_array.size((modules cs)) in
  let hm=Dfn_full.to_endingless mlx  in
  let genealogy=find_needed_data cs mlx in
  let (mlr,mlir,mllr,mlyr)=check_registrations cs hm
  and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
  let pr_end=compute_principal_ending (mlr,mlir,mllr,mlyr) in
  let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let dirfath=Image.image (Small_array.get (modules cs)) genealogy in
  let temp1=Image.image 
        (fun t->Tidel.diforchan(ancestors_at_idx cs t)) 
        genealogy in
  let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
  let tempf=(fun t->
            let nam_t=Small_array.get (modules cs) t in
            if Tidel.elfenn nam_t temp2
            then Some(nam_t)
            else None) in
  let allanc=Option.filter_and_unpack tempf (Ennig.ennig 1 n) in
  let libned=PrivateTwo.find_needed_libraries cs mlx genealogy
  and dirned=PrivateTwo.find_needed_directories cs mlx genealogy in
  (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,false);;

  let check_unix_presence e_less edg=
    let full_path=Dfn_join.to_ending e_less edg in 
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
    let n=Small_array.size((modules cs)) in
    let hm=Dfn_full.to_endingless mlx 
    and edg=Dfn_full.to_ending mlx in
    let genealogy=find_needed_data cs mlx in
    let (mlp,mlir,mllr,mlyr)=registrations_for_lonely_ending edg
    and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
    let pr_end=edg in
    let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let dirfath=Image.image (Small_array.get (modules cs)) genealogy in
    let temp1=Image.image 
          (fun t->Tidel.diforchan(ancestors_at_idx cs t)) 
          genealogy in
    let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
    let tempf=(fun t->
              let nam_t=Small_array.get (modules cs) t in
              if Tidel.elfenn nam_t temp2
              then Some(nam_t)
              else None) in
    let allanc=Option.filter_and_unpack tempf (Ennig.ennig 1 n) in
    let libned=PrivateTwo.find_needed_libraries cs mlx genealogy
    and dirned=PrivateTwo.find_needed_directories cs mlx genealogy in
    (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,false);;
  
let do_file_renaming mlx new_name=
  let core=Cull_string.before_rightmost_possibly_all (No_slashes.to_string new_name) '.' in
  let checked_name=No_slashes.of_string(core^
  (Dfa_ending.connectable_to_modulename(Dfn_full.to_ending mlx))) in
  let ap=Dfn_full.to_absolute_path mlx in
  let new_ap=Rename_file.rename ap checked_name in
  let (Dfn_full_t.J(r,_,_,_))=mlx in 
  Dfn_full.from_absolute_path_with_root new_ap r;;    
  

exception Nonregistered_module of Dfa_module_t.t;;

exception Future_name_already_taken of Dfa_module_t.t;;

let rename_module_on_monitored_modules cs old_name new_name=
  let root_dir=root cs in 
  let n=Small_array.size (modules cs) in
  let old_nm=Dfn_endingless.to_module old_name in
  let opt_idx=seek_module_index cs old_nm in
  if opt_idx=None
  then raise(Nonregistered_module(old_nm))
  else 
  let future_new_nm=Dfa_module.of_line (No_slashes.to_string new_name) in 
  if (seek_module_index cs future_new_nm) <>None 
  then raise(Future_name_already_taken(future_new_nm))
  else 
  let idx=Option.unpack opt_idx in
  let old_acolytes=acolytes_at_idx cs idx in
  let old_files=Image.image (fun mlx->Dfn_full.to_rootless_line mlx) 
       old_acolytes in 
  let new_acolytes=Image.image 
     (fun mlx->do_file_renaming mlx new_name) 
     old_acolytes in
  let new_files=Image.image (fun mlx->Dfn_full.to_rootless_line mlx) 
     new_acolytes in 
  let new_hm=Dfn_full.to_endingless(List.hd new_acolytes) in
  let old_mname=Dfn_endingless.to_module old_name
  and new_mname=Dfn_full.to_module (List.hd new_acolytes)
  in
  let changer=Look_for_module_names.change_module_name_in_ml_file old_mname new_mname in
  let separated_acolytes=Option.filter_and_unpack(
    fun k->
     if List.mem old_mname (ancestors_at_idx cs k)
    then Some(acolytes_at_idx cs k)
    else None
) (Ennig.ennig 1 n) in
  let all_acolytes=List.flatten separated_acolytes in
  let temp3=Image.image Dfn_full.to_absolute_path all_acolytes in
  let temp4=Option.filter_and_unpack (
    fun s->
    let full_path = Dfn_join.root_to (root cs) s in 
    try Some(Dfn_full.to_absolute_path full_path) with _->None
  ) [
      Coma_constant.rootless_path_for_printersfile;
    ] in
  let modified_files=Image.image Dfn_full.to_rootless_line all_acolytes in  
  let _=Image.image changer (temp3@temp4) in
  let s_root=Dfa_root.connectable_to_subpath root_dir in   
  let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.build_subdir) in   
  let _=Unix_command.uc
      ("rm -f "^s_root^s_build_dir^
      (Dfa_module.to_line old_mname)^
      ".cm* ") in
  let principal_mt=md_compute_modification_time new_hm (principal_ending_at_idx cs idx)
  and mli_mt=md_compute_modification_time new_hm Dfa_ending.mli in
  let cs2=set_module_at_idx cs idx new_mname in 
  let cs3=set_principal_mt_at_module cs2 new_mname principal_mt in 
  let cs4=set_mli_mt_at_module cs3 new_mname mli_mt in 
  let cs5=set_product_up_to_date_at_module cs4 new_mname false in 
  let replacer=Image.image(function x->if x=old_mname then new_mname else x) in
  let hm_replacer=(fun x->if x=old_name then new_hm else x) in 
  let old_preq_types=preq_types cs5 in 
  let new_preq_types=Image.image (fun (h,bowl)->(hm_replacer h,bowl)) old_preq_types in 
  let cs6=set_preq_types cs5 new_preq_types in 
  let cs_walker=ref(cs6) in 
  let _=(
     for k=idx+1 to n do
      let mn = module_at_idx (!cs_walker) k in  
      let old_dirfath=direct_fathers_at_module (!cs_walker) mn
      and old_ancestors=ancestors_at_module (!cs_walker) mn in
      cs_walker:=(set_direct_fathers_at_module (!cs_walker) mn (replacer old_dirfath)) ;
      cs_walker:=(set_ancestors_at_module (!cs_walker) mn (replacer old_ancestors)); 
     done;
  ) in
  (!cs_walker,(old_files,new_files),modified_files);;


let recompute_complete_card_at_idx cs hm=
      let nm=Dfn_endingless.to_module hm in
      let idx=find_module_index cs nm in
      let edg=List.hd(registered_endings_at_idx cs idx) in
      let mlx=Dfn_join.to_ending hm edg in
      complete_info cs mlx;;

let recompute_module_info cs hm=
  let nm=Dfn_endingless.to_module hm in
  let idx=find_module_index cs nm in
  let new_dt=recompute_complete_card_at_idx cs hm in 
  Coma_state_field.set_in_each cs idx new_dt;;  

exception Nonregistered_module_during_relocation of Dfn_endingless_t.t;;  
exception Error_during_unix_physical_relocation;;          

let relocate_module cs old_name new_subdir=
  let root_dir = root cs in 
  let old_nm=Dfn_endingless.to_module old_name in
  let opt_idx=seek_module_index cs old_nm in
  if opt_idx=None
  then raise(Nonregistered_module_during_relocation(old_name))
  else 
  let idx=Option.unpack opt_idx in 
  let old_acolytes=acolytes_at_idx cs idx in
  let old_files=Image.image Dfn_full.to_rootless_line old_acolytes in 
  let new_acolytes=Image.image 
    (fun mlx->Dfn_full.relocate mlx new_subdir) old_acolytes in
  let old_and_new = List.combine old_acolytes new_acolytes in 
  let removals = Image.image (fun (old_mlx,new_mlx)->
    let s_old =  Dfn_full.to_line old_mlx 
    and s_new =  Dfn_full.to_line new_mlx in 
    "mv "^s_old^" "^s_new  
  ) old_and_new in    
  let removals_done = Unix_command.conditional_multiple_uc removals in 
  if (not(removals_done)) then raise(Error_during_unix_physical_relocation) else 
  let new_files=Image.image 
     (fun mlx->Dfn_full.to_rootless_line mlx) new_acolytes in 
  let new_name=Dfn_full.to_endingless
   (List.hd new_acolytes) in
  let s_root=Dfa_root.connectable_to_subpath root_dir in     
  let old_middle = Dfn_endingless.to_middle_element old_name in
    let _=Unix_command.uc
     ("rm -f "^s_root^"_build/"^(Dfn_endingless.middle_element_to_line old_middle)^".cm* ") in
  let principal_mt=md_compute_modification_time new_name (principal_ending_at_idx cs idx)
  and mli_mt=md_compute_modification_time new_name Dfa_ending.mli in
  let cs2=set_subdir_at_module cs old_nm new_subdir in 
  let cs3=set_principal_mt_at_module cs2 old_nm principal_mt in 
  let cs4=set_mli_mt_at_module cs3 old_nm mli_mt in 
  (* let cs5=set_product_up_to_date_at_idx cs4 idx false in *)
  (cs4,(old_files,new_files));;



let above cs hm=
  let nm=Dfn_endingless.to_module hm in
  match seek_module_index cs nm with
  None->raise(Non_registered_module(hm))
  |Some(idx)->ancestors_at_idx cs idx;;

let below cs hm=
  let nm=Dfn_endingless.to_module hm 
  and n=Small_array.size (modules cs) in
  Option.filter_and_unpack(fun idx->
      if List.mem nm (ancestors_at_idx cs idx)
      then Some(Small_array.get (modules cs) idx)
      else None) (Ennig.ennig 1 n);;  

let directly_below cs hm=
        let nm=Dfn_endingless.to_module hm 
        and n=Small_array.size (modules cs) in
        Option.filter_and_unpack(fun idx->
            if List.mem nm (direct_fathers_at_idx cs idx)
            then Some(Small_array.get (modules cs) idx)
            else None) (Ennig.ennig 1 n);;        

let ordered_as_in_coma_state cs l=
   let temp1=Small_array.to_list (modules cs) in
   List.filter (fun x->List.mem x l) temp1;;

let above_one_in_several_or_inside cs l=
  let temp1=Image.image (
      fun nm->let idx=find_module_index cs nm in
      ancestors_at_idx cs idx
  ) l in
  let temp2=List.flatten (l::temp1) in
  ordered_as_in_coma_state cs  temp2;;


let all_mlx_files cs=
  let n=Small_array.size (modules cs) in
  List.flatten(Ennig.doyle(acolytes_at_idx cs) 1 n);;                
      
let all_mlx_paths cs=Image.image Dfn_full.to_absolute_path 
        (all_mlx_files cs);;  

let all_rootless_paths cs=
    let n=Small_array.size (modules cs) in
    List.flatten(Ennig.doyle(short_paths_at_idx cs) 1 n);;  
     

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


let system_size cs=Small_array.size((modules cs));;

exception Inconsistent_constraints of Dfa_module_t.t*Dfa_module_t.t;;
exception Bad_upper_constraint of Dfa_module_t.t;;  


exception Nonregistered_module_during_reposition of Dfn_endingless_t.t;;  

 
let reposition_module cs hm (l_before,l_after)=
    let n=Small_array.size((modules cs)) in 
    let find_idx=find_module_index cs in
    let main_idx=find_idx hm
    and indices_before=Image.image find_idx l_before
    and indices_after=Image.image find_idx l_after in
    let max_before=(if indices_before=[] then 1 else Max.list indices_before)
    and min_after=(if indices_after=[] then n else Min.list indices_after)
    in
    if max_before>min_after
    then let hm_before=Small_array.get (modules cs) max_before
         and hm_after=Small_array.get (modules cs) min_after in
         raise(Inconsistent_constraints(hm_before,hm_after))
    else 
    if max_before>main_idx
    then let hm_before=Small_array.get (modules cs) max_before in
         raise(Bad_upper_constraint(hm_before))
    else 
    Coma_state_field.reposition_in_each cs max_before main_idx;;  

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
  let opt=seek_module_index cs nm in
  if opt=None
  then None 
  else
  let idx1=Option.unpack opt in
  let hm1=hm_at_idx cs idx1 in
  let ap1=Dfn_full.to_absolute_path(Dfn_join.to_ending hm1 
     Dfa_ending.ml) in
  let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
  Option.seek (
     fun itm->Ocaml_gsyntax_item.name(itm)=s
  ) temp1;;


let all_naked_modules cs=
  Small_array.image Dfa_module.to_line (modules cs);;     

let all_ml_absolute_paths cs=
  let n=Small_array.size (modules cs) in   
Option.filter_and_unpack (fun idx->
  if not(check_ending_in_at_idx Dfa_ending.ml cs idx)
  then None
  else 
  let hm=hm_at_idx cs idx in
  let mlx=Dfn_join.to_ending hm Dfa_ending.ml in
  Some(Dfn_full.to_absolute_path mlx)
) (Ennig.ennig 1 n);;

let modules_using_value cs value_name =
  let n=Small_array.size (modules cs) in 
  Option.filter_and_unpack (fun idx->
  let hm=hm_at_idx cs idx
  and pr_end=principal_ending_at_idx cs idx in
  let mlx=Dfn_join.to_ending hm pr_end in
   let ap=Dfn_full.to_absolute_path mlx in
   if Substring.is_a_substring_of 
       value_name (Io.read_whole_file ap)
   then Some hm
   else None ) (Ennig.ennig 1 n);;




let update_ancs_libs_and_dirs_at_idx cs idx=
  let hm=hm_at_idx cs idx  
  and pr_end=principal_ending_at_idx cs idx in
  let mn = Dfn_endingless.to_module hm in 
  let mlx=Dfn_join.to_ending hm pr_end in 
  let fathers=direct_fathers_at_idx cs idx in
  let separated_ancestors=Image.image 
  (fun nm2->
    let idx2=Small_array.leftmost_index_of_in nm2 (modules cs) in
    Tidel.safe_set(ancestors_at_idx cs idx2)
  ) fathers in
  let all_ancestors=Tidel.big_teuzin((Tidel.safe_set fathers)::separated_ancestors) in
  let unordered_ancestor_indices=Tidel.image (
    fun nm3->Small_array.leftmost_index_of_in nm3 (modules cs)
  ) all_ancestors in
  let genealogy=Ordered.forget_order(Tidel.diforchan unordered_ancestor_indices) in
  let new_libs=PrivateTwo.find_needed_libraries cs mlx genealogy
  and new_dirs=PrivateTwo.find_needed_directories cs mlx genealogy 
  and ordered_ancestors=Image.image (
    Small_array.get (modules cs)
  ) genealogy in
  let cs2=set_ancestors_at_module cs mn ordered_ancestors in 
  let cs3=set_needed_libs_at_module cs2 mn new_libs in
  set_needed_dirs_at_module cs3 mn new_dirs;;

let update_ancs_libs_and_dirs cs=
  let n=Small_array.size (modules cs) in
  let cs_walker=ref(cs) in 
  let _=(for idx=1 to n do
    cs_walker:=update_ancs_libs_and_dirs_at_idx (!cs_walker) idx
  done) in
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
      let md_list=Small_array.to_list (modules cs) in
      let coat=Memoized.make (fun nm->
        let idx=Small_array.leftmost_index_of_in nm (modules cs) in
        direct_fathers_at_idx cs idx
      ) in
      let (cycles,reordered_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles (
        (fun nm->
           let idx=Small_array.leftmost_index_of_in nm (modules cs) in 
           let middle = Dfn_endingless.to_middle_element ( hm_at_idx cs idx) in 
           Dfn_endingless.middle_element_to_line middle )
      ) cycles in     
      let cs2=Coma_state_field.reorder cs (Image.image fst reordered_list) in    
      let cs3=update_ancs_libs_and_dirs cs2 in 
      let n=Small_array.size (modules cs3) in
      let active_descendants=Option.filter_and_unpack (
          fun idx->
            let nm=Small_array.get (modules cs) idx in
            if List.mem nm initially_active_nms
            then Some(nm)
            else
            if List.exists (fun nm2->List.mem nm2 initially_active_nms) 
                 (ancestors_at_idx cs idx)
            then Some(nm)
            else None
      ) (Ennig.ennig 1 n) in  
      (cs3,active_descendants);;
     
end;; 
     
let md_recompute_modification_time hm edg=
  let mlx=Dfn_join.to_ending hm edg in
  let file=Dfn_full.to_line mlx in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;
  

let quick_update cs idx=
  let hm =hm_at_idx cs idx 
  and pr_ending=principal_ending_at_idx cs idx in
  let middle = Dfn_endingless.to_middle_element hm in 
  if (Dfn_endingless.middle_element_to_line middle)=Coma_constant.name_for_debugged_module
  then None
  else
  let mli_modif_time=md_recompute_modification_time hm Dfa_ending.mli 
  and pr_modif_time=md_recompute_modification_time hm pr_ending 
  and old_mli_modif_time=mli_mt_at_idx cs idx
  and old_pr_modif_time=principal_mt_at_idx cs idx 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  let mn = Dfn_endingless.to_module hm in 
  if (old_values=new_values)&&(product_up_to_date_at_module cs mn)
  then None
  else
  let mlx=Dfn_join.to_ending hm pr_ending in
  let direct_fathers=PrivateTwo.find_needed_names cs mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;
    
let latest_changes cs = 
  let n=Small_array.size (modules cs) in
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun idx->
    let nm=Small_array.get (modules cs) idx in
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (short_paths_at_idx cs idx))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun idx->
    match quick_update (!cs_walker) idx with
    None->()
    |Some(pr_modif_time,mli_modif_time,direct_fathers)->
    let mname = module_at_idx (!cs_walker) idx in 
    (
    declare_changed(idx);
    cs_walker:=set_principal_mt_at_module (!cs_walker) mname pr_modif_time;
    cs_walker:=set_mli_mt_at_module (!cs_walker) mname mli_modif_time;
    cs_walker:=set_direct_fathers_at_module (!cs_walker) mname direct_fathers;
    cs_walker:=set_product_up_to_date_at_module (!cs_walker) mname false;
    )
)(Ennig.ennig 1 n) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then [] else
let _=PrivateThree.announce_changed_modules changed_modules in
(!ref_for_changed_shortpaths);; 


let recompile_on_monitored_modules tolerate_cycles cs = 
  let n=Small_array.size (modules cs) in
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun idx->
    let nm=Small_array.get (modules cs) idx in
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (short_paths_at_idx cs idx))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun idx->
    match quick_update (!cs_walker) idx with
    None->()
    |Some(pr_modif_time,mli_modif_time,direct_fathers)->
    let mname = module_at_idx (!cs_walker) idx in 
    (
    declare_changed(idx);
    cs_walker:=set_principal_mt_at_module (!cs_walker) mname pr_modif_time;
    cs_walker:=set_mli_mt_at_module (!cs_walker) mname mli_modif_time;
    cs_walker:=set_direct_fathers_at_module (!cs_walker) mname direct_fathers;
    cs_walker:=set_product_up_to_date_at_module (!cs_walker) mname false;
    )
)(Ennig.ennig 1 n) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then ((!cs_walker,[]),[]) else
let _=PrivateThree.announce_changed_modules changed_modules in
(PrivateThree.put_md_list_back_in_order tolerate_cycles 
  (!cs_walker) changed_modules,
(!ref_for_changed_shortpaths));;  

let printer_equipped_types_from_data cs=
  let n=Small_array.size (modules cs) in
  Option.filter_and_unpack (
    fun idx->
    let hm=hm_at_idx cs idx
    and pr_end=principal_ending_at_idx cs idx in
    let mlx=Dfn_join.to_ending hm pr_end in
    let ap=Dfn_full.to_absolute_path mlx in
    let text=Io.read_whole_file ap in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(hm)
    else None
  ) (Ennig.ennig 1 n);;
 



exception Already_registered_file of Dfn_full_t.t;;  
exception Overcrowding of Dfn_full_t.t*(Dfa_ending_t.t list);;
exception Bad_pair of Dfn_full_t.t*Dfa_ending_t.t;; 


let register_mlx_file_on_monitored_modules cs mlx_file =
          let n=Small_array.size (modules cs) in
          let hm=Dfn_full.to_endingless mlx_file
          and ending=Dfn_full.to_ending mlx_file in 
          let nm=Dfn_full.to_module mlx_file in
          let opt_idx=seek_module_index cs nm in
          if opt_idx=None
          then  let info=complete_id_during_new_module_registration cs mlx_file in
                Coma_state_field.push_right_in_each cs info 
          else
          let idx=Option.unpack(opt_idx) in
          let edgs=registered_endings_at_idx cs idx in
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
                Dfn_join.to_ending hm old_pr_end in
              let (hm,_,old_mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=
                 complete_info cs old_mlx_file in
               let new_mlimt = md_compute_modification_time hm ending in
               let new_dt=(hm,old_pr_end,true,prmt,new_mlimt,libned,dirfath,allanc,dirned,false) in
               Coma_state_field.set_in_each cs idx new_dt
          else
          let new_dt=complete_id_during_new_module_registration cs mlx_file in 
          let (_,_,_,_,_,libned,dirfath,allanc,dirned,_)=new_dt in
          let temp3=List.rev(dirfath) in
          if temp3=[]
          then Coma_state_field.set_in_each cs idx new_dt 
          else  
          let last_father=List.hd(temp3) in
          let last_father_idx=Small_array.leftmost_index_of_in last_father (modules cs) in
          let nm=Dfn_endingless.to_module hm in 
          let cs_walker=ref(cs) in 
          let _=
            (
              for k=last_father_idx+1 to n 
              do
              let current_anc= ancestors_at_idx (!cs_walker) k in  
              if not(List.mem nm current_anc)
              then ()
              else  
                   let current_libs= needed_libs_at_idx cs k in
                   let new_ancestors=Small_array.filter_and_unpack(
                      fun nm2->
                      if (List.mem nm2 allanc)||(List.mem nm2 current_anc)
                      then Some(nm2)
                      else None
                    ) (modules (!cs_walker)) 
                    and new_libs=List.filter (
                      fun lib->(List.mem lib libned)||(List.mem lib current_libs)
                    ) Ocaml_library.all_libraries in  
                    let current_module = module_at_idx (!cs_walker) k in 
                    let ordered_dirs=Tidel.teuzin
                       (Tidel.safe_set(needed_dirs_at_module (!cs_walker) current_module))
                       (Tidel.safe_set (dirned)) in
                    let new_dirs=Ordered.forget_order(ordered_dirs) in
                    cs_walker:=set_ancestors_at_module (!cs_walker) current_module new_ancestors;
                    cs_walker:=set_needed_libs_at_module (!cs_walker) current_module new_libs;
                    cs_walker:=set_needed_dirs_at_module (!cs_walker) current_module new_dirs;
              done;
              cs_walker:=Coma_state_field.remove_in_each_at_index (!cs_walker) idx;
              cs_walker:=Coma_state_field.push_after_in_each (!cs_walker) last_father_idx new_dt;  
            )
          in
          (!cs_walker);;

module Modern = struct 

exception Unregistered_cmi of Dfn_endingless_t.t;;
exception Unregistered_cmo of Dfn_endingless_t.t;;

let command_for_cmi (cmod:Compilation_mode_t.t) dir cs hm=
    let nm=Dfn_endingless.to_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=Dfn_endingless.to_line hm in
    let mli_reg=check_ending_in_at_idx Dfa_ending.mli cs idx in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                       then Some("mv "^s_fhm^".o "^s_root^workdir) 
                       else None) in 
    let central_cmd=
        (Compilation_mode.executioner cmod)^
        (needed_dirs_and_libs_in_command cmod cs idx)^
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
   
  let command_for_cmo (cmod:Compilation_mode_t.t) dir cs hm=
    let nm=Dfn_endingless.to_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=Dfn_endingless.to_line hm in
    let dir_and_libs=needed_dirs_and_libs_in_command cmod cs idx in
    let mli_reg=check_ending_in_at_idx Dfa_ending.mli cs idx in 
    let full_mli=s_fhm^".mli" in
    let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                       then Some("mv "^s_fhm^".o "^s_root^workdir) 
                       else None) in 
    let central_cmds=
    [ 
      (Compilation_mode.executioner cmod)^dir_and_libs^" -c "^s_fhm^".ml";
      "mv "^s_fhm^".cm* "^s_root^workdir
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

let command_for_module_separate_compilation cmod cs hm=
    let dir = root cs in 
    let nm=Dfn_endingless.to_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_element(hm)) else 
    let idx=Option.unpack opt_idx in
    let mli_reg=check_ending_in_at_idx Dfa_ending.mli cs idx
    and ml_reg=check_ending_in_at_idx Dfa_ending.ml cs idx in
    let temp2=(
    let co=command_for_cmo cmod dir cs hm in 
    if mli_reg
    then let ci=command_for_cmi cmod dir cs hm in 
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
    let nm_deps_with_indices = Image.image (
       fun nm->let idx=find_module_index cs nm in 
               let subdir=subdir_at_idx cs idx in 
        (idx,subdir,nm)
    ) nm_deps in 
    let s_root=Dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) in
    let unpointed_short_path = Cull_string.before_rightmost short_path '.' in 
    let libs_for_prow = 
      Tidel.diforchan(
      Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Image.image Dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Image.image 
     (fun (idx,_,_) -> Tidel.diforchan(needed_libs_at_idx cs idx)) nm_deps_with_indices in
    let pre_libs2=Ordered.forget_order (Tidel.big_teuzin (libs_for_prow::pre_libs1)) in 
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
    let nm_deps_with_indices = Image.image (
       fun nm->let idx=find_module_index cs nm in 
               let subdir=subdir_at_idx cs idx in 
        (idx,subdir,nm)
    ) nm_deps in 
    let s_root=Dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) 
    and ending=Compilation_mode.ending_for_element_module cmod 
    and product_ending=Compilation_mode.ending_for_final_product cmod  in
    let cm_elements_but_the_last = Image.image (
      fun (idx,subdir,nm)->
         (* s_root^workdir ^ *) (Dfa_module.to_line nm)^ending
    ) nm_deps_with_indices in 
    let unpointed_short_path = Cull_string.before_rightmost rootless_path '.' in 
    let nm_name = (Cull_string.after_rightmost unpointed_short_path '/') in 
    let last_cm_element=nm_name^ending in 
    let all_cm_elements= (cm_elements_but_the_last) @ [last_cm_element] in 
    let libs_for_prow = 
      Tidel.diforchan(
      Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Image.image Dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Image.image 
     (fun (idx,_,_) -> Tidel.diforchan(needed_libs_at_idx cs idx)) nm_deps_with_indices in
    let pre_libs2=Ordered.forget_order (Tidel.big_teuzin (libs_for_prow::pre_libs1)) in 
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


exception Failed_during_compilation of (int*Dfn_endingless_t.t*string);;

let rec helper_for_feydeau  (cmod:Compilation_mode_t.t) cs (rejected,treated,to_be_treated)=
     match to_be_treated with 
     []->(cs,rejected,List.rev treated)
     |triple::other_triples->
       let (idx,hm,cmd)=triple in
       let nm=Dfn_endingless.to_module hm in 
       if (Unix_command.uc cmd)=0
       then 
            let cs2=set_product_up_to_date_at_module cs nm true in 
            helper_for_feydeau cmod cs2 (rejected,(idx,hm)::treated,other_triples)
       else if (cmod<>Compilation_mode_t.Usual)
            then raise(Failed_during_compilation(triple))
            else 
            let triples_after=snd(Prepared.partition_in_two_parts (fun (idx2,_,_)->idx2<>idx) other_triples) in 
            let (rejected_siblings_as_triples,survivors)=List.partition
           (
              fun (idx2,hm2,_)->
                List.mem nm (ancestors_at_idx cs idx2)
           ) triples_after in 
           let rejected_siblings_with_redundancies =  
              Image.image (fun (idx2,hm2,_)->(idx2,hm2) ) rejected_siblings_as_triples in 
           let rejected_siblings = Listennou.nonredundant_version rejected_siblings_with_redundancies in    
           let newly_rejected = (idx,hm)::rejected_siblings in 
           let cs_walker=ref(cs) in 
           let _=List.iter(
              fun (idx3,hm3)->
                 let mname3=Dfn_endingless.to_module hm3 in 
                cs_walker:=set_product_up_to_date_at_module (!cs_walker) mname3 false
           ) newly_rejected in 
           helper_for_feydeau cmod (!cs_walker) (rejected@newly_rejected,treated,survivors) ;;

let dependencies_inside_shaft cmod cs (opt_indices,opt_rootless_path)=
   match cmod with 
   Compilation_mode_t.Usual->Option.unpack opt_indices
   |_->let rootless_path=Option.unpack opt_rootless_path in 
       let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath (root cs))^rootless_path) in 
       let nm_direct_deps = Look_for_module_names.names_in_ml_file full_path in 
       let nm_deps=modules_with_their_ancestors cs nm_direct_deps in 
       Option.filter_and_unpack (seek_module_index cs) nm_deps;;


let list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_indices,opt_short_path)=
   let l=dependencies_inside_shaft cmod cs (opt_indices,opt_short_path) in 
   let temp1=Image.image (fun idx->
     let hm=hm_at_idx cs idx in 
     let cmds=Modern.command_for_module_separate_compilation cmod cs hm in 
    Image.image (fun cmd->(idx,hm_at_idx cs idx,cmd) ) cmds ) l in 
    List.flatten temp1;;

let list_of_commands_for_connecting_part_of_feydeau cmod cs (opt_indices,opt_short_path)=
   let cmds=(
   match cmod with 
   Compilation_mode_t.Usual->[] 
   |_->
      let short_path=Option.unpack opt_short_path in 
      Modern.command_for_predebuggable_or_preexecutable cmod cs short_path) in 
   cmds;;


let list_of_commands_for_end_part_of_feydeau cmod cs (opt_indices,opt_short_path)= 
   let cmds=(
   match cmod with 
   Compilation_mode_t.Usual->[] 
   |_->
      let short_path=Option.unpack opt_short_path in 
      Modern.command_for_debuggable_or_executable cmod cs short_path) in 
   cmds;;

let list_of_commands_for_ternary_feydeau cmod cs short_path=
   let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod cs (None,Some(short_path)) in 
   let cmds1=Image.image (fun (_,_,cmd)->cmd) pre_cmds1
   and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod cs (None,Some(short_path))
   and cmds3=list_of_commands_for_end_part_of_feydeau cmod cs (None,Some(short_path)) in 
   cmds1@cmds2@cmds3;;


let shaft_part_of_feydeau cmod cs (opt_indices,opt_short_path)=
  let cmds=list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_indices,opt_short_path) in  
  helper_for_feydeau cmod cs ([],[],cmds);; 

let end_part_of_feydeau cmod cs (opt_indices,opt_short_path)=
  match cmod with 
   Compilation_mode_t.Usual->()
   |_->
     let all_cmds=
       (list_of_commands_for_connecting_part_of_feydeau cmod cs (opt_indices,opt_short_path))@
       (list_of_commands_for_end_part_of_feydeau cmod cs (opt_indices,opt_short_path)) in 
     let _=Image.image  Unix_command.hardcore_uc all_cmds in 
     ()
  
let feydeau cmod cs (opt_indices,opt_short_path)=
  let answer=shaft_part_of_feydeau cmod cs (opt_indices,opt_short_path) in 
  let _=end_part_of_feydeau cmod cs (opt_indices,opt_short_path) in 
  answer;; 



let usual_feydeau cs indices = feydeau Compilation_mode_t.Usual cs (Some(indices),None);;


end;;  


let recompile cs=
     let ((cs2,nms_to_be_updated),short_paths)=
        recompile_on_monitored_modules false cs in
     if nms_to_be_updated=[] then (cs2,false,[]) else
     let new_dirs=compute_subdirectories_list cs2  in
     let indexed_nms=Image.image(
       fun nm->
       let idx=find_module_index cs2 nm in 
       (idx,hm_at_idx cs2 idx)
     ) nms_to_be_updated in 
     let indices = Image.image fst indexed_nms in 
     let (cs3,rejected_pairs,accepted_pairs)=
       Ocaml_target_making.usual_feydeau cs2 indices in 
     let rejected_hms=Image.image snd rejected_pairs in  
      let new_preqt=Image.image(
        fun (hm,_)->(hm,not(List.mem hm rejected_hms))
      )  (preq_types cs3) in   
     let cs4=set_directories cs3 new_dirs in 
     let cs5=set_preq_types cs4 new_preqt in 
    (cs5,true,short_paths);;       

let add_printer_equipped_type cs hm=
  set_preq_types cs ((preq_types cs)@[hm]);;

let remove_printer_equipped_type cs hm=
  set_preq_types cs (List.filter (fun hm2->hm2<>hm) (preq_types cs));;

let uple_form cs=
  (cs,
   directories cs,
   preq_types cs
   );;

    
(*
let backup cs diff opt= Backup_coma_state.backup
  (root cs,backup_dir cs,push_after_backup cs) 
    diff opt;;
*)


  let unregister_mlx_file_on_targets root_dir cs mlx=
    let nm=Dfn_full.to_module mlx in 
    let idx=find_module_index cs nm in
    let n=size cs in 
    let sibling_indices=List.filter(
        fun jdx->
         List.mem nm (ancestors_at_idx cs jdx)
    )(Ennig.ennig idx (n+1)) in 
    let was_lonely=
      (List.length(registered_endings_at_idx cs idx)=1) in 
    let _=set_product_up_to_date_at_module cs nm false in 
    let cs2=unregister_mlx_file_on_monitored_modules cs mlx in
    let new_dirs=compute_subdirectories_list cs2 in
    let cs3=(if was_lonely 
           then cs2
           else ( fun (cs4,_,_)->cs4)
           (Ocaml_target_making.usual_feydeau 
             cs2 (idx::sibling_indices)) ) in 
    (cs3,new_dirs);;   

exception FileWithDependencies of 
Dfn_full_t.t*(Dfa_module_t.t list);;


let forget_file_on_targets root_dir pair ap=
  let (cs,dirs)=pair in
  let rootless_path = Dfn_common.decompose_absolute_path_using_root ap root_dir in 
  let mlx = Dfn_join.root_to root_dir rootless_path in 
  let hm=Dfn_full.to_endingless mlx  in
  let nm=Dfn_endingless.to_module hm in
  match seek_module_index  cs nm with
   None->pair
  |Some(_)->
   let bel=below cs hm in
    if bel=[]
    then let fn=Dfn_endingless.to_line hm  in
         let _=Image.image
         (fun edg->Unix_command.uc("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         unregister_mlx_file_on_targets root_dir cs mlx
    else raise(FileWithDependencies(mlx,bel));;



let forget_file cs ap=
    let (cs2,new_dirs)= 
     forget_file_on_targets (root cs) (cs,directories cs) ap in  
     set_directories cs2 new_dirs;;

module Unregister_module=struct


let on_targets root_dir cs hm=
    let (cs2,short_paths)=unregister_module_on_monitored_modules  cs hm in
    let new_dirs=compute_subdirectories_list cs2  in
     ((cs2,new_dirs),short_paths);;   
     
   

end;;          
   


let forget_unregistered_file root_dir ap=
   let s_dir=Dfa_root.connectable_to_subpath root_dir in
   let n_dir=String.length s_dir in
   let s_ap=Absolute_path.to_string ap in
   let subpath=Cull_string.cobeginning n_dir s_ap in
   let trash_dir=Dfa_subdirectory.without_trailing_slash
               (Coma_constant.abandoned_ideas_subdir) in
   let new_subpath=(Current_date.current_date())^"_"^
         (Replace_inside.replace_inside_string ("/","_dir_") subpath) in
   let _=Unix_command.uc ("mkdir -p "^s_dir^trash_dir) in
   let _=Unix_command.uc ("touch "^s_dir^trash_dir^"/"^new_subpath) in
   let _=Unix_command.uc ("mv "^s_ap^" "^s_dir^trash_dir^"/"^new_subpath) in
   subpath;;

exception ModuleWithDependenciesDuringForgetting of 
        Dfn_endingless_t.t*(Dfa_module_t.t list);;

exception Non_registered_module_during_forgetting of Dfa_module_t.t;;
      
let forget_module_on_targets root_dir (cs,dirs) hm=
        let nm=Dfn_endingless.to_module hm in
        match seek_module_index  cs nm with
         None->raise(Non_registered_module_during_forgetting(nm))
        |Some(dt)->
         let bel=below cs hm in
          if bel=[]
          then let (answer,short_paths)=Unregister_module.on_targets root_dir 
                          cs hm in
               let sfn=Dfa_module.to_line nm in
               let _=Image.image
               (fun edg->
                let cmd="rm -f _build/"^sfn^edg in
                Unix_command.uc(cmd))
               [".cm*";".d.cm*";".caml_debuggable"] in
               let temp1=Image.image (fun t->
                  Absolute_path.of_string((Dfa_root.connectable_to_subpath root_dir)^t)
               ) short_paths in
               let _=Image.image 
               (forget_unregistered_file root_dir) temp1 in
               (answer,short_paths)
          else raise(ModuleWithDependenciesDuringForgetting(hm,bel));;
      

let forget_module cs hm=
    let ((cs2,new_dirs),short_paths)= 
      forget_module_on_targets (root cs) (cs,directories cs) hm in
      let _=(
          set_directories cs new_dirs;
      ) in
      (cs2,short_paths);;          

let read_persistent_version x=
        let full_path=Dfn_join.root_to (root x)  Coma_constant.rootless_path_for_targetfile in
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
     
    let init_dir=
      Dfa_subdirectory.connectable_to_subpath 
      (Coma_constant.automatically_generated_subdir);;
    
    let copy_special_files s_main_dir=
      let dname=Coma_constant.name_for_debugged_module in
      let _=Image.image(
       fun s->
        Unix_command.uc 
          ("mkdir -p "^s_main_dir^"/"^(Dfa_subdirectory.without_trailing_slash s))
      ) [
           Coma_constant.automatically_generated_subdir;
           Coma_constant.temporary_subdir;
        ]
      in 
      let _=Unix_command.uc ("mkdir -p "^s_main_dir^"/"^(Dfa_subdirectory.connectable_to_subpath Coma_constant.build_subdir)) in
      let _=Unix_command.uc ("mkdir -p "^s_main_dir^"/"^(Dfa_subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)) in
      let _=Unix_command.uc ("mkdir -p "^s_main_dir^"/"^(Dfa_subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir)) in
      let augen = Image.image Dfn_rootless.to_line Coma_constant.up_to_date_but_not_registered_files in 
      let _=Image.image (fun s->
        Unix_command.uc ("touch "^s_main_dir^"/"^s)
         ) ([dname^".ml";
           ".ocamlinit"]
           @
           augen
        ) in ();;
    
    let put_default_content_in_special_files s_main_dir=
      (Io.overwrite_with 
      (Absolute_path.of_string (s_main_dir^"/.ocamlinit"))
      (
      "\n#use\""^(Dfn_rootless.to_line Coma_constant.rootless_path_for_loadingsfile)^"\""^Double_semicolon.ds^
      "\n#use\""^(Dfn_rootless.to_line Coma_constant.rootless_path_for_printersfile)^"\""^Double_semicolon.ds^
      "\nopen Needed_values;;"^
      "\ninitialize_toplevel();;"
       );
      Io.overwrite_with 
      (Absolute_path.of_string (s_main_dir^"/"^init_dir^"/my_printers.ml"))
      "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n");; 
      
    
let select_good_files s_main_dir=
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
      let temp3=detect_identical_names ([],temp2) in
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
      let _=display_circular_dependencies
      (fun (j1,(ap1,s1))->s1) temp1 cycles in
      Image.image (fun (j,_)->snd(List.nth temp1 (j-1)) ) good_list;;
      
    let from_prepared_list dir backup_dir g_after_b l=
       let temp1=Option.filter_and_unpack (fun (ap,s)->
          try (Some(Dfn_full.from_absolute_path_with_root ap dir)) with 
          _->None
       ) l in
       Try_to_register.mlx_files (Coma_state_field.empty_one dir backup_dir g_after_b) temp1;;
    
    end;;   
    
let from_main_directory dir backup_dir g_after_b=
      let old_s=Dfa_root.connectable_to_subpath(dir) in
      let s_main_dir=Cull_string.coending 1 old_s in (* mind the trailing slash *)
      let _=
        (Private.copy_special_files s_main_dir;
         Private.put_default_content_in_special_files s_main_dir 
        ) in
        let temp1=Private.select_good_files s_main_dir in
        let temp2=Private.clean_list_of_files dir temp1 in
        let temp3=Private.compute_dependencies temp2 in
        let (failures,cs1)=Private.from_prepared_list dir backup_dir g_after_b temp3 in
        let pre_preqt=printer_equipped_types_from_data cs1 in
        let n=size cs1 in 
        let (cs2,rejected_pairs,_)=
          Ocaml_target_making.usual_feydeau 
          cs1 (Ennig.ennig 1 n) in
        let rejected_hms=Image.image snd rejected_pairs in 
       let preqt=Image.image (fun hm->(hm,not(List.mem hm rejected_hms))) pre_preqt in 
       (cs2,[],preqt);;
    
    

end;;  

let delchacre_from_scratch (source_dir,dir_for_backup) cs=
  let temp1=all_mlx_paths cs in
  let temp3=temp1 in
  let temp4=Image.image (fun ap->
     let rootless_path = Dfn_common.decompose_absolute_path_using_root ap source_dir in 
     Dfn_rootless.to_line rootless_path) temp3 in
 Prepare_dircopy_update.compute_diff
    (source_dir,temp4) dir_for_backup;;

let refresh cs=
      let (cs2,new_tgts,new_ptypes)=
        Target_system_creation.from_main_directory 
             (root cs)
             (backup_dir cs)
             (push_after_backup cs)
         in 
        let new_dirs=compute_subdirectories_list cs2 in
        let new_diff=delchacre_from_scratch (root cs2,backup_dir cs2) cs2 in
        let cs3=set_directories cs2 new_dirs in 
        let cs4=set_preq_types cs3 new_ptypes in
        (cs4,new_diff);; 

module Register_mlx_file=struct

let on_targets (cs,old_dirs) mlx=
    let new_dir=Dfn_full.to_subdirectory mlx in
   let cs2=register_mlx_file_on_monitored_modules cs mlx in
   let new_dirs=
   (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
    in
    let nm=Dfn_full.to_module mlx in 
    let idx=find_module_index cs2 nm in 
    let (cs3,_,_)=Ocaml_target_making.usual_feydeau cs2 [idx] in 
    (cs3,new_dirs);; 
  

end;;  


let register_mlx_file cs mlx=
          let (cs2,new_dirs)= 
          Register_mlx_file.on_targets (cs,directories cs) mlx in   
           set_directories cs2 new_dirs;;            

module Raneme_directory = struct 

let on_subdirectory=Dfa_subdirectory.rename_endsubdirectory;;

let on_printer_equipped_type pair (hm,is_compiled_correctly)=
    (Dfn_endingless.rename_endsubdirectory pair hm,is_compiled_correctly);;


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
         
      
let rename_module cs old_name new_name= 
  let (cs2,(old_files,new_files),modified_files)=
     rename_module_on_monitored_modules cs old_name new_name in   
  let (cs3,_,_)=recompile cs2 in 
  (cs3,(old_files,new_files),modified_files);;   

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
          

let unregister_module cs hm=
        let ((cs2,new_dirs),short_paths)= 
         Unregister_module.on_targets (root cs) cs  hm in 
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
       (seek_module_index cs mn)=None 
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
    let mlx = Dfn_join.root_to (root cs) rootless_path in 
    Dfn_full.to_endingless mlx ;;

module Local_rename_value_inside_module = struct

exception No_module_given of string;;

exception No_value_with_name of string;;

let get_module_inside_name s=
   let f=Cull_string.before_rightmost s '/' in
   if f=""
   then raise(No_module_given(s))
   else f;;
   
let rename_value_inside_module cs s new_name=
   let j=Substring.leftmost_index_of_in "." s in
   if j<0 
   then raise(No_module_given(s))
   else 
   let module_name=Cull_string.beginning (j-1) s in
   let hm=decipher_module cs  module_name 
   and path=decipher_path cs  module_name in 
   let nm=Dfn_endingless.to_module hm in
   let idx1=find_module_index cs nm in
   let pre_temp2=(ancestors_at_idx cs idx1)@[nm] in
   let temp2=Image.image (hm_from_nm cs) pre_temp2 in
   let all_files=Image.image  (fun hm2->
   	 Dfn_full.to_absolute_path(Dfn_join.to_ending hm2 Dfa_ending.ml)
   ) temp2 in
   let temp3=Read_ocaml_files.read_ocaml_files all_files in
   let opt_temp4=Option.seek (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s
   ) temp3 in
   if opt_temp4=None
   then raise(No_value_with_name(s))
   else
   let temp4=Option.unpack(opt_temp4) in
   let (i1,j1)=temp4.Ocaml_gsyntax_item.interval_for_name in
   let _=Overwrite_at_intervals.inside_file [(i1,j1),new_name] path in
   let temp3_again=Read_ocaml_files.read_ocaml_files all_files in
   let beheaded_name=Cull_string.cobeginning j s in
   let s_new_beheaded_name=(fun (fa,nn)->if fa="" then nn else fa^"."^nn)
   (Cull_string.before_rightmost beheaded_name '.',Overwriter.to_string new_name) in
   let new_beheaded_name=Overwriter.of_string s_new_beheaded_name in
   let s_new_full_name=module_name^"."^s_new_beheaded_name in
   let temp4_again=Option.find (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s_new_full_name
   ) temp3_again in
   let k1=Listennou.find_index temp4_again temp3_again in
   let temp5=Listennou.big_tail k1 temp3_again in
   
   let temp6=Option.filter_and_unpack(
      fun itm->
        let txt=itm.Ocaml_gsyntax_item.content in
        let ttemp7=Isolated_occurrences.of_in 
           beheaded_name txt in
        if ttemp7<>[]
        then  let isoc=Isolated_occurrences.of_in beheaded_name txt in
              let replacings=Image.image (fun p->(p,new_beheaded_name)) isoc in
              let new_txt=Overwrite_at_intervals.inside_string
                   replacings txt in
             Some(itm.Ocaml_gsyntax_item.interval_for_content,
                  Overwriter.of_string new_txt)
        else None   
   ) temp5 in
   Overwrite_at_intervals.inside_file temp6 path;;


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
   Ordered_string.diforchan temp3;;

let list_values_from_module_in_modulesystem cs module_name=
   let temp1=all_mlx_paths cs in
   let temp2=Image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Ordered_string.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=Image.image fst temp3 in 
   let temp5=Ordered_string.diforchan temp4 in
   let temp6=Ordered.forget_order temp5 in
   let temp7=Image.image (
      fun x->(x,Option.filter_and_unpack(
        fun (y,ap)->if y=x then Some(ap) else None
      ) temp3)
   ) temp6 in
   temp7;;
 
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
exception No_module_with_name of string;;

let local_seek_module_index cs x=
  let uncapitalized_x=
    Dfa_module.of_line(String.uncapitalize_ascii x) in
  seek_module_index cs  uncapitalized_x;;

let find_half_dressed_module cs x=
   match local_seek_module_index cs x
   with 
   Some(idx)->hm_at_idx cs idx
   |None->raise(No_module_with_name(x));;  



let local_above cs x=
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle_element (hm_from_nm cs nm) in 
    Dfn_endingless.middle_element_to_line middle )
  (above cs (find_half_dressed_module cs x));;


let local_below cs x=
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle_element (hm_from_nm cs nm) in 
    Dfn_endingless.middle_element_to_line middle )
  (below cs (find_half_dressed_module cs x));;

let local_directly_below cs x=
  Image.image (fun nm-> 
    let middle = Dfn_endingless.to_middle_element (hm_from_nm cs nm) in 
    Dfn_endingless.middle_element_to_line middle )
  (directly_below cs (find_half_dressed_module cs x));;

let forget_file_with_backup_before_saving cs x=
   let ap=decipher_path cs x in
   let rootless_path = Dfn_common.decompose_absolute_path_using_root ap (root cs) in 
   let cut_ap=Dfn_rootless.to_line rootless_path in
   let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [cut_ap])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list []) in
   let (cs2,_,_)=recompile cs in 
   let cs3=forget_file cs2 ap in 
   (cs3,diff);; 

let forget_module_with_backup_before_saving cs x=
    let hm = find_half_dressed_module cs x in 
    let (cs2,_,_)=recompile cs in
    let (cs3,short_paths)=forget_module cs2 hm in    
    let ordered_paths=Ordered_string.forget_order(Ordered_string.safe_set(short_paths)) in
    let diff=
      Dircopy_diff.veil
      (Recently_deleted.of_string_list ordered_paths)
      (Recently_changed.of_string_list [])
      (Recently_created.of_string_list []) in
    (cs3,diff);; 
 
let forget cs x=
      if String.contains x '.'
      then forget_file_with_backup_before_saving cs x
      else forget_module_with_backup_before_saving cs x;;


let recompile cs=
  let (cs2,change_exists,short_paths)=recompile cs  in
  let changed_paths=
   (if not change_exists
   then []
   else Ordered_string.forget_order(Ordered_string.safe_set(short_paths))) in
    (cs2,Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list changed_paths)
    (Recently_created.of_string_list [])) ;;


let local_register_mlx_file cs mlx=
    let (cs2,_)=recompile cs  in 
    let cs3=register_mlx_file cs2 mlx in 
    cs3;;  


let register_short_path cs x=
  let rootless_path = Dfn_rootless.of_line x in 
  let mlx=Dfn_join.root_to (root cs) rootless_path in
  let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list [x]) in
  let cs2=register_mlx_file cs mlx in 
  (cs2,diff);;


let local_rename_module cs old_name new_name=
   let old_hm = find_half_dressed_module cs (String.uncapitalize_ascii old_name) 
   and unslashed_new_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
   let (cs2,(old_files,new_files),modified_files)=rename_module cs old_hm unslashed_new_name in
   let diff=Dircopy_diff.veil
    (Recently_deleted.of_string_list old_files)
    (Recently_changed.of_string_list modified_files)
    (Recently_created.of_string_list new_files) in
   (cs2,diff);;

let local_relocate_module cs capitalized_or_not_old_hm_name new_subdir=
  let old_hm_name = String.uncapitalize_ascii  capitalized_or_not_old_hm_name in 
  let idx = Option.unpack(local_seek_module_index cs old_hm_name) in 
  let old_hm = find_half_dressed_module cs old_hm_name 
  and old_short_paths = short_paths_at_idx cs idx  in 
  let (cs2,_)=relocate_module cs old_hm new_subdir in
  let  new_short_paths = short_paths_at_idx cs2 idx  in 
  let diff=Dircopy_diff.veil
    (Recently_deleted.of_string_list old_short_paths)
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list new_short_paths) in
   (cs2,diff);;   

let local_rename_directory cs old_subdir new_subdirname=
   let old_short_paths=short_paths_inside_subdirectory cs old_subdir in
   let cs2=rename_directory cs (old_subdir,new_subdirname) in 
   let s_old_subdir=Dfa_subdirectory.without_trailing_slash old_subdir in
   let subdir_father = Cull_string.before_rightmost s_old_subdir '/' in 
   let new_subdir=(if subdir_father="" 
                   then new_subdirname 
                   else subdir_father^"/"^new_subdirname) in 
   let new_short_paths=short_paths_inside_subdirectory cs2 (Dfa_subdirectory.of_line new_subdir) in
   let diff=Dircopy_diff.veil
    (Recently_deleted.of_string_list old_short_paths)
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list new_short_paths) in
   (cs2,diff);;   


let rename_string_or_value cs old_name new_name=
  (*
  let dependent_full_paths=files_containing_string cs old_name in
  let modified_full_paths=(  
  let j=Substring.leftmost_index_of_in "." old_name in
   if j<0 
   then dependent_full_paths
   else 
   let module_name=Cull_string.beginning (j-1) old_name in
   let full_path=decipher_path cs  module_name in
   full_path::dependent_full_paths
  ) in   
  let modified_short_paths=Image.image (
     fun ap->
       let s_ap=Absolute_path.to_string ap 
       and s_root=Root_directory.connectable_to_subpath (root cs) in 
       Cull_string.two_sided_cutting (s_root,"") s_ap
  ) modified_full_paths in 
  let diff=Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list modified_short_paths)
    (Recently_created.of_string_list []) in
  *)  
  let _=Values_in_modules.rename_string_or_value cs old_name new_name in 
  let (cs2,diff)=recompile cs in 
  (cs2,diff);;

end;; 


