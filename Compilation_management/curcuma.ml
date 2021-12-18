(* 

#use"Compilation_management/curcuma.ml";;

*)

module Private = struct

let qarent cs = {
   Fw_with_batch_compilation_t.parent = cs.Coma_state_t.frontier_with_unix_world ;
   last_compilation_result_for_module = cs.Coma_state_t.last_compilation_result_for_module;
} ;;

let tneraq fw = {
   Coma_state_t.frontier_with_unix_world = fw.Fw_with_batch_compilation_t.parent ;
   last_compilation_result_for_module = fw.Fw_with_batch_compilation_t.last_compilation_result_for_module ;
} ;;

let below cs mn = Fw_with_batch_compilation.below (qarent cs) mn ;;
let default_constructor = tneraq ;;
let directly_below cs mn = Fw_with_batch_compilation.directly_below (qarent cs) mn ;;
let root cs = Fw_with_batch_compilation.root (qarent cs) ;;

let usual_batch cs modnames = 
  let (new_parent,rejected_ones,accepted_ones) = Fw_with_batch_compilation.usual_batch (qarent cs) modnames in 
  (tneraq new_parent,rejected_ones,accepted_ones) ;; 


  let salt = "Coma_"^"state_field.";;
  
  let frontier_with_unix_world_label      = salt ^ "frontier_with_unix_world";;
  let last_compilation_result_for_module_label = salt ^ "last_compilation_result_for_module";;
  
  let cr_of_pair f l= Crobj_converter_combinator.of_pair_list  Dfa_module.to_concrete_object f l;;
  let cr_to_pair f crobj= Crobj_converter_combinator.to_pair_list  Dfa_module.of_concrete_object f crobj;;
  

  let of_concrete_object ccrt_obj = 
     let g=Concrete_object.get_record ccrt_obj in
     {
        Coma_state_t.frontier_with_unix_world = Fw_with_dependencies.of_concrete_object (g frontier_with_unix_world_label);
        last_compilation_result_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g last_compilation_result_for_module_label);
     };; 
  
  let to_concrete_object cs=
     let items= 
     [
      frontier_with_unix_world_label, Fw_with_dependencies.to_concrete_object cs.Coma_state_t.frontier_with_unix_world;
      last_compilation_result_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object cs.Coma_state_t.last_compilation_result_for_module;    
     ]  in
     Concrete_object_t.Record items;;
  
    


(*  
module Automatic = struct 

  exception Module_not_found of Dfa_module_t.t ;;

  (* Converters *)
  let of_t x=x;;
  let to_t x=x;;
  (*
  in debug mode, change the above to
  let of_t (Coma_state_t.CS x)=x;;
  let to_t x=(Coma_state_t.CS x);;
  *)
  (* End of converters *)
  
  
  let frontier_with_unix_world cs = cs.Coma_state_t.frontier_with_unix_world;;
  let configuration cs=Fw_with_dependencies.configuration (frontier_with_unix_world cs) ;;
  let backup_dir cs=(configuration cs).Fw_configuration_t.dir_for_backup;;
  let gitpush_after_backup cs=(configuration cs).Fw_configuration_t.gitpush_after_backup;;   
  let github_url cs=(configuration cs).Fw_configuration_t.github_url;;
  let encoding_protected_files cs=(configuration cs).Fw_configuration_t.encoding_protected_files;;
  
  
  let subdir_for_module cs mn= 
   Fw_with_dependencies.subdir_for_module
     (frontier_with_unix_world cs) mn ;;
     
  
  let principal_ending_for_module cs mn=
    Fw_with_dependencies.principal_ending_for_module
     (frontier_with_unix_world cs) mn ;;
  
  let mli_presence_for_module cs mn=
    Fw_with_dependencies.mli_presence_for_module
    (frontier_with_unix_world cs) mn;;
  
  let principal_mt_for_module cs mn=
    Fw_with_dependencies.principal_mt_for_module
    (frontier_with_unix_world cs) mn ;;
  
  let mli_mt_for_module cs mn=
    Fw_with_dependencies.mli_mt_for_module
    (frontier_with_unix_world cs) mn;;
  
  let needed_libs_for_module cs mn=
    Fw_with_dependencies.needed_libs_for_module
    (frontier_with_unix_world cs) mn;;
  
  let direct_fathers_for_module cs mn=
    Fw_with_dependencies.direct_fathers_for_module
    (frontier_with_unix_world cs) mn;;
  
  let ancestors_for_module cs mn=
    Fw_with_dependencies.ancestors_for_module
    (frontier_with_unix_world cs) mn;;

  
  let needed_dirs_for_module cs mn=
    Fw_with_dependencies.needed_dirs_for_module
    (frontier_with_unix_world cs) mn;;
  
  let last_compilation_result_for_module cs mn=
     try  List.assoc mn ((of_t cs).Coma_state_t.last_compilation_result_for_module) with     
     _ -> raise(Module_not_found(mn));;
  
  let all_subdirectories cs = 
    Fw_with_batch_compilation.all_subdirectories (qarent cs) ;;
  
  
  let printer_equipped_types cs =
    Fw_with_dependencies.printer_equipped_types
    (frontier_with_unix_world cs) ;;
  
  
  
  let dep_ordered_modules cs=
    Fw_with_dependencies.dep_ordered_modules
    (frontier_with_unix_world cs) ;;
  
  let test_module_for_registration cs modname=
    List.mem modname (dep_ordered_modules cs);;
  
  let follows_it_but_does_not_necessarily_depend_on_it cs mn=
      let (_,_,after) = Three_parts.select_center_element_and_reverse_left (fun x->x=mn)
        (dep_ordered_modules cs) in 
      after;;
  
  
  let all_used_subdirs cs = 
     let fw = frontier_with_unix_world cs in 
     Image.image (Fw_with_dependencies.subdir_for_module fw) (dep_ordered_modules cs) ;;
  
  
  
  (* Setters  *)
  
  let set_frontier_with_unix_world cs v= 
     let ccs=of_t cs in 
     to_t({ccs with Coma_state_t.frontier_with_unix_world=v});;
  
  

  
  let set_last_compilation_result_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.last_compilation_result_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.last_compilation_result_for_module=new_assocs });;
      
  
  
  (* Adhoc setters *)
  
  exception Impose_last_change_exn of Dircopy_diff_t.t ;;
  
  let impose_last_changes cs diff =
     let old_fw = frontier_with_unix_world cs in 
     let old_diff = Fw_with_dependencies.last_noticed_changes old_fw in 
     if not(Dircopy_diff.is_empty old_diff)
     then raise(Impose_last_change_exn(old_diff))
     else 
     let new_fw = 
      Fw_with_dependencies.set_last_noticed_changes old_fw diff in  
     set_frontier_with_unix_world cs new_fw ;;
  
  (* End of adhoc setters *)
  
  
  
  let empty_one config=
      to_t({
       Coma_state_t.frontier_with_unix_world= Fw_with_dependencies.empty_one config;
       last_compilation_result_for_module = [];
  });;
  
  
  
  

  let change_one_module_name wrapped_cs old_mn new_mn=
      (* note that printer_equipped_types are not dealt with here *)
      let cs=of_t wrapped_cs in
      let rep_pair = (old_mn,new_mn) in 
      let new_products_up_to_date = Associative_list.change_name_for_key  cs.Coma_state_t.last_compilation_result_for_module rep_pair  in 
  to_t({ cs with 
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;
  
  let remove_in_each_at_module wrapped_cs mname=
      let cs=of_t wrapped_cs in
      let new_products_up_to_date = Associative_list.remove_key  cs.Coma_state_t.last_compilation_result_for_module mname  in 
  to_t({ cs with 
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;
  
  
  
  let push_right_in_each wrapped_cs (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
      let nm=Dfn_endingless.to_module hm 
      and  cs=of_t wrapped_cs in
      let new_products_up_to_date = (cs.Coma_state_t.last_compilation_result_for_module)@[nm,upy]  in 
  to_t({ cs with
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;
  
  let set_in_each wrapped_cs nm (pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
      let cs=of_t wrapped_cs in
      let new_products_up_to_date = Associative_list.change_value_for_key  cs.Coma_state_t.last_compilation_result_for_module (nm,upy)  in 
  to_t({ cs with 
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;
    
  
      
  let reposition_in_each wrapped_cs mn1 mn2=
      let cs=of_t wrapped_cs in
      let l_rep=(fun l->Associative_list.reposition_by_putting_snd_immediately_after_fst l mn1 mn2 ) in 
      let new_products_up_to_date = l_rep cs.Coma_state_t.last_compilation_result_for_module in 
  to_t({ cs with 
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;
  
  
  let reorder wrapped_cs reordered_list_of_modules =
       let cs=of_t wrapped_cs in 
      let l_rep =(fun l->Associative_list.reorder l reordered_list_of_modules) in    
      let new_products_up_to_date = l_rep cs.Coma_state_t.last_compilation_result_for_module  in 
  to_t({ cs with 
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;  
  
  
  
  let push_after_module_in_each wrapped_cs pivot (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
      let nm=Dfn_endingless.to_module hm
      and  cs=of_t wrapped_cs in
      let new_products_up_to_date = Associative_list.push_immediately_after cs.Coma_state_t.last_compilation_result_for_module (nm,upy) pivot  in 
  to_t({ cs with 
        Coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
  });;
      
  let endingless_at_module cs mn=
     Dfn_endingless_t.J(
          root cs,
          subdir_for_module cs mn,
          mn
      );;
  
  let printer_equipped_types_from_preceding_data  
     (frontier_with_unix_world_field,
        modules_field,
          subdir_for_modules_field,
            principal_ending_for_module_field)=
    let the_root = Fw_with_small_details.root frontier_with_unix_world_field in         
    Option.filter_and_unpack (
      fun mn->
      let subdir = List.assoc mn subdir_for_modules_field 
      and pr_end= List.assoc mn principal_ending_for_module_field  in
      let rootless=Dfn_rootless_t.J(subdir,mn,pr_end) in 
      let text=Fw_with_small_details.get_content frontier_with_unix_world_field rootless in
      if (Substring.is_a_substring_of ("let "^"print_out ") text)
      then let eless=Dfn_endingless_t.J(the_root,subdir,mn) in 
           Some(eless)
      else None
    ) modules_field;;    
  

end ;;  


(* Inherited values *)




let backup_dir =Automatic.backup_dir;;
let gitpush_after_backup =Automatic.gitpush_after_backup;;
let github_url =Automatic.github_url;;
let encoding_protected_files =Automatic.encoding_protected_files;;

let subdir_for_module = Automatic.subdir_for_module ;;
let principal_ending_for_module = Automatic.principal_ending_for_module ;;
let mli_presence_for_module = Automatic.mli_presence_for_module ;;
let principal_mt_for_module = Automatic.principal_mt_for_module ;;
let mli_mt_for_module = Automatic.mli_mt_for_module ;;
let needed_libs_for_module  = Automatic.needed_libs_for_module ;;
let direct_fathers_for_module = Automatic.direct_fathers_for_module ;;
let ancestors_for_module = Automatic.ancestors_for_module ;; 
let needed_dirs_for_module  = Automatic.needed_dirs_for_module ;;
let last_compilation_result_for_module = Automatic.last_compilation_result_for_module ;;
let all_subdirectories = Automatic.all_subdirectories;;
let printer_equipped_types = Automatic.printer_equipped_types;;


let set_frontier_with_unix_world = Automatic.set_frontier_with_unix_world;;
let set_last_compilation_result_for_module = Automatic.set_last_compilation_result_for_module ;;


let dep_ordered_modules = Automatic.dep_ordered_modules;;
let follows_it = Automatic.follows_it_but_does_not_necessarily_depend_on_it;;
let all_used_subdirs = Automatic.all_used_subdirs;;


let change_one_module_name = Automatic.change_one_module_name ;;
let configuration = Automatic.configuration ;;
let empty_one = Automatic.empty_one ;;
let impose_last_changes = Automatic.impose_last_changes ;;
let to_concrete_object = Automatic.to_concrete_object ;;


(* End of inherited values *)


let endingless_at_module cs mn=
   Dfn_endingless_t.J(
        root cs,
        subdir_for_module cs mn,
        mn
    );;


let endingless_from_mildly_capitalized_module_name cs mname=
    endingless_at_module cs (Dfa_module.of_line(String.capitalize_ascii mname));;

let check_ending_in_at_module edg cs mn=
   if edg=principal_ending_for_module cs mn
   then true 
   else 
   if edg=Dfa_ocaml_ending_t.Mli
   then mli_presence_for_module cs mn
   else false;;



let acolytes_at_module cs mn=
  let eless = endingless_at_module cs mn in
  Option.filter_and_unpack (fun 
  edg->
     if check_ending_in_at_module edg cs mn
     then Some(Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending edg))
     else None
) Dfa_ocaml_ending.all ;;



let rootless_lines_at_module cs mn=
   Image.image Dfn_full.to_rootless_line (acolytes_at_module cs mn);;
  

let rootless_paths_at_module cs mn=
   Image.image Dfn_full.to_rootless (acolytes_at_module cs mn);;
  

let registered_endings_at_module cs mn=
  List.filter (fun edg->
  check_ending_in_at_module edg cs mn 
  ) Dfa_ocaml_ending.all ;;



let check_for_single_ending_at_module cs mn=
  if mli_presence_for_module cs mn
  then (principal_ending_for_module cs mn)=(Dfa_ocaml_ending_t.Mli)
  else true ;;



let size cs = List.length (dep_ordered_modules cs);;      

let all_rootlesses cs =
   List.flatten(Image.image (rootless_paths_at_module cs) (dep_ordered_modules cs));;




let preq_types_with_extra_info cs = 
    Fw_with_batch_compilation.preq_types_with_extra_info (qarent cs) ;; 
   

exception Find_subdir_from_suffix_exn of string * (Dfa_subdirectory_t.t list) ;;

let find_subdir_from_suffix cs possibly_slashed_suffix =
  let suffix = Cull_string.trim_slashes_on_the_right possibly_slashed_suffix  in
  let temp1 = List.filter (
    fun subdir -> Supstring.contains (Dfa_subdirectory.without_trailing_slash subdir) suffix
  ) (all_subdirectories cs) in 
  let test_for_minimality = (fun subdir1->
     List.for_all (fun subdir2 ->
        if subdir2 = subdir1 then true else 
        not(Dfa_subdirectory.begins_with subdir1 subdir2) 
     ) temp1
  ) in 
  let temp2 = List.filter test_for_minimality temp1 in 
  if List.length(temp2)<>1
  then raise(Find_subdir_from_suffix_exn(suffix,temp2))
  else let (Dfa_subdirectory_t.SD container) = List.hd temp2 in 
       let j1 = Substring.leftmost_index_of_in suffix container in 
       let j2 = j1 + (String.length suffix) -1 in 
       Dfa_subdirectory.of_line(Cull_string.beginning j2 container);;
  
let compute_long_subdir_name cs old_subdir new_subdir_short_name =
   let temp1 =  Cull_string.trim_slashes_on_the_right new_subdir_short_name in
   let long_name = (
   if String.contains temp1 '/'
   then temp1 
   else let old_subdir_name = Dfa_subdirectory.without_trailing_slash old_subdir in 
        let father_name = Cull_string.before_rightmost old_subdir_name '/' in 
        if father_name = ""
        then temp1
        else father_name^"/"^temp1 ) in 
   Dfa_subdirectory.of_line long_name ;;       


let find_needed_data_for_file cs fn=
      let temp1=Look_for_module_names.names_in_mlx_file fn in
      List.filter (
         fun mn->List.mem mn temp1  
      )(dep_ordered_modules cs);;

let  find_needed_data cs rless=
   let full_version = Dfn_join.root_to_rootless (root cs) rless in 
   let fn=Dfn_full.to_absolute_path full_version in
      find_needed_data_for_file cs fn;;    

let needed_dirs_and_libs_in_command cmod cs mn=
   let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
   let s_root=Dfa_root.connectable_to_subpath(root cs) in
   let dirs=
   "-I "^s_root^(Dfa_subdirectory.connectable_to_subpath(Compilation_mode.workspace cmod))
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    (needed_libs_for_module cs mn)) in
    String.concat " " ["";dirs;libs;""];;

let all_endinglesses cs=
  Image.image (endingless_at_module cs) (dep_ordered_modules cs);; 

let get_modification_time cs mn edg=
  if edg=principal_ending_for_module cs mn then principal_mt_for_module cs mn else 
  if edg=Dfa_ocaml_ending_t.Mli then mli_mt_for_module cs mn else 
  "0.";;

(* exception Non_existent_mtime of Dfn_full_t.t;; *)



 
exception Derelict_children of Dfa_module_t.t list;;  
           
let unregister_modules cs elesses=
  let nms= Image.image Dfn_endingless.to_module elesses in
  let descendants=List.filter(
      fun mn-> List.exists(fun mn2->
       List.mem mn2 ( ancestors_for_module cs mn ) ) nms
  ) (dep_ordered_modules cs) in
  let problematic_descendants=List.filter(
      fun mn-> not(List.mem mn nms)
  ) descendants in
   if problematic_descendants<>[]
   then raise(Derelict_children(problematic_descendants))
   else
   let cs2=List.fold_left Automatic.remove_in_each_at_module cs nms in
   cs2;;     

let unregister_module cs eless= unregister_modules cs [eless] ;; 
                    
exception Non_registered_file of Dfn_full_t.t;;  
exception Abandoned_children of Dfn_full_t.t * (Dfa_module_t.t list);;
                      
                     
let partially_remove_mlx_file cs mlxfile=
    let nm=Dfn_full.to_module mlxfile in
    let pre_desc=List.filter(
      fun mn7->
      List.mem nm ( ancestors_for_module cs mn7)
    ) (dep_ordered_modules cs) in
    if pre_desc<>[]
    then raise(Abandoned_children(mlxfile,pre_desc))
    else
    let edg=Dfa_ocaml_ending.of_ending(Dfn_full.to_ending mlxfile) in
    if (not(check_ending_in_at_module edg cs nm))
    then raise(Non_registered_file(mlxfile))
    else if check_for_single_ending_at_module cs nm
         then let cs5=Automatic.remove_in_each_at_module cs nm in 
              cs5
         else (* if we get here, there are two registered endings, one of which
              is the mli *) 
              cs ;;
              (*
              if edg=(Dfa_ocaml_ending_t.Mli)
              then (
                       let cs3=set_mli_presence_for_module cs nm false in 
                       set_mli_mt_for_module cs3 nm "0."
                   )
               else 
                     let old_mt=principal_mt_for_module cs nm in
                     (
                      let cs4=set_principal_ending_for_module cs nm (Dfa_ocaml_ending_t.Mli) in 
                      set_principal_mt_for_module cs4 nm old_mt
                    );;
             *) 


let compute_subdirectories_list cs=
  let temp1=Image.image Dfa_subdirectory.without_trailing_slash (all_used_subdirs cs) in
    let temp2=Set_of_strings.sort temp1 in
    let temp3=Set_of_strings.forget_order temp2 in
    Image.image Dfa_subdirectory.of_line temp3;;

let  check_registrations cs eless=
   let mn=Dfn_endingless.to_module eless in 
   Dfa_ending.compute_on_all_ocaml_endings 
      (fun edg->check_ending_in_at_module (Dfa_ocaml_ending.of_ending edg) cs mn);;


module EsterhazyTwo=struct


let find_needed_libraries cs rless ordered_ancestors=
  let full_version=Dfn_join.root_to_rootless (root cs) rless in
  let fn=Dfn_full.to_absolute_path full_version in
  let temp1=Look_for_module_names.names_in_mlx_file fn in
  List.filter
  (
    fun lib->
      if List.exists 
         (fun mdl->List.mem(Dfa_module.of_line mdl)(temp1))
           (Ocaml_library.modules_telling_a_library_away lib)
      then true
      else List.exists 
           (fun mn->
            List.mem lib (needed_libs_for_module cs mn) ) 
           ordered_ancestors
  )
  Ocaml_library.all_libraries;;


let find_needed_directories cs rless ordered_ancestors=
  let temp1=Image.image (fun mn->
    Set_of_polys.sort(needed_dirs_for_module cs mn)) ordered_ancestors in
  let subdir_in_mlx=Dfn_rootless.to_subdirectory rless in
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
      [Dfa_ocaml_ending_t.Ml;Dfa_ocaml_ending_t.Mll;Dfa_ocaml_ending_t.Mly] in
    let temp2=Option.filter_and_unpack (
       fun (bowl,ending)->if bowl then Some(ending) else None 
    ) temp1 in
    if temp2=[] then Dfa_ocaml_ending_t.Mli else List.hd temp2;;

let md_compute_modification_time hm edg=
  let mlx=Dfn_join.to_ending hm (Dfa_ocaml_ending.to_ending edg) in
  let file=Dfn_full.to_line mlx in 
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;

let md_compute_modification_times hm=
      Dfa_ending.compute_on_all_ocaml_endings (fun edg->
        md_compute_modification_time hm (Dfa_ocaml_ending.of_ending edg) );;
    
let md_associated_modification_time  (ml_mt,mli_mt,mll_mt,mly_mt) edg=
  match edg with
     Dfa_ocaml_ending_t.Ml->ml_mt
    |Mli->mli_mt
    |Mll->mll_mt
    |Mly->mly_mt;;  

let complete_info cs  rless=
  let middle = Dfn_rootless.to_middle rless in 
  let hm=Dfn_join.root_to_middle (root cs) middle in
  let modules_written_in_file=find_needed_data cs rless in
  let (mlr,mlir,mllr,mlyr)=check_registrations cs hm
  and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
  let pr_end=compute_principal_ending (mlr,mlir,mllr,mlyr) in
  let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let temp1=Image.image 
          (fun mn->
           Set_of_polys.sort(ancestors_for_module cs mn)) 
          modules_written_in_file in
  let temp2=Set_of_polys.fold_merge ((Set_of_polys.sort(modules_written_in_file) )::temp1) in
  let tempf=(fun mn->
              if Set_of_polys.mem mn temp2
              then Some(mn)
              else None) in
  let allanc=Option.filter_and_unpack tempf (dep_ordered_modules cs) in
  let libned=EsterhazyTwo.find_needed_libraries cs rless modules_written_in_file
  and dirned=EsterhazyTwo.find_needed_directories cs rless modules_written_in_file in
  (hm,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;

let update_just_one_module cs rootless =
    let mn = Dfn_rootless.to_module rootless in 
    if not(List.mem mn (dep_ordered_modules cs))
    then cs 
    else let (_,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=complete_info cs rootless in 
         Automatic.set_in_each cs mn (pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated);;


  let check_unix_presence eless edg=
    let full_path=Dfn_join.to_ending eless edg in 
    Sys.file_exists(Dfn_full.to_line full_path);;

let  check_unix_presences hm=
    Dfa_ending.compute_on_all_ocaml_endings (fun edg->check_unix_presence hm edg);;  

let registrations_for_lonely_ending edg =
    (
      edg=Dfa_ocaml_ending_t.Ml,
      edg=Dfa_ocaml_ending_t.Mli,
      edg=Dfa_ocaml_ending_t.Mll,
      edg=Dfa_ocaml_ending_t.Mly
     );;

     
let complete_id_during_new_module_registration cs rless=
    let middle = Dfn_rootless.to_middle rless in 
    let eless=Dfn_join.root_to_middle (root cs) middle 
    and edg=Dfa_ocaml_ending.of_ending(Dfn_rootless.to_ending rless) in
    let modules_written_in_file=find_needed_data cs rless in
    let (mlp,mlir,mllr,mlyr)=registrations_for_lonely_ending edg
    and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times eless in
    let pr_end=edg in
    let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let temp1=Image.image 
          (fun mn->
           Set_of_polys.sort(ancestors_for_module cs mn)) 
          modules_written_in_file in
    let temp2=Set_of_polys.fold_merge ((Set_of_polys.sort(modules_written_in_file) )::temp1) in
    let tempf=(fun mn->
              if Set_of_polys.mem mn temp2
              then Some(mn)
              else None) in
    let allanc=Option.filter_and_unpack tempf (dep_ordered_modules cs) in
    let libned=EsterhazyTwo.find_needed_libraries cs rless modules_written_in_file
    and dirned=EsterhazyTwo.find_needed_directories cs rless modules_written_in_file in
    (eless,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;
    

let above cs eless=
  let nm=Dfn_endingless.to_module eless in
  ancestors_for_module cs nm;;
 
let below_module cs mn0 =
  List.filter(fun mn->List.mem mn0 (ancestors_for_module cs mn)) (dep_ordered_modules cs);; 
   

let directly_above cs eless=
    let nm=Dfn_endingless.to_module eless in
     direct_fathers_for_module cs nm;;           

let ordered_as_in_coma_state cs l=
   List.filter (fun x->List.mem x l) (dep_ordered_modules cs);;

let above_one_in_several_or_inside cs l=
  let temp1=Image.image (ancestors_for_module cs) l in
  let temp2=List.flatten (l::temp1) in
  ordered_as_in_coma_state cs  temp2;;

let acolytes_above_module cs mn =
   let temp2 = Image.image (rootless_paths_at_module cs) (ancestors_for_module cs mn) in 
   List.flatten temp2 ;; 

let all_mlx_files cs=
  let mods=dep_ordered_modules cs in
  List.flatten(Image.image(acolytes_at_module cs) mods);;                
      
let all_mlx_paths cs=Image.image Dfn_full.to_absolute_path 
        (all_mlx_files cs);;  

let all_rootless_paths cs=
    let mods=dep_ordered_modules cs in
    List.flatten(Image.image(rootless_lines_at_module cs) mods);;  
     

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


let number_of_modules fw = Fw_with_batch_compilation.number_of_modules (qarent fw) ;;

exception Inconsistent_constraints of Dfa_module_t.t*Dfa_module_t.t;;
exception Bad_upper_constraint of Dfa_module_t.t;;  


exception Nonregistered_module_during_reposition of Dfn_endingless_t.t;;  

 
let reposition_module cs eless (l_before,l_after)=
    let l_mods = dep_ordered_modules cs in 
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
    Automatic.reposition_in_each cs pivot eless;;  

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
  if not(check_ending_in_at_module Dfa_ocaml_ending_t.Ml cs mn)
  then None
  else 
  let hm=endingless_at_module cs mn in
  let mlx=Dfn_join.to_ending hm Dfa_ending.ml in
  Some(Dfn_full.to_absolute_path mlx)
) (dep_ordered_modules cs);;





     
let md_recompute_modification_time eless edg=
  let mlx=Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending edg) in
  let file=Dfn_full.to_line mlx in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;
  

let check_for_possible_change cs mn=
  let eless =endingless_at_module cs mn 
  and pr_ending=principal_ending_for_module cs mn in
  let mli_modif_time=md_recompute_modification_time eless Dfa_ocaml_ending_t.Mli 
  and pr_modif_time=md_recompute_modification_time eless pr_ending 
  and old_mli_modif_time=mli_mt_for_module cs mn
  and old_pr_modif_time=principal_mt_for_module cs mn 
  in
  let mn = Dfn_endingless.to_module eless in 
  let no_change_for_mlis =(
     if not(mli_presence_for_module cs mn)
     then true 
    else   mli_modif_time = old_mli_modif_time
  ) in 
  if no_change_for_mlis&&(pr_modif_time=old_pr_modif_time)&&(last_compilation_result_for_module cs mn)
  then None
  else
  let rless=Dfn_full.to_rootless(Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending pr_ending)) in
  let direct_fathers=find_needed_data cs rless in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;
  


let printer_equipped_types_from_data cs=
  Option.filter_and_unpack (
    fun mn->
    let eless=endingless_at_module cs mn
    and pr_end=principal_ending_for_module cs mn in
    let mlx=Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending pr_end) in
    let ap=Dfn_full.to_absolute_path mlx in
    let text=Io.read_whole_file ap in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(eless)
    else None
  ) (dep_ordered_modules cs);;
 



exception Already_registered_file of Dfn_rootless_t.t;;  
exception Overcrowding of Dfn_rootless_t.t*(Dfa_ocaml_ending_t.t list);;
exception Bad_pair of Dfn_rootless_t.t*Dfa_ocaml_ending_t.t;; 


let register_mlx_file_on_monitored_modules cs rless =
  let middle = Dfn_rootless.to_middle rless
  and ending=Dfa_ocaml_ending.of_ending(Dfn_rootless.to_ending rless) in 
  let nm=Dfn_rootless.to_module rless in
  if not(Automatic.test_module_for_registration cs nm)
  then  let info=complete_id_during_new_module_registration cs rless in
                Automatic.push_right_in_each cs info 
  else
  let edgs=registered_endings_at_module cs nm in
  if List.length(edgs)>1
  then  raise(Overcrowding(rless,edgs))
  else  
  if List.mem ending edgs
  then raise(Already_registered_file(rless))
  else
  if (not(List.mem Dfa_ocaml_ending_t.Mli (ending::edgs)))
  then raise(Bad_pair(rless,List.hd edgs))
  else 
  if ending = Dfa_ocaml_ending_t.Mli
  then let old_pr_end = List.hd edgs in
       let old_rless =
         Dfn_join.middle_to_ending middle (Dfa_ocaml_ending.to_ending old_pr_end) in
        let (eless,_,old_mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=
                 complete_info cs old_rless in
        let new_mlimt = md_compute_modification_time eless ending in
        let new_dt=(old_pr_end,true,prmt,new_mlimt,libned,dirfath,allanc,dirned,false) in
        Automatic.set_in_each cs nm new_dt
  else
  let new_dt=complete_id_during_new_module_registration cs rless in 
  let (_,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=new_dt in
  let temp3=List.rev(dirfath) in
  if temp3=[]
  then Automatic.set_in_each cs nm (pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated) 
  else  cs ;;



exception FileWithDependencies of 
Dfn_full_t.t*(Dfa_module_t.t list);;

*)

let read_persistent_version x=
  let full_path=Dfn_join.root_to_rootless (root x)  Coma_constant.rootless_path_for_targetfile in
  let ap= Dfn_full.to_absolute_path full_path in
  let the_archive=Io.read_whole_file ap in
  let archived_object = Crobj_parsing.parse the_archive in 
  of_concrete_object archived_object;;      

(*

module Try_to_register=struct

  let mlx_file cs mlx_file=
    try(Some(register_mlx_file_on_monitored_modules 
        cs mlx_file)) with _->None;;  

module Esterhazy=struct

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
   Esterhazy.iterator(cs,[],mlx_files);;
 

end;;  




let decipher_path cs x=Find_suitable_ending.find_file_location 
   (root cs) (all_subdirectories cs) x;;

let forgotten_files_in_build_subdir cs= 
   let s_root=Dfa_root.connectable_to_subpath (root cs) 
   and s_build=Dfa_subdirectory.connectable_to_subpath (Coma_constant.usual_build_subdir) in 
   let dir=Directory_name.of_string (s_root^s_build) in 
   let temp1=More_unix.beheaded_simple_ls dir in 
   List.filter (
      fun s->
       let s_mn=Cull_string.before_rightmost_possibly_all s '.' in 
       let mn=Dfa_module.of_line s_mn in 
       not(Automatic.test_module_for_registration cs mn)
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
   let pre_temp2=(ancestors_for_module cs nm)@[nm] in
   let temp2=Image.image (endingless_at_module cs) pre_temp2 in
   let preceding_files=Image.image  (fun eless2->
   	 Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
   ) temp2 in
   Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name path;;



end;; 

exception Module_already_exists of string;;

let duplicate_module cs old_t1 old_t2=
   let t1=String.uncapitalize_ascii old_t1
   and t2=String.uncapitalize_ascii old_t2 in 
   let ap1=decipher_path cs t1 in
   let s_ap1=Absolute_path.to_string ap1 in
   let s_ending = Cull_string.after_rightmost s_ap1 '.' in 
   let s_ap2=(Cull_string.before_rightmost_possibly_all s_ap1 '/')^"/"^t2^"."^s_ending in
   if Sys.file_exists s_ap2
   then raise(Module_already_exists(t2))
   else 
   let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Absolute_path.of_string s_ap2 in
   let _ =  (
     if s_ending = "ml"
     then Put_use_directive_in_initial_comment.put_usual (root cs) ap2) in 
   Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;             


let test_for_foreign root ap =
   match (
     try Some(Dfn_common.decompose_absolute_path_using_root ap root) with 
              _->None 
   ) with 
   None -> true 
   |Some(rootless) ->
      (
       not(List.mem
          (Dfn_rootless.to_ending rootless) Dfa_ending.endings_for_readable_files)   
      )
      ;;

let census_of_foreigners cs=
   let config = Fw_with_dependencies.configuration (cs.Coma_state_t.frontier_with_unix_world) in 
   let  the_root = config.Fw_configuration_t.root in 
   let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
   let (list1,_) = More_unix.complete_ls_with_ignored_subdirs the_dir config.Fw_configuration_t.ignored_subdirectories false in 
   List.filter (test_for_foreign the_root) list1;;

let check_module_sequence_for_forgettability cs l=
  let modules_below = List.filter (
    fun mn -> List.exists (fun mn2->
        List.mem mn2 (ancestors_for_module cs mn)
      ) l 
  )(dep_ordered_modules cs) in 
  List.filter (fun mn->not(List.mem mn l)) modules_below;;


let check_rootless_path_sequence_for_forgettability cs old_l =
  (* if there are several rootlesses corresponding to the same module, 
    because of our conventions, there are two of them and one of them is a mli. 
    So any one of the two can be deleted without harming the other
    *)
  let possibly_redundant = Option.filter_and_unpack (fun rl->
    if not(Dfn_rootless.is_compilable rl) then None else
    Some(Dfn_rootless.to_module rl)  ) old_l in 
  let l = Listennou.nonredundant_version   possibly_redundant in 
  check_module_sequence_for_forgettability cs l ;;

 


let principal_acolyte cs eless = 
  let mn = Dfn_endingless.to_module eless in 
  let edg = principal_ending_for_module cs mn in 
  Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending edg) ;;

let all_principals cs =
    Image.image (principal_acolyte cs) (all_endinglesses cs) ;;  

exception Module_not_found_while_choosing_automatic of Dfa_module_t.t ;;

let choose_automatic_if_possible cs modulename =
    let mname = Dfa_module.to_line modulename 
    and list_of_modules = Automatic.dep_ordered_modules cs in 
    let auto_version = Dfa_module.of_line(mname^"_automatic") in 
    if not(List.mem modulename list_of_modules)
    then raise(Module_not_found_while_choosing_automatic modulename)
    else 
    if List.mem auto_version list_of_modules
    then auto_version
    else modulename ;;      
*)

end ;; 


let check_that_no_change_has_occurred cs =
  Fw_with_batch_compilation.check_that_no_change_has_occurred (Private.qarent cs) ;; 
let clean_debug_dir cs = Fw_with_batch_compilation.clean_debug_dir (Private.qarent cs) ;;
let clean_exec_dir cs = Fw_with_batch_compilation.clean_exec_dir (Private.qarent cs) ;;
let configuration cs= Fw_with_batch_compilation.configuration (Private.qarent cs) ;;
let forget_modules cs mods = 
  let new_parent = Fw_with_batch_compilation.forget_modules (Private.qarent cs) mods in 
  Private.tneraq new_parent ;; 
let gitpush_after_backup cs=(configuration cs).Fw_configuration_t.gitpush_after_backup;;   
let inspect_and_update cs  = 
    let (new_parent,changed_usual_compilables) = Fw_with_batch_compilation.inspect_and_update (Private.qarent cs)  in 
    (Private.tneraq new_parent,changed_usual_compilables) ;;   
let latest_changes fw = Fw_with_batch_compilation.latest_changes (Private.qarent fw)  ;;      
let list_values_from_module cs mn = 
  Fw_with_batch_compilation.list_values_from_module  (Private.qarent cs) mn ;;
let modern_recompile cs changed_modules_in_any_order = 
  let new_parent = Fw_with_batch_compilation.modern_recompile (Private.qarent cs) changed_modules_in_any_order in 
  Private.tneraq new_parent ;; 
let modules_using_value cs module_name =
    Fw_with_batch_compilation.modules_using_value (Private.qarent cs) module_name ;;  
let number_of_modules fw = Fw_with_batch_compilation.number_of_modules (Private.qarent fw) ;;    
let of_configuration config = 
    let new_parent = Fw_with_batch_compilation.of_configuration config in 
    Private.tneraq new_parent ;;   
let of_concrete_object = Private.of_concrete_object ;;    
let preq_types_with_extra_info cs = 
  Fw_with_batch_compilation.preq_types_with_extra_info (Private.qarent cs) ;; 
let read_persistent_version = Private.read_persistent_version ;;
let reflect_latest_changes_in_github cs opt_msg=
    let new_parent = Fw_with_batch_compilation.reflect_latest_changes_in_github (Private.qarent cs) opt_msg in 
    Private.tneraq new_parent ;;   
let register_rootless_paths cs mod_names = 
    let new_parent = Fw_with_batch_compilation.register_rootless_paths (Private.qarent cs) mod_names in 
    Private.tneraq new_parent ;;      
let relocate_module_to cs mod_name new_subdir = 
    let new_parent = Fw_with_batch_compilation.relocate_module_to (Private.qarent cs) mod_name new_subdir in 
    Private.tneraq new_parent ;;  
let remove_files cs rps = 
    let new_parent = Fw_with_batch_compilation.remove_files (Private.qarent cs) rps in 
    Private.tneraq new_parent ;;        
let rename_module cs old_middle_name new_nonslashed_name = 
    let (new_parent,extra) = Fw_with_batch_compilation.rename_module (Private.qarent cs) old_middle_name new_nonslashed_name in 
    (Private.tneraq new_parent,extra) ;;   
let rename_string_or_value cs old_sov new_sov = 
    let (new_parent,changed_modules_in_any_order) = Fw_with_batch_compilation.rename_string_or_value (Private.qarent cs) old_sov new_sov  in 
    (Private.tneraq new_parent,changed_modules_in_any_order) ;; 
let rename_subdirectory_as cs (old_subdir,new_subdir) = 
    let new_parent = Fw_with_batch_compilation.rename_subdirectory_as (Private.qarent cs) (old_subdir,new_subdir) in 
    Private.tneraq new_parent ;;      
let root = Private.root ;;
let set_gitpush_after_backup cs bowl = 
  let new_parent = Fw_with_batch_compilation.set_gitpush_after_backup (Private.qarent cs) bowl in 
  Private.tneraq new_parent ;;   
let show_value_occurrences cs t = 
  Fw_with_batch_compilation.show_value_occurrences  (Private.qarent cs) t ;;  
let start_debugging cs = Fw_with_batch_compilation.start_debugging (Private.qarent cs) ;; 
let start_executing cs short_path = Fw_with_batch_compilation.start_executing (Private.qarent cs) short_path;;  
let to_concrete_object = Private.to_concrete_object ;;
let up_to_date_elesses cs = 
  Fw_with_batch_compilation.up_to_date_elesses (Private.qarent cs) ;; 