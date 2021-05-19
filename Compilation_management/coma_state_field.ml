(* 

#use"Compilation_management/coma_state_field.ml";;

Here, we put soma wrappers so that all direct manipulations of
the Coma_state_t.t datatype should be done here.

*)

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

let frontier_with_unix_world cs = (of_t cs).Coma_state_t.frontier_with_unix_world;;
let configuration cs=(frontier_with_unix_world cs).Fw_wrapper_t.configuration;;
let root cs= Fw_configuration.root (configuration cs);;
let backup_dir cs=(configuration cs).Fw_configuration_t.dir_for_backup;;
let gitpush_after_backup cs=(configuration cs).Fw_configuration_t.gitpush_after_backup;;   
let github_url cs=(configuration cs).Fw_configuration_t.github_url;;
let confidential_files cs=(configuration cs).Fw_configuration_t.confidential_files;;


let subdir_at_module cs mn=
   try List.assoc mn ( (of_t cs).Coma_state_t.subdir_for_module) with 
   _ -> raise(Module_not_found(mn));;

let principal_ending_at_module cs mn=
   try List.assoc mn ( (of_t cs).Coma_state_t.principal_ending_for_module) with 
   _ -> raise(Module_not_found(mn));;

let mli_presence_at_module cs mn=
   try List.assoc mn ( (of_t cs).Coma_state_t.mli_presence_for_module) with 
   _ -> raise(Module_not_found(mn));;

let principal_mt_at_module cs mn=
   try List.assoc mn ( (of_t cs).Coma_state_t.principal_mt_for_module) with 
   _ -> raise(Module_not_found(mn));;

let mli_mt_at_module cs mn=
    try List.assoc mn ( (of_t cs).Coma_state_t.mli_mt_for_module) with 
    _ -> raise(Module_not_found(mn));;

let needed_libs_at_module cs mn=
    try List.assoc mn ( (of_t cs).Coma_state_t.needed_libs_for_module) with 
    _ -> raise(Module_not_found(mn));;

let direct_fathers_at_module cs mn=
    try  List.assoc mn ( (of_t cs).Coma_state_t.direct_fathers_for_module) with 
    _ -> raise(Module_not_found(mn));;

let ancestors_at_module cs mn=
    try  List.assoc mn ( (of_t cs).Coma_state_t.ancestors_for_module) with 
    _ -> raise(Module_not_found(mn));;

let needed_dirs_at_module cs mn=
    try  List.assoc mn  ((of_t cs).Coma_state_t.needed_dirs_for_module) with 
    _ -> raise(Module_not_found(mn));;

let product_up_to_date_at_module cs mn=
   try  List.assoc mn ((of_t cs).Coma_state_t.product_up_to_date_for_module) with     
   _ -> raise(Module_not_found(mn));;

let directories cs=(of_t cs).Coma_state_t.directories;;
let preq_types cs=(of_t cs).Coma_state_t.printer_equipped_types;;



let ordered_list_of_modules cs=((of_t cs).Coma_state_t.modules);; 

let test_module_for_registration cs modname=
  List.mem modname (ordered_list_of_modules cs);;

let follows_it_but_does_not_necessarily_depend_on_it cs mn=
    let (_,_,after) = Three_parts.select_center_element_and_reverse_left (fun x->x=mn)
      (ordered_list_of_modules cs) in 
    after;;


let all_used_subdirs cs =
   let current_assoc = (of_t cs).Coma_state_t.subdir_for_module in 
   Image.image snd current_assoc ;;



(* Setters  *)

let set_frontier_with_unix_world cs v= 
   let ccs=of_t cs in 
   to_t({ccs with Coma_state_t.frontier_with_unix_world=v});;


let set_push_after_backup cs bowl = let ccs=of_t cs in 
     let old_frontier = ccs.Coma_state_t.frontier_with_unix_world in 
     let old_config = old_frontier.Fw_wrapper_t.configuration in 
     let new_config = {old_config with Fw_configuration_t.gitpush_after_backup=bowl } in 
     let new_frontier = {old_frontier with Fw_wrapper_t.configuration = new_config} in 
     to_t({ccs with Coma_state_t.frontier_with_unix_world=new_frontier });;



let set_subdir_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.subdir_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.subdir_for_module=new_assocs });;
    

let set_principal_ending_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.principal_ending_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.principal_ending_for_module=new_assocs });;


let set_mli_presence_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.mli_presence_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.mli_presence_for_module=new_assocs });;


let set_principal_mt_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.principal_mt_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.principal_mt_for_module=new_assocs });;

let set_mli_mt_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.mli_mt_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.mli_mt_for_module=new_assocs });;

let set_needed_libs_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.needed_libs_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.needed_libs_for_module=new_assocs });;


let set_direct_fathers_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.direct_fathers_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.direct_fathers_for_module=new_assocs });;



let set_ancestors_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.ancestors_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.ancestors_for_module=new_assocs });;


let set_needed_dirs_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.needed_dirs_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.needed_dirs_for_module=new_assocs });;
    


let set_product_up_to_date_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Coma_state_t.product_up_to_date_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.product_up_to_date_for_module=new_assocs });;
    


let set_directories cs v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.directories=v});;


let set_preq_types cs v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.printer_equipped_types=v});;


(* Adhoc setters *)

exception Impose_last_change_exn of Dircopy_diff_t.t ;;

let impose_last_changes cs diff =
   let old_fw = frontier_with_unix_world cs in 
   let old_diff = old_fw.Fw_wrapper_t.last_noticed_changes in 
   if not(Dircopy_diff.is_empty old_diff)
   then raise(Impose_last_change_exn(old_diff))
   else 
   let new_fw = {
       old_fw with 
       Fw_wrapper_t.last_noticed_changes = diff 
   } in 
   set_frontier_with_unix_world cs new_fw ;;

let modify_all_subdirs cs f =
   let ccs=of_t cs in 
   let old_subdirs = ((of_t cs).Coma_state_t.subdir_for_module) in 
   let new_subdirs = Image.image (fun (key,vaal)->(key,f vaal)) old_subdirs in 
   to_t({ccs with Coma_state_t.subdir_for_module= new_subdirs });;

let modify_all_needed_dirs cs f =
   let ccs=of_t cs in 
   let old_needed_dirs = ((of_t cs).Coma_state_t.needed_dirs_for_module) in 
   let new_needed_dirs = Image.image (fun (key,vaal)->(key,Image.image f vaal)) old_needed_dirs in 
   to_t({ccs with Coma_state_t.needed_dirs_for_module= new_needed_dirs });;

(* End of adhoc setters *)



let empty_one config=
    to_t({
     Coma_state_t.frontier_with_unix_world= Fw_wrapper.empty_one config;
     modules = [];
     subdir_for_module = [] ;
     principal_ending_for_module = [] ;
     mli_presence_for_module = [] ;
     principal_mt_for_module = [] ;
     mli_mt_for_module = [] ;
     needed_libs_for_module = [] ;
     direct_fathers_for_module = [];
     ancestors_for_module = [] ; 
     needed_dirs_for_module = [];
     product_up_to_date_for_module = [];
     directories =[];
     printer_equipped_types =[];
});;


let change_one_module_name wrapped_cs old_mn new_mn=
    (* note that preq_types are not dealt with here *)
    let cs=of_t wrapped_cs in
    let new_modules = Image.image (fun x->if x=old_mn then new_mn else x)(ordered_list_of_modules cs) in  
    let rep_pair = (old_mn,new_mn) in 
    let new_subdirs = Associative_list.change_name_for_key (cs.Coma_state_t.subdir_for_module) rep_pair
    and new_principal_endings = Associative_list.change_name_for_key (cs.Coma_state_t.principal_ending_for_module) rep_pair
    and new_mli_presences = Associative_list.change_name_for_key (cs.Coma_state_t.mli_presence_for_module) rep_pair
    and new_principal_mts = Associative_list.change_name_for_key (cs.Coma_state_t.principal_mt_for_module) rep_pair
    and new_mli_mts = Associative_list.change_name_for_key (cs.Coma_state_t.mli_mt_for_module) rep_pair
    and new_needed_libs = Associative_list.change_name_for_key (cs.Coma_state_t.needed_libs_for_module) rep_pair
    and new_direct_fathers = Associative_list.change_name_for_key (cs.Coma_state_t.direct_fathers_for_module) rep_pair
    and new_ancestors = Associative_list.change_name_for_key (cs.Coma_state_t.ancestors_for_module) rep_pair
    and new_needed_dirs = Associative_list.change_name_for_key (cs.Coma_state_t.needed_dirs_for_module) rep_pair  
    and new_products_up_to_date = Associative_list.change_name_for_key  cs.Coma_state_t.product_up_to_date_for_module rep_pair  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module=  new_subdirs;
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module= new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;

let remove_in_each_at_module wrapped_cs mname=
    let cs=of_t wrapped_cs in
    let new_modules = List.filter (fun x->x<>mname) (ordered_list_of_modules cs) 
    and new_subdirs = Associative_list.remove_key (cs.Coma_state_t.subdir_for_module) mname
    and new_principal_endings = Associative_list.remove_key (cs.Coma_state_t.principal_ending_for_module) mname
    and new_mli_presences = Associative_list.remove_key (cs.Coma_state_t.mli_presence_for_module) mname
    and new_principal_mts = Associative_list.remove_key (cs.Coma_state_t.principal_mt_for_module) mname
    and new_mli_mts = Associative_list.remove_key (cs.Coma_state_t.mli_mt_for_module) mname
    and new_needed_libs = Associative_list.remove_key (cs.Coma_state_t.needed_libs_for_module) mname
    and new_direct_fathers = Associative_list.remove_key (cs.Coma_state_t.direct_fathers_for_module) mname
    and new_ancestors = Associative_list.remove_key (cs.Coma_state_t.ancestors_for_module) mname
    and new_needed_dirs = Associative_list.remove_key (cs.Coma_state_t.needed_dirs_for_module) mname  
    and new_products_up_to_date = Associative_list.remove_key  cs.Coma_state_t.product_up_to_date_for_module mname  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module=  new_subdirs;
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module= new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;



let push_right_in_each wrapped_cs (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Dfn_endingless.to_module hm
    and subdir=Dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = (cs.Coma_state_t.modules)@[nm] 
    and new_subdirs = (  cs.Coma_state_t.subdir_for_module) @[nm,subdir]
    and new_principal_endings = (  cs.Coma_state_t.principal_ending_for_module) @[nm,pr_end] 
    and new_mli_presences = (  cs.Coma_state_t.mli_presence_for_module) @[nm,mlip] 
    and new_principal_mts = (  cs.Coma_state_t.principal_mt_for_module) @[nm,prmt] 
    and new_mli_mts = (  cs.Coma_state_t.mli_mt_for_module) @[nm,mlimt] 
    and new_needed_libs = (  cs.Coma_state_t.needed_libs_for_module) @[nm,libned] 
    and new_direct_fathers = (  cs.Coma_state_t.direct_fathers_for_module) @[nm,dirfath]
    and new_ancestors = (  cs.Coma_state_t.ancestors_for_module) @[nm,allanc] 
    and new_needed_dirs = (cs.Coma_state_t.needed_dirs_for_module)@[nm,dirned] 
    and new_products_up_to_date = (cs.Coma_state_t.product_up_to_date_for_module)@[nm,upy]  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module=  new_subdirs;
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;

let set_in_each wrapped_cs nm (pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let cs=of_t wrapped_cs in
    let new_principal_endings = Associative_list.change_value_for_key (cs.Coma_state_t.principal_ending_for_module) (nm,pr_end) 
    and new_mli_presences = Associative_list.change_value_for_key (cs.Coma_state_t.mli_presence_for_module) (nm, mlip) 
    and new_principal_mts = Associative_list.change_value_for_key (cs.Coma_state_t.principal_mt_for_module) (nm,prmt) 
    and new_mli_mts = Associative_list.change_value_for_key (cs.Coma_state_t.mli_mt_for_module) (nm,mlimt) 
    and new_needed_libs = Associative_list.change_value_for_key (cs.Coma_state_t.needed_libs_for_module) (nm,libned) 
    and new_direct_fathers = Associative_list.change_value_for_key (cs.Coma_state_t.direct_fathers_for_module) (nm,dirfath)
    and new_ancestors = Associative_list.change_value_for_key (cs.Coma_state_t.ancestors_for_module) (nm,allanc) 
    and new_needed_dirs = Associative_list.change_value_for_key (cs.Coma_state_t.needed_dirs_for_module) (nm,dirned) 
    and new_products_up_to_date = Associative_list.change_value_for_key  cs.Coma_state_t.product_up_to_date_for_module (nm,upy)  in 
to_t({ cs with 
      (* the "module" and "subdir" fields are not changed *)
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;
  

    
let reposition_in_each wrapped_cs mn1 mn2=
    let cs=of_t wrapped_cs in
    let l_rep=(fun l->Associative_list.reposition_by_putting_snd_immediately_after_fst l mn1 mn2 ) in 
    let new_modules = Listennou.reposition_by_putting_snd_immediately_after_fst (ordered_list_of_modules cs) mn1 mn2 
    and new_subdirs = l_rep (cs.Coma_state_t.subdir_for_module) 
    and new_principal_endings = l_rep (cs.Coma_state_t.principal_ending_for_module) 
    and new_mli_presences = l_rep (cs.Coma_state_t.mli_presence_for_module) 
    and new_principal_mts = l_rep (cs.Coma_state_t.principal_mt_for_module) 
    and new_mli_mts = l_rep (cs.Coma_state_t.mli_mt_for_module) 
    and new_needed_libs = l_rep (cs.Coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = l_rep (cs.Coma_state_t.direct_fathers_for_module) 
    and new_ancestors = l_rep (cs.Coma_state_t.ancestors_for_module) 
    and new_needed_dirs = l_rep (cs.Coma_state_t.needed_dirs_for_module)
    and new_products_up_to_date = l_rep cs.Coma_state_t.product_up_to_date_for_module in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module=  new_subdirs;
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;


let reorder wrapped_cs reordered_list_of_modules =
     let cs=of_t wrapped_cs in 
    let l_rep =(fun l->Associative_list.reorder l reordered_list_of_modules) in    
    let new_subdirs = l_rep (cs.Coma_state_t.subdir_for_module) 
    and new_principal_endings = l_rep (cs.Coma_state_t.principal_ending_for_module) 
    and new_mli_presences = l_rep (cs.Coma_state_t.mli_presence_for_module) 
    and new_principal_mts = l_rep (cs.Coma_state_t.principal_mt_for_module) 
    and new_mli_mts = l_rep (cs.Coma_state_t.mli_mt_for_module) 
    and new_needed_libs = l_rep (cs.Coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = l_rep (cs.Coma_state_t.direct_fathers_for_module) 
    and new_ancestors = l_rep (cs.Coma_state_t.ancestors_for_module) 
    and new_needed_dirs = l_rep (cs.Coma_state_t.needed_dirs_for_module) 
    and new_products_up_to_date = l_rep cs.Coma_state_t.product_up_to_date_for_module  in 
to_t({ cs with 
      Coma_state_t.modules = reordered_list_of_modules;
      Coma_state_t.subdir_for_module=  new_subdirs;
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;  

(* For debugging purposes *)

let sizes wrapped_cs =
    let cs=of_t wrapped_cs in
    [ 
      ["modules",List.length(cs.Coma_state_t.modules)];
      ["subdirs",List.length(cs.Coma_state_t.subdir_for_module)];
      ["pr_endings",List.length(cs.Coma_state_t.principal_ending_for_module)];
      ["mlis",List.length(cs.Coma_state_t.mli_presence_for_module)];
      ["mod_times",List.length(cs.Coma_state_t.principal_mt_for_module)];
      ["mli_mod_times",List.length(cs.Coma_state_t.mli_mt_for_module)];
      ["needed_libs",List.length(cs.Coma_state_t.needed_libs_for_module)];
      ["fathers",List.length(cs.Coma_state_t.direct_fathers_for_module)];
      ["ancestors",List.length(cs.Coma_state_t.ancestors_for_module)];
      ["needed_dirs",List.length(cs.Coma_state_t.needed_dirs_for_module)];
      ["datechecks",List.length(cs.Coma_state_t.product_up_to_date_for_module)];
  ];;


let push_after_module_in_each wrapped_cs pivot (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Dfn_endingless.to_module hm
    and subdir=Dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Listennou.push_immediately_after (ordered_list_of_modules cs) nm  pivot 
    and new_subdirs = Associative_list.push_immediately_after (cs.Coma_state_t.subdir_for_module) (nm,subdir) pivot 
    and new_principal_endings = Associative_list.push_immediately_after (cs.Coma_state_t.principal_ending_for_module) (nm,pr_end) pivot 
    and new_mli_presences = Associative_list.push_immediately_after (cs.Coma_state_t.mli_presence_for_module) (nm,mlip) pivot 
    and new_principal_mts = Associative_list.push_immediately_after (cs.Coma_state_t.principal_mt_for_module) (nm,prmt) pivot 
    and new_mli_mts = Associative_list.push_immediately_after (cs.Coma_state_t.mli_mt_for_module) (nm,mlimt) pivot 
    and new_needed_libs = Associative_list.push_immediately_after (cs.Coma_state_t.needed_libs_for_module) (nm,libned) pivot 
    and new_direct_fathers = Associative_list.push_immediately_after (cs.Coma_state_t.direct_fathers_for_module) (nm,dirfath) pivot 
    and new_ancestors = Associative_list.push_immediately_after (cs.Coma_state_t.ancestors_for_module) (nm,allanc) pivot 
    and new_needed_dirs = Associative_list.push_immediately_after (cs.Coma_state_t.needed_dirs_for_module) (nm,dirned) pivot
    and new_products_up_to_date = Associative_list.push_immediately_after cs.Coma_state_t.product_up_to_date_for_module (nm,upy) pivot  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module =  new_subdirs;
      Coma_state_t.principal_ending_for_module =  new_principal_endings;
      Coma_state_t.mli_presence_for_module =  new_mli_presences;
      Coma_state_t.principal_mt_for_module =  new_principal_mts;
      Coma_state_t.mli_mt_for_module =  new_mli_mts;
      Coma_state_t.needed_libs_for_module =  new_needed_libs;
      Coma_state_t.direct_fathers_for_module =  new_direct_fathers;
      Coma_state_t.ancestors_for_module =  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;
    
let endingless_at_module cs mn=
   Dfn_endingless_t.J(
        root cs,
        subdir_at_module cs mn,
        mn
    );;

let printer_equipped_types_from_preceding_data  
   (frontier_with_unix_world_field,
      modules_field,
        subdir_for_modules_field,
          principal_ending_at_module_field)=
  let the_root = Fw_wrapper_automatic.root frontier_with_unix_world_field in         
  Option.filter_and_unpack (
    fun mn->
    let subdir = List.assoc mn subdir_for_modules_field 
    and pr_end= List.assoc mn principal_ending_at_module_field  in
    let rootless=Dfn_rootless_t.J(subdir,mn,pr_end) in 
    let text=Fw_wrapper_automatic.get_content frontier_with_unix_world_field rootless in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then let eless=Dfn_endingless_t.J(the_root,subdir,mn) in 
         Some(eless)
    else None
  ) modules_field;;    

let restrict wrapped_cs smaller_list_of_modules =
    let cs=of_t wrapped_cs in 
    let restr =(fun l->Associative_list.restrict l smaller_list_of_modules) in    
    let temp_direct_fathers = restr (cs.Coma_state_t.direct_fathers_for_module) 
    and temp_ancestors = restr (cs.Coma_state_t.ancestors_for_module) in 
    let among_fathers =Image.image (fun (key,fathers)->
       (key,List.filter (fun father -> List.mem father smaller_list_of_modules) fathers) ) in 
    let new_subdirs = restr (cs.Coma_state_t.subdir_for_module) 
    and new_principal_endings = restr (cs.Coma_state_t.principal_ending_for_module) 
    and new_mli_presences = restr (cs.Coma_state_t.mli_presence_for_module) 
    and new_principal_mts = restr (cs.Coma_state_t.principal_mt_for_module) 
    and new_mli_mts = restr (cs.Coma_state_t.mli_mt_for_module) 
    and new_needed_libs = restr (cs.Coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = among_fathers  temp_direct_fathers  
    and new_ancestors = among_fathers  temp_ancestors
    and new_needed_dirs = restr (cs.Coma_state_t.needed_dirs_for_module) 
    and new_products_up_to_date = restr cs.Coma_state_t.product_up_to_date_for_module in  
    let new_preq_types= List.filter (
        fun (eless,_)->
          let middle = Dfn_endingless.to_middle eless in 
        List.exists (fun (mn,subdir)->middle = Dfn_middle_t.J(subdir,mn) ) new_subdirs 
        )  cs.Coma_state_t.printer_equipped_types   in 
    let new_directories = Ordered.sort Total_ordering.standard (Image.image snd new_subdirs) in 
to_t({ cs with 
      Coma_state_t.modules = smaller_list_of_modules;
      Coma_state_t.subdir_for_module=  new_subdirs;
      Coma_state_t.principal_ending_for_module=  new_principal_endings;
      Coma_state_t.mli_presence_for_module=  new_mli_presences;
      Coma_state_t.principal_mt_for_module=  new_principal_mts;
      Coma_state_t.mli_mt_for_module=  new_mli_mts;
      Coma_state_t.needed_libs_for_module=  new_needed_libs;
      Coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Coma_state_t.ancestors_for_module=  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
      Coma_state_t.directories = new_directories;
      Coma_state_t.printer_equipped_types = new_preq_types;
});;  

let transplant wrapped_cs new_frontier = 
     let cs=of_t wrapped_cs in 
     let new_principal_mts=Image.image (fun (mn,_)->
          let subdir = List.assoc mn cs.Coma_state_t.subdir_for_module 
          and pr_end = List.assoc mn cs.Coma_state_t.principal_ending_for_module in 
          let rootless = (Dfn_rootless_t.J(subdir,mn,pr_end)) in 
          (mn,Fw_wrapper_automatic.get_mtime new_frontier rootless)
     ) cs.Coma_state_t.principal_ending_for_module
     and new_mli_mts=Image.image (fun (mn,subdir)->
          let rootless = (Dfn_rootless_t.J(subdir,mn,Dfa_ending.mli)) in 
          (mn,Fw_wrapper_automatic.get_mtime_or_zero_if_file_is_nonregistered 
           new_frontier rootless)
     ) cs.Coma_state_t.subdir_for_module 
     and new_products_up_to_date=Image.image (fun (mn,_)->(mn,false)
     ) cs.Coma_state_t.product_up_to_date_for_module
     and new_preq_types=Image.image (fun (eless,_)->(eless,false)
     ) cs.Coma_state_t.printer_equipped_types in 
     to_t({
           cs with    
            Coma_state_t.frontier_with_unix_world= new_frontier;
            principal_mt_for_module = new_principal_mts;
            mli_mt_for_module = new_mli_mts;
            product_up_to_date_for_module = new_products_up_to_date;
            printer_equipped_types = new_preq_types;
     });;

module Private = struct 

let salt = "Coma_"^"state_field.";;

let frontier_with_unix_world_label      = salt ^ "frontier_with_unix_world";;
let dir_for_backup_label                = salt ^ "dir_for_backup";;
let gitpush_after_backup_label          = salt ^ "gitpush_after_backup";;
let modules_label                       = salt ^ "modules";;
let subdir_for_module_label             = salt ^ "subdir_for_module";;
let principal_ending_for_module_label   = salt ^ "principal_ending_for_module";;
let mli_presence_for_module_label       = salt ^ "mli_presence_for_module";;
let principal_mt_for_module_label       = salt ^ "principal_mt_for_module";;
let mli_mt_for_module_label             = salt ^ "mli_mt_for_module";;
let needed_libs_for_module_label        = salt ^ "needed_libs_for_module";;
let direct_fathers_for_module_label     = salt ^ "direct_fathers_for_module";;
let ancestors_for_module_label          = salt ^ "ancestors_for_module";;
let needed_dirs_for_module_label        = salt ^ "needed_dirs_for_module";;
let product_up_to_date_for_module_label = salt ^ "product_up_to_date_for_module";;
let directories_label                   = salt ^ "directories";;
let printer_equipped_types_label        = salt ^ "printer_equipped_types";;

let cr_of_pair f l= Crobj_converter_combinator.of_pair_list  Dfa_module.to_concrete_object f l;;
let cr_to_pair f crobj= Crobj_converter_combinator.to_pair_list  Dfa_module.of_concrete_object f crobj;;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_automatic.get_record ccrt_obj in
   {
      Coma_state_t.frontier_with_unix_world = Fw_wrapper_automatic.of_concrete_object (g frontier_with_unix_world_label);
      modules = Crobj_converter_combinator.to_list Dfa_module.of_concrete_object (g modules_label);
      subdir_for_module = cr_to_pair Dfa_subdirectory.of_concrete_object (g subdir_for_module_label);
      principal_ending_for_module = cr_to_pair Dfa_ending.of_concrete_object (g principal_ending_for_module_label);
      mli_presence_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g mli_presence_for_module_label);
      principal_mt_for_module = cr_to_pair Crobj_converter.string_of_concrete_object (g principal_mt_for_module_label);
      mli_mt_for_module = cr_to_pair Crobj_converter.string_of_concrete_object (g mli_mt_for_module_label);
      needed_libs_for_module = cr_to_pair (Crobj_converter_combinator.to_list Ocaml_library.of_concrete_object) (g needed_libs_for_module_label);
      direct_fathers_for_module = cr_to_pair (Crobj_converter_combinator.to_list Dfa_module.of_concrete_object) (g direct_fathers_for_module_label);
      ancestors_for_module = cr_to_pair (Crobj_converter_combinator.to_list Dfa_module.of_concrete_object) (g ancestors_for_module_label); 
      needed_dirs_for_module = cr_to_pair (Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object) (g needed_dirs_for_module_label);
      product_up_to_date_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g product_up_to_date_for_module_label);
      directories = (Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object)  (g directories_label);
      printer_equipped_types = Crobj_converter_combinator.to_pair_list 
                                      Dfn_endingless.of_concrete_object
                                      Crobj_converter.bool_of_concrete_object (g printer_equipped_types_label);
   };; 

let to_concrete_object cs=
   let items= 
   [
    frontier_with_unix_world_label, Fw_wrapper_automatic.to_concrete_object cs.Coma_state_t.frontier_with_unix_world;
    modules_label, Crobj_converter_combinator.of_list Dfa_module.to_concrete_object cs.Coma_state_t.modules;
    subdir_for_module_label, cr_of_pair Dfa_subdirectory.to_concrete_object cs.Coma_state_t.subdir_for_module;
    principal_ending_for_module_label, cr_of_pair Dfa_ending.to_concrete_object cs.Coma_state_t.principal_ending_for_module;
    mli_presence_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object cs.Coma_state_t.mli_presence_for_module;  
    principal_mt_for_module_label, cr_of_pair Crobj_converter.string_to_concrete_object cs.Coma_state_t.principal_mt_for_module;
    mli_mt_for_module_label, cr_of_pair Crobj_converter.string_to_concrete_object  cs.Coma_state_t.mli_mt_for_module;
    needed_libs_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Ocaml_library.to_concrete_object) cs.Coma_state_t.needed_libs_for_module; 
    direct_fathers_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Dfa_module.to_concrete_object) cs.Coma_state_t.direct_fathers_for_module;   
    ancestors_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Dfa_module.to_concrete_object) cs.Coma_state_t.ancestors_for_module;   
    needed_dirs_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object)  (cs.Coma_state_t.needed_dirs_for_module);  
    product_up_to_date_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object cs.Coma_state_t.product_up_to_date_for_module; 
    directories_label,  (Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object) cs.Coma_state_t.directories; 
    printer_equipped_types_label,  Crobj_converter_combinator.of_pair_list 
                                      Dfn_endingless.to_concrete_object
                                      Crobj_converter.bool_to_concrete_object cs.Coma_state_t.printer_equipped_types;    
   ]  in
   Concrete_object_t.Record items;;


end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


