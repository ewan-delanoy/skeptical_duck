(* 

#use"Compilation_management/coma_state.ml";;

*)

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
  let root cs= Fw_configuration.root (configuration cs);;
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
  
  let product_up_to_date_for_module cs mn=
     try  List.assoc mn ((of_t cs).Coma_state_t.product_up_to_date_for_module) with     
     _ -> raise(Module_not_found(mn));;
  
  let all_subdirectories cs = 
    Fw_with_dependencies.all_subdirectories
    (frontier_with_unix_world cs) ;;
  
  
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
     let current_assoc = (of_t cs).Coma_state_t.subdir_for_module in 
     Image.image snd current_assoc ;;
  
  
  
  (* Setters  *)
  
  let set_frontier_with_unix_world cs v= 
     let ccs=of_t cs in 
     to_t({ccs with Coma_state_t.frontier_with_unix_world=v});;
  
  
  let set_push_after_backup cs bowl = let ccs=of_t cs in 
       let old_frontier = ccs.Coma_state_t.frontier_with_unix_world in 
       let new_frontier = 
        Fw_with_dependencies.set_gitpush_after_backup 
         old_frontier bowl  in 
       to_t({ccs with Coma_state_t.frontier_with_unix_world=new_frontier });;
  
  let set_subdir_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.subdir_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.subdir_for_module=new_assocs });;
      
  
  let set_principal_ending_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.principal_ending_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.principal_ending_for_module=new_assocs });;
  
  
  let set_mli_presence_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.mli_presence_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.mli_presence_for_module=new_assocs });;
  
  
  let set_principal_mt_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.principal_mt_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.principal_mt_for_module=new_assocs });;
  
  let set_mli_mt_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.mli_mt_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.mli_mt_for_module=new_assocs });;
  
  let set_needed_libs_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.needed_libs_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.needed_libs_for_module=new_assocs });;
  
  
  let set_direct_fathers_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.direct_fathers_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.direct_fathers_for_module=new_assocs });;
  
  
  
  let set_ancestors_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.ancestors_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.ancestors_for_module=new_assocs });;
  
  
  let set_needed_dirs_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.needed_dirs_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.needed_dirs_for_module=new_assocs });;
      
  
  
  let set_product_up_to_date_for_module cs mn v=
      let ccs=of_t cs in 
      let old_assocs = ccs.Coma_state_t.product_up_to_date_for_module in 
      let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
      to_t({ccs with Coma_state_t.product_up_to_date_for_module=new_assocs });;
      
  
  
  let set_all_subdirectories cs v = let ccs=of_t cs in 
                              to_t({ccs with Coma_state_t.directories=v});;
  
  
  let set_preq_types cs v = let ccs=of_t cs in 
                              to_t({ccs with Coma_state_t.printer_equipped_types=v});;
  
  
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
       Coma_state_t.frontier_with_unix_world= Fw_with_dependencies.empty_one config;
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
  
  
  let passive_constructor fw = 
      let modules_in_order = Fw_with_dependencies.dep_ordered_modules fw in 
      let subdirs_fm = Image.image (
        fun mn -> (mn,Fw_with_dependencies.subdir_for_module fw mn) 
      ) modules_in_order in 
      let principal_endings_fm = Image.image (
                             fun mn -> (mn,Fw_with_dependencies.principal_ending_for_module fw mn) 
                           ) modules_in_order in 
      let mli_presences_fm =  Image.image (
        fun mn -> (mn,Fw_with_dependencies.mli_presence_for_module fw mn) 
     ) modules_in_order in 
     let principal_mts_fm = Image.image (
      fun mn -> (mn,Fw_with_dependencies.principal_mt_for_module fw mn) 
     ) modules_in_order in 
     let mli_mts_fm = Image.image (
      fun mn -> (mn,Fw_with_dependencies.mli_mt_for_module fw mn) 
     ) modules_in_order in 
     let needed_libs_fm = Image.image (
      fun mn -> (mn,Fw_with_dependencies.needed_libs_for_module fw mn) 
     ) modules_in_order in 
     let needed_dirs_fm = Image.image (
      fun mn -> (mn,Fw_with_dependencies.needed_dirs_for_module fw mn) 
     ) modules_in_order in 
     let direct_fathers_fm = Image.image (
      fun mn -> (mn,Fw_with_dependencies.direct_fathers_for_module fw mn) 
     ) modules_in_order in 
     let ancestors_fm = Image.image (
      fun mn -> (mn,Fw_with_dependencies.ancestors_for_module fw mn) 
     ) modules_in_order in 
     let all_subdirs = Fw_with_dependencies.all_subdirectories  fw in 
     let preq_types = Fw_with_dependencies.printer_equipped_types  fw in 
      to_t({
       Coma_state_t.frontier_with_unix_world= fw;
       modules = modules_in_order ;
       subdir_for_module = subdirs_fm ;
       principal_ending_for_module = principal_endings_fm ;
       mli_presence_for_module = mli_presences_fm  ;
       principal_mt_for_module = principal_mts_fm ;
       mli_mt_for_module = mli_mts_fm ;
       needed_libs_for_module = needed_libs_fm ; 
       direct_fathers_for_module = direct_fathers_fm ; 
       ancestors_for_module = ancestors_fm ;                    
       needed_dirs_for_module = needed_dirs_fm ; 
       directories = all_subdirs;
       printer_equipped_types = preq_types ;
       product_up_to_date_for_module = Image.image (
                              fun mn -> (mn,false) 
                          ) modules_in_order  ;
  });;
  

  let change_one_module_name wrapped_cs old_mn new_mn=
      (* note that printer_equipped_types are not dealt with here *)
      let cs=of_t wrapped_cs in
      let new_modules = Image.image (fun x->if x=old_mn then new_mn else x)(dep_ordered_modules cs) in  
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
      let new_modules = List.filter (fun x->x<>mname) (dep_ordered_modules cs) 
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
      let new_modules = Listennou.reposition_by_putting_snd_immediately_after_fst (dep_ordered_modules cs) mn1 mn2 
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
      let new_modules = Listennou.push_immediately_after (dep_ordered_modules cs) nm  pivot 
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
     let g=Concrete_object.get_record ccrt_obj in
     {
        Coma_state_t.frontier_with_unix_world = Fw_with_dependencies.of_concrete_object (g frontier_with_unix_world_label);
        modules = Crobj_converter_combinator.to_list Dfa_module.of_concrete_object (g modules_label);
        subdir_for_module = cr_to_pair Dfa_subdirectory.of_concrete_object (g subdir_for_module_label);
        principal_ending_for_module = cr_to_pair Dfa_ocaml_ending.of_concrete_object (g principal_ending_for_module_label);
        mli_presence_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g mli_presence_for_module_label);
        principal_mt_for_module = cr_to_pair Crobj_converter.string_of_concrete_object (g principal_mt_for_module_label);
        mli_mt_for_module = cr_to_pair Crobj_converter.string_of_concrete_object (g mli_mt_for_module_label);
        needed_libs_for_module = cr_to_pair (Crobj_converter_combinator.to_list Ocaml_library.of_concrete_object) (g needed_libs_for_module_label);
        direct_fathers_for_module = cr_to_pair (Crobj_converter_combinator.to_list Dfa_module.of_concrete_object) (g direct_fathers_for_module_label);
        ancestors_for_module = cr_to_pair (Crobj_converter_combinator.to_list Dfa_module.of_concrete_object) (g ancestors_for_module_label); 
        needed_dirs_for_module = cr_to_pair (Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object) (g needed_dirs_for_module_label);
        product_up_to_date_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g product_up_to_date_for_module_label);
        directories = (Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object)  (g directories_label);
        printer_equipped_types = Crobj_converter_combinator.to_list 
                                        Dfn_middle.of_concrete_object
                                         (g printer_equipped_types_label);
     };; 
  
  let to_concrete_object cs=
     let items= 
     [
      frontier_with_unix_world_label, Fw_with_dependencies.to_concrete_object cs.Coma_state_t.frontier_with_unix_world;
      modules_label, Crobj_converter_combinator.of_list Dfa_module.to_concrete_object cs.Coma_state_t.modules;
      subdir_for_module_label, cr_of_pair Dfa_subdirectory.to_concrete_object cs.Coma_state_t.subdir_for_module;
      principal_ending_for_module_label, cr_of_pair Dfa_ocaml_ending.to_concrete_object cs.Coma_state_t.principal_ending_for_module;
      mli_presence_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object cs.Coma_state_t.mli_presence_for_module;  
      principal_mt_for_module_label, cr_of_pair Crobj_converter.string_to_concrete_object cs.Coma_state_t.principal_mt_for_module;
      mli_mt_for_module_label, cr_of_pair Crobj_converter.string_to_concrete_object  cs.Coma_state_t.mli_mt_for_module;
      needed_libs_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Ocaml_library.to_concrete_object) cs.Coma_state_t.needed_libs_for_module; 
      direct_fathers_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Dfa_module.to_concrete_object) cs.Coma_state_t.direct_fathers_for_module;   
      ancestors_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Dfa_module.to_concrete_object) cs.Coma_state_t.ancestors_for_module;   
      needed_dirs_for_module_label, cr_of_pair (Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object)  (cs.Coma_state_t.needed_dirs_for_module);  
      product_up_to_date_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object cs.Coma_state_t.product_up_to_date_for_module; 
      directories_label,  (Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object) cs.Coma_state_t.directories; 
      printer_equipped_types_label,  Crobj_converter_combinator.of_list 
                                        Dfn_middle.to_concrete_object
                                        cs.Coma_state_t.printer_equipped_types;    
     ]  in
     Concrete_object_t.Record items;;
  
  
  end ;;
  
  let of_concrete_object = Private.of_concrete_object;;
  let to_concrete_object = Private.to_concrete_object;;
  
  
  

end ;;  


(* Inherited values *)



let root =Automatic.root;;
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
let product_up_to_date_for_module = Automatic.product_up_to_date_for_module ;;
let all_subdirectories = Automatic.all_subdirectories;;
let printer_equipped_types = Automatic.printer_equipped_types;;


let set_frontier_with_unix_world = Automatic.set_frontier_with_unix_world;;
let set_subdir_for_module = Automatic.set_subdir_for_module ;;
let set_principal_ending_for_module = Automatic.set_principal_ending_for_module ;;
let set_mli_presence_for_module = Automatic.set_mli_presence_for_module ;;
let set_principal_mt_for_module = Automatic.set_principal_mt_for_module ;;
let set_mli_mt_for_module = Automatic.set_mli_mt_for_module ;;
let set_needed_libs_for_module  = Automatic.set_needed_libs_for_module ;;
let set_direct_fathers_for_module = Automatic.set_direct_fathers_for_module ;;
let set_ancestors_for_module = Automatic.set_ancestors_for_module ;; 

let set_needed_dirs_for_module  = Automatic.set_needed_dirs_for_module ;;
let set_product_up_to_date_for_module = Automatic.set_product_up_to_date_for_module ;;
let set_all_subdirectories = Automatic.set_all_subdirectories;;
let set_preq_types = Automatic.set_preq_types;;


let dep_ordered_modules = Automatic.dep_ordered_modules;;
let follows_it = Automatic.follows_it_but_does_not_necessarily_depend_on_it;;
let all_used_subdirs = Automatic.all_used_subdirs;;


let change_one_module_name = Automatic.change_one_module_name ;;
let configuration = Automatic.configuration ;;
let empty_one = Automatic.empty_one ;;
let impose_last_changes = Automatic.impose_last_changes ;;
let modify_all_needed_dirs = Automatic.modify_all_needed_dirs ;;
let modify_all_subdirs = Automatic.modify_all_subdirs ;;
let root = Automatic.root ;;
let set_push_after_backup = Automatic.set_push_after_backup ;;
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


let up_to_date_elesses cs =
   Option.filter_and_unpack (
     fun mn->
       if product_up_to_date_for_module cs mn
       then Some(endingless_at_module cs mn)
       else None
   )(dep_ordered_modules cs);;

let preq_types_with_extra_info cs =
   let root = root cs  in 
   Image.image (fun middle->
    let mn = Dfn_middle.to_module middle in 
    (Dfn_join.root_to_middle root middle,product_up_to_date_for_module cs mn)
   ) (printer_equipped_types cs) ;;

exception Find_subdir_from_suffix_exn of string * (Dfa_subdirectory_t.t list) ;;

let find_subdir_from_suffix cs possibly_slashed_suffix =
  let suffix = Cull_string.trim_slashes_on_the_right possibly_slashed_suffix  in
  let temp1 = List.filter (
    fun subdir -> Supstring.contains (Dfa_subdirectory.without_trailing_slash subdir) suffix
  ) (cs.Coma_state_t.directories) in 
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

let modules_with_their_ancestors cs l=
   let temp1=List.filter (
     fun nm->List.mem nm l 
     ) (dep_ordered_modules cs )   in 
   let temp2=Image.image (
     fun nm->
       (ancestors_for_module cs nm)@[nm] 
   ) temp1 in 
   let temp3=List.flatten temp2 in 
   Listennou.nonredundant_version temp3;;

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

exception Non_existent_mtime of Dfn_full_t.t;;

let force_modification_time root_dir cs mlx=
      let edg=Dfn_full.to_ending mlx in
      let nm=Dfn_full.to_module mlx in
      let file=Dfn_full.to_line mlx in 
      let new_val=string_of_float((Unix.stat file).Unix.st_mtime)  in
      let cs2=(
        if (Dfa_ocaml_ending.of_ending edg)=principal_ending_for_module cs nm 
        then set_principal_mt_for_module cs nm new_val
        else cs
      ) in
      let cs3=(
        if edg=Dfa_ending.mli
        then set_mli_mt_for_module cs2 nm new_val
        else cs2
      ) in     
      cs3;;


 
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
   let old_preqtypes = Automatic.printer_equipped_types cs2 in 
   let new_preqtypes = List.filter (fun 
    middle -> List.for_all (fun eless->
       (Dfn_endingless.to_middle eless)<>middle ) elesses) old_preqtypes in
   let cs3 = (
     if new_preqtypes <> old_preqtypes 
     then Automatic.set_preq_types cs2 new_preqtypes
     else cs2
   ) in 
   cs3;;     

let unregister_module cs eless= unregister_modules cs [eless] ;; 
                    
exception Non_registered_file of Dfn_full_t.t;;  
exception Abandoned_children of Dfn_full_t.t * (Dfa_module_t.t list);;
                      
                     
let partially_remove_mlx_file cs mlxfile=
    let eless=Dfn_full.to_endingless mlxfile
    and nm=Dfn_full.to_module mlxfile in
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
              let old_preqtypes = Automatic.printer_equipped_types cs5 in 
              let new_preqtypes = List.filter (fun 
                middle -> 
                (Dfn_endingless.to_middle eless)<>middle 
              ) old_preqtypes in
              let cs6=(
                if new_preqtypes <> old_preqtypes 
                then Automatic.set_preq_types cs5 new_preqtypes
                else cs5
              ) in 
              cs6
         else (* if we get here, there are two registered endings, one of which
              is the mli *) 
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
            


let compute_subdirectories_list cs=
  let temp1=Image.image Dfa_subdirectory.without_trailing_slash (all_used_subdirs cs) in
    let temp2=Set_of_strings.sort temp1 in
    let temp3=Set_of_strings.forget_order temp2 in
    Image.image Dfa_subdirectory.of_line temp3;;

let  check_registrations cs eless=
   let mn=Dfn_endingless.to_module eless in 
   Dfa_ending.compute_on_all_ocaml_endings 
      (fun edg->check_ending_in_at_module (Dfa_ocaml_ending.of_ending edg) cs mn);;


module PrivateTwo=struct


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
  let libned=PrivateTwo.find_needed_libraries cs rless modules_written_in_file
  and dirned=PrivateTwo.find_needed_directories cs rless modules_written_in_file in
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
    let libned=PrivateTwo.find_needed_libraries cs rless modules_written_in_file
    and dirned=PrivateTwo.find_needed_directories cs rless modules_written_in_file in
    (eless,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;
    

let above cs eless=
  let nm=Dfn_endingless.to_module eless in
  ancestors_for_module cs nm;;
 
let below_module cs mn0 =
  List.filter(fun mn->List.mem mn0 (ancestors_for_module cs mn)) (dep_ordered_modules cs);; 

let below cs eless=
        let mn0=Dfn_endingless.to_module eless  in
        Option.filter_and_unpack(fun mn->
            if List.mem mn0 (ancestors_for_module cs mn)
            then Some(mn)
            else None) (dep_ordered_modules cs);;    

let directly_above cs eless=
    let nm=Dfn_endingless.to_module eless in
     direct_fathers_for_module cs nm;;     

let directly_below cs eless=
        let mn0=Dfn_endingless.to_module eless  in
        Option.filter_and_unpack(fun mn->
            if List.mem mn0 (direct_fathers_for_module cs mn)
            then Some(mn)
            else None) (dep_ordered_modules cs);;        

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


let system_size cs=List.length(dep_ordered_modules cs);;

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

let modules_using_value cs value_name =
  Option.filter_and_unpack (fun mn->
  let eless=endingless_at_module cs mn
  and pr_end=principal_ending_for_module cs mn in
  let mlx=Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending pr_end) in
   let ap=Dfn_full.to_absolute_path mlx in
   if Substring.is_a_substring_of 
       value_name (Io.read_whole_file ap)
   then Some eless
   else None ) (dep_ordered_modules cs);;





let update_ancs_libs_and_dirs_at_module cs mn=
  let eless=endingless_at_module cs mn  
  and pr_end=principal_ending_for_module cs mn in
  let rless=Dfn_full.to_rootless (Dfn_join.to_ending eless (Dfa_ocaml_ending.to_ending pr_end)) in 
  let fathers=direct_fathers_for_module cs mn in
  let separated_ancestors=Image.image 
  (fun nm2->
    Set_of_polys.safe_set(ancestors_for_module cs nm2)
  ) fathers in
  let ancestors_with_wrong_order=Set_of_polys.fold_merge((Set_of_polys.safe_set fathers)::separated_ancestors) in
  let ordered_ancestors=List.filter (
    fun mn->Set_of_polys.mem mn ancestors_with_wrong_order
  ) (dep_ordered_modules cs) in
  let new_libs=PrivateTwo.find_needed_libraries cs rless ordered_ancestors
  and new_dirs=PrivateTwo.find_needed_directories cs rless ordered_ancestors in
  let cs2=set_ancestors_for_module cs mn ordered_ancestors in 
  let cs3=set_needed_libs_for_module cs2 mn new_libs in
  set_needed_dirs_for_module cs3 mn new_dirs;;


let update_ancs_libs_and_dirs cs=
  let cs_walker=ref(cs) in 
  let _=List.iter(fun mn->cs_walker:=update_ancs_libs_and_dirs_at_module (!cs_walker) mn)(dep_ordered_modules cs) in
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
      "\n\n"^
      "The following modules have been directly changed :\n\n"^
      (String.concat ", " temp1)^
      "\n\n"
    ;;       

    let message_about_involved_modules involved_modules=
      let temp1=Image.image Dfa_module.to_line involved_modules in
      "\n\n"^
      "The following modules need to be recompiled \n"^
      "because they depend on directly changed modules :\n\n"^
      (String.concat ", " temp1)^
      "\n\n"
    ;;    

    let message_about_changed_noncompilables changed_noncompilables=
      let temp1=Image.image Dfn_rootless.to_line changed_noncompilables in
      "\n\n"^
      "The following noncompilables have been directly changed :\n\n"^
      (String.concat ", " temp1)^
      "\n\n"
    ;;    

    let message_about_changed_archived_compilables changed_ac=
    let temp1=Image.image Dfn_rootless.to_line changed_ac in
    "\n\n"^
    "The following archived files have been directly changed :\n\n"^
    (String.concat ", " temp1)^
    "\n\n"
  ;;    

    let announce_changed_modules changed_modules=
      if changed_modules=[]
      then ()
      else (print_string(message_about_changed_modules changed_modules);flush stdout);;

    let announce_involved_modules involved_modules=
      if involved_modules=[]
      then ()
      else (print_string(message_about_involved_modules involved_modules);flush stdout);;  
             
    let announce_changed_noncompilables changed_noncompilables=
      if changed_noncompilables=[]
      then ()
      else (print_string(message_about_changed_noncompilables changed_noncompilables);flush stdout);;
    
    let announce_changed_archived_compilables changed_ac=
      if changed_ac=[]
      then ()
      else (print_string(message_about_changed_archived_compilables changed_ac);flush stdout);;  

    let put_md_list_back_in_order tolerate_cycles 
      cs initially_active_nms=
      let md_list=dep_ordered_modules cs in
      let coat=Memoized.make (fun nm->direct_fathers_for_module cs nm) in
      let (cycles,reordered_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles (
        (fun nm->
           let middle = Dfn_endingless.to_middle ( endingless_at_module cs nm) in 
           Dfn_middle.to_line middle )
      ) cycles in     
      let cs2=Automatic.reorder cs (Image.image fst reordered_list) in    
      let cs3=update_ancs_libs_and_dirs cs2 in 
      let active_descendants=Option.filter_and_unpack (
          fun nm->
            if List.mem nm initially_active_nms
            then Some(nm)
            else
            if List.exists (fun nm2->List.mem nm2 initially_active_nms) 
                 (ancestors_for_module cs nm)
            then Some(nm)
            else None
      ) (dep_ordered_modules cs) in  
      (cs3,active_descendants);;
     
end;; 
     
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
  if no_change_for_mlis&&(pr_modif_time=old_pr_modif_time)&&(product_up_to_date_for_module cs mn)
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
  
(*  
let latest_changes_in_compilables cs = 
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun nm->
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (rootless_lines_at_module cs nm))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun mname->
    match check_for_possible_change (!cs_walker) mname with
    None->()
    |_->
    (
    declare_changed(mname);
    )
)(dep_ordered_modules cs) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then [] else
let _=PrivateThree.announce_changed_modules changed_modules in
(!ref_for_changed_shortpaths);; 
*)

let latest_changes cs = 
  let fw = Automatic.frontier_with_unix_world cs in 
  let par1 = fw.Fw_with_dependencies_t.parent in 
  let par2 = par1.Fw_with_small_details_t.parent in 
  let (_,changed_files) = File_watcher.inspect_and_update ~verbose:false par2 in 
  let (a_files,u_files,nc_files) = Fw_modular.partition_for_singles par2 changed_files in 
  let im = Image.image Dfn_rootless.to_line in 
  (im a_files,im u_files,im nc_files);;



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
  else  
  let last_father=List.hd(temp3) in
  let nm=Dfn_rootless.to_module rless in 
  let cs_walker=ref(cs) in 
  let _=List.iter(
      fun current_module ->
      let current_anc= ancestors_for_module (!cs_walker) current_module in  
      if not(List.mem nm current_anc)
      then ()
      else  
      let current_libs= needed_libs_for_module cs current_module in
      let new_ancestors=Option.filter_and_unpack(
        fun nm2->
        if (List.mem nm2 allanc)||(List.mem nm2 current_anc)
        then Some(nm2)
        else None
      ) (dep_ordered_modules (!cs_walker)) 
      and new_libs=List.filter (
          fun lib->(List.mem lib libned)||(List.mem lib current_libs)
      ) Ocaml_library.all_libraries in  
      let ordered_dirs=Set_of_polys.merge
        (Set_of_polys.safe_set(needed_dirs_for_module (!cs_walker) current_module))
        (Set_of_polys.safe_set (dirned)) in
      let new_dirs=Set_of_polys.forget_order(ordered_dirs) in
      cs_walker:=set_ancestors_for_module (!cs_walker) current_module new_ancestors;
      cs_walker:=set_needed_libs_for_module (!cs_walker) current_module new_libs;
      cs_walker:=set_needed_dirs_for_module (!cs_walker) current_module new_dirs;
  )(follows_it cs last_father) in 
  let _=
  ( 
              cs_walker:=Automatic.remove_in_each_at_module (!cs_walker) nm;
              cs_walker:=Automatic.push_after_module_in_each (!cs_walker) last_father new_dt;  
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
    let mli_reg=check_ending_in_at_module Dfa_ocaml_ending_t.Mli cs nm in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if (cmod=Compilation_mode_t.Executable)&&(not(mli_reg)) 
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
    let mli_reg=check_ending_in_at_module Dfa_ocaml_ending_t.Mli cs nm in 
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
    let mli_reg=check_ending_in_at_module Dfa_ocaml_ending_t.Mli cs nm
    and ml_reg=check_ending_in_at_module Dfa_ocaml_ending_t.Ml cs nm in
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

let command_for_predebuggable  cs short_path=
    let cmod = Compilation_mode_t.Debug in 
    let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath(root cs))^short_path) in 
    let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
    let nm_deps =modules_with_their_ancestors cs nm_direct_deps in 
    let nm_deps_with_subdirs = Image.image (
       fun nm->
               let subdir=subdir_for_module cs nm in 
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
     (fun (_,nm) -> Set_of_polys.sort(needed_libs_for_module cs nm)) nm_deps_with_subdirs in
    let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let extension=".cma" in
    let libs=String.concat(" ")
      (Image.image(fun z->Ocaml_library.file_for_library(z)^extension) pre_libs2) in 
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
    let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
    let nm_deps =modules_with_their_ancestors cs nm_direct_deps in 
    let nm_deps_with_subdirs = Image.image (
       fun nm->let subdir=subdir_for_module cs nm in 
        (subdir,nm)
    ) nm_deps in 
    let s_root=Dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) 
    and ending=Compilation_mode.ending_for_nonlast_module cmod 
    and last_ending=Compilation_mode.ending_for_last_module cmod 
    and product_ending=Compilation_mode.ending_for_final_product cmod  in
    let cm_elements_but_the_last = Image.image (
      fun (subdir,nm)->(Dfa_module.to_line nm)^ending
    ) nm_deps_with_subdirs in 
    let unpointed_short_path = Cull_string.before_rightmost rootless_path '.' in 
    let nm_name = (Cull_string.after_rightmost unpointed_short_path '/') in 
    let last_cm_element=nm_name^last_ending in 
    let all_cm_elements= cm_elements_but_the_last @ [last_cm_element] in 
    let libs_for_prow = 
      Set_of_polys.sort(
      Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Image.image Dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Image.image 
     (fun (_,nm) -> Set_of_polys.sort(needed_libs_for_module cs nm)) nm_deps_with_subdirs in
    let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
    let libs=String.concat(" ")
      (Image.image(fun z->Ocaml_library.file_for_library(z)^extension) pre_libs2) in 
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

let ocamldebug_printersfile_path root= 
           (Dfa_root.connectable_to_subpath root)^
           (Dfa_subdirectory.connectable_to_subpath(Coma_constant.utility_files_subdir)) ^
             "cmos_for_ocamldebug.txt";;


module Ocaml_target_making=struct




exception Failed_during_compilation of (Dfa_module_t.t*Dfn_endingless_t.t*string);;

let rec helper_for_feydeau  (cmod:Compilation_mode_t.t) cs (rejected,treated,to_be_treated)=
     match to_be_treated with 
     []->(cs,rejected,List.rev treated)
     |triple::other_triples->
       let (nm,eless,cmd)=triple in
       if (Unix_command.uc cmd)=0
       then 
            let cs2=set_product_up_to_date_for_module cs nm true in 
            helper_for_feydeau cmod cs2 (rejected,(nm,eless)::treated,other_triples)
       else if (cmod<>Compilation_mode_t.Usual)
            then raise(Failed_during_compilation(triple))
            else 
            let triples_after=snd(Prepared.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
            let (rejected_siblings_as_triples,survivors)=List.partition
           (
              fun (nm2,_,_)->
                List.mem nm (ancestors_for_module cs nm2)
           ) triples_after in 
           let rejected_siblings_with_redundancies =  
              Image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
           let rejected_siblings = Listennou.nonredundant_version rejected_siblings_with_redundancies in    
           let newly_rejected = (nm,eless)::rejected_siblings in 
           let cs_walker=ref(cs) in 
           let _=List.iter(
              fun (nm3,hm3)->
                cs_walker:=set_product_up_to_date_for_module (!cs_walker) nm3 false
           ) newly_rejected in 
           helper_for_feydeau cmod (!cs_walker) (rejected@newly_rejected,treated,survivors) ;;
         

let prepare_pretty_printers_for_ocamldebug cs deps = 
  let temp1 = "load_printer str.cma"::(Image.image (fun mname->
    let s= Dfa_module.to_line mname in 
    "load_printer "^s^".cmo"
  ) deps) 
  and printer_equipped_types = preq_types_with_extra_info cs  in 
  let printable_deps = List.filter (
    fun mn -> let eless = endingless_at_module cs mn in 
    List.mem (eless,true) printer_equipped_types
  ) deps in 
  let temp2 = Image.image (fun mname->
    let s= Dfa_module.to_line mname in 
    "install_printer "^(String.capitalize_ascii s)^".print_out"
  ) printable_deps in 
  let full_text = String.concat "\n" (temp1@temp2) in 
  let ppodbg_path = ocamldebug_printersfile_path (root cs) in 
  Io.overwrite_with (Absolute_path.of_string ppodbg_path) full_text;;

let dependencies_inside_shaft cmod cs (opt_modnames,opt_rootless_path)=
   match cmod with 
   Compilation_mode_t.Usual->Option.unpack opt_modnames
   |_->let rootless_path=Option.unpack opt_rootless_path in 
       let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath (root cs))^rootless_path) in 
       let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
       let nm_deps=modules_with_their_ancestors cs nm_direct_deps in 
       let deps =List.filter (fun mn->List.mem mn nm_deps) (dep_ordered_modules cs) in 
       let _=(if cmod = Compilation_mode_t.Debug 
              then prepare_pretty_printers_for_ocamldebug cs deps) in 
       deps;;



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
    Compilation_mode_t.Usual
   |Compilation_mode_t.Executable ->[] 
   |_->
      let rootless_path=Option.unpack opt_rootless_path in 
      Modern.command_for_predebuggable cs rootless_path) in 
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
   let pair = (None,Some(short_path)) in 
   let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod cs pair in 
   let cmds1=Image.image (fun (_,_,cmd)->cmd) pre_cmds1
   and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod cs pair
   and cmds3=list_of_commands_for_end_part_of_feydeau cmod cs pair in 
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
  let eless = endingless_at_module cs mn in 
  let middle = Dfn_endingless.to_middle eless in 
  set_preq_types cs ((printer_equipped_types cs)@[middle]);;

let remove_printer_equipped_type cs mn=
  set_preq_types cs (List.filter (fun mn2->mn2<>mn) (printer_equipped_types cs));;

let uple_form cs=
  (cs,
   all_subdirectories cs,
   preq_types_with_extra_info cs
   );;


let unregister_mlx_file cs mlx=
    let mn=Dfn_full.to_module mlx in 
    let following = mn::(follows_it cs mn) in  
    let was_lonely=
      (List.length(registered_endings_at_module cs mn)=1) in 
    let _=set_product_up_to_date_for_module cs mn false in 
    let cs2=partially_remove_mlx_file cs mlx in
    let new_dirs=compute_subdirectories_list cs2 in
    let cs3=(if was_lonely 
           then cs2
           else ( fun (cs4,_,_)->cs4)
           (Ocaml_target_making.usual_feydeau 
             cs2 following) ) in 
    set_all_subdirectories cs3 new_dirs;;   

let unregister_mlx_files cs mlxs = 
  List.fold_left unregister_mlx_file cs mlxs ;; 


exception FileWithDependencies of 
Dfn_full_t.t*(Dfa_module_t.t list);;

let read_persistent_version x=
        let full_path=Dfn_join.root_to_rootless (root x)  Coma_constant.rootless_path_for_targetfile in
        let ap= Dfn_full.to_absolute_path full_path in
        let the_archive=Io.read_whole_file ap in
        let archived_object = Crobj_parsing.parse the_archive in 
        Automatic.of_concrete_object archived_object;;      

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



module Register_mlx_file=struct

let on_targets (cs,old_dirs) rless=
    let new_dir=Dfn_rootless.to_subdirectory rless in
   let cs2=register_mlx_file_on_monitored_modules cs rless in
   let new_dirs=
   (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
    in
    let nm=Dfn_rootless.to_module rless in 
    let (cs3,_,_)=Ocaml_target_making.usual_feydeau cs2 [nm] in 
    (cs3,new_dirs);; 
  

end;;  


let register_mlx_file cs mlx=
          let (cs2,new_dirs)= 
          Register_mlx_file.on_targets (cs,all_subdirectories cs) mlx in   
           set_all_subdirectories cs2 new_dirs;;            

let register_mlx_files cs mlxs = List.fold_left register_mlx_file cs mlxs;;

let clean_debug_dir cs=
  let s_root=Dfa_root.connectable_to_subpath(root cs) in
  let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir)) in 
  Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
   
let name_element_for_debugged_file = "debugged" ;;
let debugged_file_path = (Dfa_subdirectory.connectable_to_subpath(Coma_constant.utility_files_subdir))
             ^ name_element_for_debugged_file ^ ".ml" ;;  

let start_debugging cs=
  let  _=clean_debug_dir cs in
  let ppodbg_path = ocamldebug_printersfile_path (root cs) in 
  let _= Io.overwrite_with (Absolute_path.of_string ppodbg_path) "" in   
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Compilation_mode_t.Debug cs debugged_file_path in 
  let answer=Unix_command.conditional_multiple_uc cmds in 
  let dbgbuild_path =  Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir) in 
	let msg=(
	  if answer
	  then "\n\n Now, start \n\nocamldebug "^dbgbuild_path^name_element_for_debugged_file^
         ".ocaml_debuggable\n\nin another terminal.\n\n"^
         "If you need to use pretty printers, from inside ocamldebug do \n\n"^ 
         "source "^ppodbg_path^" \n\n"
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
   let temp1=Look_for_module_names.indices_in_mlx_file file in
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


module Almost_concrete = struct 


let local_above cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in  
  Image.image (fun nm-> 
    let mname = Dfn_endingless.to_module (endingless_at_module cs nm) in 
    Dfa_module.to_line mname )
  (above cs endingless);;


let local_below cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in  
  Image.image (fun nm-> 
    let mname = Dfn_endingless.to_module (endingless_at_module cs nm) in 
    Dfa_module.to_line mname )
  (below cs endingless);;

let local_directly_above cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in   
  Image.image (fun nm-> 
    let mname = Dfn_endingless.to_module (endingless_at_module cs nm) in 
    Dfa_module.to_line mname )
  (directly_above cs endingless);;

let local_directly_below cs capitalized_or_not_module_name=
  let mn = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in   
  Image.image (fun nm-> 
    let mname = Dfn_endingless.to_module (endingless_at_module cs nm) in 
    Dfa_module.to_line mname )
  (directly_below cs endingless);; 


end;; 




module Recent_changes = struct
           
    exception Recompilation_needed of Dfa_module_t.t list;;       

            let check_for_change_at_module_and_ending cs mn edg=
               let hm=endingless_at_module cs mn in 
               (md_recompute_modification_time hm edg)
               <>(get_modification_time cs mn edg);;

            let check_for_change_at_module  cs mn=
               let pr_ending = principal_ending_for_module cs mn in 
               let endings = (
                   if mli_presence_for_module cs mn 
                   then  [Dfa_ocaml_ending_t.Mli;pr_ending]
                   else [pr_ending]
               ) in 
            List.exists (check_for_change_at_module_and_ending cs mn) endings ;;
          

            let detect_changes cs =
            Option.filter_and_unpack (
               fun mn->
               if check_for_change_at_module cs mn 
               then Some(mn)
               else None
            ) (dep_ordered_modules cs);;

            let check_for_changes cs = 
            let changes = detect_changes cs in 
            if changes<>[]
            then raise(Recompilation_needed(changes))
            else ();;

end;;    

module Late_Recompilation = struct 

let quick_update cs (new_fw,changed_rootlesses)  mn=
  let eless =endingless_at_module cs mn 
  and pr_ending=principal_ending_for_module cs mn in
  let ocaml_pr_ending=Dfa_ocaml_ending.to_ending pr_ending in 
  let middle = Dfn_endingless.to_middle eless in 
  let mli_modif_time=Fw_with_dependencies.get_mtime_or_zero_if_file_is_nonregistered new_fw (Dfn_join.middle_to_ending middle Dfa_ending.mli) 
  and pr_modif_time=Fw_with_dependencies.get_mtime new_fw (Dfn_join.middle_to_ending middle ocaml_pr_ending)  
  and old_mli_modif_time=mli_mt_for_module cs mn
  and old_pr_modif_time=principal_mt_for_module cs mn 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  let mn = Dfn_endingless.to_module eless in 
  if (old_values=new_values)&&(product_up_to_date_for_module cs mn)&&
     (List.for_all (fun rl->(Dfn_rootless.to_middle rl)<>middle ) changed_rootlesses)  
  then None
  else
  let mlx=Dfn_join.middle_to_ending middle ocaml_pr_ending in
  let direct_fathers=find_needed_data cs mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;


end ;;

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


let reflect_latest_changes_in_github cs opt_msg=
  let old_fw = cs.Coma_state_t.frontier_with_unix_world in 
  let new_fw = Fw_with_dependencies.reflect_latest_changes_in_github old_fw opt_msg in 
  {cs with Coma_state_t.frontier_with_unix_world = new_fw} ;;

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

 

exception Empty_acolytes_list ;; 
exception Too_many_acolytes of Dfn_rootless_t.t list ;;
exception Unknown_first_acolyte_ending  of Dfn_rootless_t.t ;;
exception Unknown_second_acolyte_ending of Dfn_rootless_t.t ;;
exception Missing_mli of Dfn_rootless_t.t * Dfn_rootless_t.t ;;
exception Incompatible_locations of Dfn_rootless_t.t * Dfn_rootless_t.t ;;
exception Circular_dependencies_detected ;;

module Simplified_ts_creation = struct 

let find_the_mli_among_the_two rless1 rless2 = 
    if (Dfn_rootless.to_ending rless1) = Dfa_ending.mli 
    then Some(rless1,rless2) 
    else   
    if (Dfn_rootless.to_ending rless2) = Dfa_ending.mli 
    then Some(rless2,rless1) 
    else None ;;
    
let check_admissiblity_of_single_acolyte rless =
    if List.mem (Dfn_rootless.to_ending rless) 
       [Dfa_ending.mli;Dfa_ending.ml;Dfa_ending.mll;Dfa_ending.mly]
    then (None,rless)
    else raise(Unknown_first_acolyte_ending(rless)) ;; 

let check_admissibility_of_acolytes_list l=
   let n = List.length(l) in 
   if n > 2 then raise(Too_many_acolytes l) else 
   if n = 0 then raise  Empty_acolytes_list else 
   if n = 1 then check_admissiblity_of_single_acolyte(List.hd l) else 
   (* if we get here n=2 *)
   match  find_the_mli_among_the_two (List.nth l 0) (List.nth l 1) with 
   None -> raise(Missing_mli(List.nth l 0,List.nth l 1))
   |Some(rless1,rless2) ->
      let subdir1 =  Dfn_rootless.to_subdirectory rless1 
      and subdir2 =  Dfn_rootless.to_subdirectory rless2 in 
      if subdir1 <> subdir2 
      then raise(Incompatible_locations(rless1,rless2))
      else 
      if not(List.mem (Dfn_rootless.to_ending rless2) 
          [Dfa_ending.ml;Dfa_ending.mll;Dfa_ending.mly])
      then raise(Unknown_second_acolyte_ending(rless2))  
      else (Some rless1,rless2) ;;

let classify_according_to_module root compilable_files =
    let temp1 = Image.image (fun (rless,_)->
       (Dfn_rootless.to_module rless,rless)  
    ) compilable_files in 
    let temp2 = Listennou.partition_according_to_fst temp1 in 
    let ap_from_rootless = (fun rless->
       let full = Dfn_join.root_to_rootless root rless in 
       Dfn_full.to_absolute_path full
      ) in 
    Image.image (fun (mn,l)->
      let (opt_mli,principal)=check_admissibility_of_acolytes_list l in 
      let opt_mli_ap = Option.propagate ap_from_rootless opt_mli 
      and principal_ap = ap_from_rootless principal in
      (Dfa_module.to_line mn,(opt_mli,opt_mli_ap,principal,principal_ap))
      ) temp2 ;;

let treat_circular_dependencies cycles= 
      if cycles=[]
      then ()
      else
      let temp1=Image.image(String.concat " -> ") cycles in
      let temp2="\n\n The following cycles have been detected : "^
        (String.concat "\n\n" temp1) in
      let _ = (print_string temp2;flush stdout) in 
      raise Circular_dependencies_detected ;;

let compute_dependencies  prepared_list_of_modules =
  let lex_order = Total_ordering.lex_for_strings in 
  let modules_in_lex_order = Ordered.sort lex_order (Image.image fst  prepared_list_of_modules) in 
  let coatoms = Memoized.make (fun mname ->
     let (opt,opt_ap,pr_rless,pr_ap) = List.assoc mname  prepared_list_of_modules in 
     let mli_part = (match opt_ap with None->[] |(Some ap)->Look_for_module_names.names_in_mlx_file ap)
     and pr_part =  Look_for_module_names.names_in_mlx_file pr_ap in 
     let temp1 = Image.image Dfa_module.to_line (mli_part@pr_part) in 
     List.filter (fun mname -> Ordered.mem lex_order mname modules_in_lex_order) temp1
  )     in 
  let (cycles,good_list) = Reconstruct_linear_poset.reconstruct_linear_poset coatoms  modules_in_lex_order in 
  let _ = treat_circular_dependencies cycles in
  good_list ;; 

end ;;   

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

let passive_constructor = Automatic.passive_constructor ;;

let below_several cs mods = 
  let all_mods_in_order = dep_ordered_modules cs in 
  let below_module = (fun mn->below cs (endingless_at_module cs mn)) in 
  let temp1 = List.flatten(mods :: (Image.image below_module mods)) in
  let all_deps = List.filter (fun mn->List.mem mn temp1) all_mods_in_order in 
  let (mods_in_order,new_deps) = List.partition (fun mn->List.mem mn mods) all_deps in 
  (all_deps,new_deps,mods_in_order) ;;
    

