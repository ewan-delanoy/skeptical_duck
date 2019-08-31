
(* 

#use"Compilation_management/coma_state_field.ml";;

Here, we put soma wrappers so that all direct manipulations of
the Coma_state_t.t datatype should be done here.

*)


(* Converters *)
let of_t x=x;;
let to_t x=x;;
(*
in debug mode, change the above to
let of_t (Coma_state_t.CS x)=x;;
let to_t x=(Coma_state_t.CS x);;
*)
(* End of converters *)

(* Temporary converters *)

let temporary_cvrtr_dead_to_alive cs small_array = 
   let l_mod = Small_array.to_list((of_t cs).Coma_state_t.modules) in 
   List.combine l_mod (Small_array.to_list small_array);;

let temporary_cvrtr_alive_to_dead assoc_list = 
   Small_array.of_list (Image.image snd assoc_list);;


(* End of temporary converters *)

let root cs=(of_t cs).Coma_state_t.root;;
let backup_dir cs=(of_t cs).Coma_state_t.dir_for_backup;;
let push_after_backup cs=(of_t cs).Coma_state_t.push_after_backup;;   

let module_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.modules k ;;
let subdir_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.subdir_for_module k ;;

let subdir_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.subdir_for_module);;

let principal_ending_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.principal_ending_for_module k ;;

let principal_ending_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.principal_ending_for_module);;

let mli_presence_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.mli_presence_for_module k ;;

let mli_presence_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.mli_presence_for_module);;

let principal_mt_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.principal_mt_for_module k ;;

let principal_mt_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.principal_mt_for_module);;

let mli_mt_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.mli_mt_for_module k ;;

let mli_mt_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.mli_mt_for_module);;

let needed_libs_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.needed_libs_for_module k ;;

let needed_libs_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.needed_libs_for_module);;

let direct_fathers_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.direct_fathers_for_module k;;

let direct_fathers_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.direct_fathers_for_module);;

let ancestors_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.ancestors_for_module k ;; 

let ancestors_at_module cs mn=
   List.assoc mn ( temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.ancestors_for_module);;


let needed_dirs_at_module cs mn=
   List.assoc mn  ((of_t cs).Coma_state_t.needed_dirs_for_module);;

let product_up_to_date_at_module cs mn=
    List.assoc mn ((of_t cs).Coma_state_t.product_up_to_date_for_module);;    

let directories cs=(of_t cs).Coma_state_t.directories;;
let preq_types cs=(of_t cs).Coma_state_t.printer_equipped_types;;


let modules cs= (of_t cs).Coma_state_t.modules;;


let all_used_subdirs cs =
   let current_assoc = temporary_cvrtr_dead_to_alive cs (of_t cs).Coma_state_t.subdir_for_module in 
   Image.image snd current_assoc ;;



(* Setters  *)


let set_push_after_backup cs bowl = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.push_after_backup=bowl });;

let set_module_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.modules=
                                  (Small_array.set ccs.Coma_state_t.modules k v) });;

let set_subdir_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.subdir_for_module=
                                  (Small_array.set ccs.Coma_state_t.subdir_for_module k v) });;

let set_subdir_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.subdir_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.subdir_for_module=temporary_cvrtr_alive_to_dead new_assocs });;
    
let set_principal_ending_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.principal_ending_for_module=
                                  (Small_array.set ccs.Coma_state_t.principal_ending_for_module k v) });;

let set_principal_ending_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.principal_ending_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.principal_ending_for_module=temporary_cvrtr_alive_to_dead new_assocs });;


let set_mli_presence_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.mli_presence_for_module=
                                  (Small_array.set ccs.Coma_state_t.mli_presence_for_module k v) });;

let set_mli_presence_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.mli_presence_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.mli_presence_for_module=temporary_cvrtr_alive_to_dead new_assocs });;

let set_principal_mt_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.principal_mt_for_module=
                                  (Small_array.set ccs.Coma_state_t.principal_mt_for_module k v) });;

let set_principal_mt_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.principal_mt_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.principal_mt_for_module=temporary_cvrtr_alive_to_dead new_assocs });;

let set_mli_mt_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.mli_mt_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.mli_mt_for_module=temporary_cvrtr_alive_to_dead new_assocs });;

let set_needed_libs_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.needed_libs_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.needed_libs_for_module=temporary_cvrtr_alive_to_dead new_assocs });;


let set_direct_fathers_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.direct_fathers_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.direct_fathers_for_module=temporary_cvrtr_alive_to_dead new_assocs });;



let set_ancestors_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = temporary_cvrtr_dead_to_alive cs ccs.Coma_state_t.ancestors_for_module in 
    let new_assocs=Associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Coma_state_t.ancestors_for_module=temporary_cvrtr_alive_to_dead new_assocs });;


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

let modify_all_subdirs cs f =
   let ccs=of_t cs in 
   let old_subdirs = temporary_cvrtr_dead_to_alive cs ((of_t cs).Coma_state_t.subdir_for_module) in 
   let new_subdirs = Image.image (fun (key,vaal)->(key,f vaal)) old_subdirs in 
   to_t({ccs with Coma_state_t.subdir_for_module= temporary_cvrtr_alive_to_dead new_subdirs });;

let modify_all_needed_dirs cs f =
   let ccs=of_t cs in 
   let old_needed_dirs = ((of_t cs).Coma_state_t.needed_dirs_for_module) in 
   let new_needed_dirs = Image.image (fun (key,vaal)->(key,Image.image f vaal)) old_needed_dirs in 
   to_t({ccs with Coma_state_t.needed_dirs_for_module= new_needed_dirs });;

(* End of adhoc setters *)

let empty_one x y b=to_t({
     Coma_state_t.root =x;
     dir_for_backup =y;
     push_after_backup=b;
     modules = Small_array.of_list [];
     subdir_for_module = Small_array.of_list [] ;
     principal_ending_for_module = Small_array.of_list [] ;
     mli_presence_for_module = Small_array.of_list [] ;
     principal_mt_for_module = Small_array.of_list [] ;
     mli_mt_for_module = Small_array.of_list [] ;
     needed_libs_for_module = Small_array.of_list [] ;
     direct_fathers_for_module = Small_array.of_list [];
     ancestors_for_module = Small_array.of_list [] ; 
     needed_dirs_for_module = [];
     product_up_to_date_for_module = [];
     directories =[];
     printer_equipped_types =[];
});;


let remove_in_each_at_index wrapped_cs idx=
    let cs=of_t wrapped_cs in
    let mname = Small_array.get (cs.Coma_state_t.modules) idx in
    let new_modules = Small_array.remove_item_at_index cs.Coma_state_t.modules idx 
    and new_subdirs = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.subdir_for_module) mname
    and new_principal_endings = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_ending_for_module) mname
    and new_mli_presences = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_presence_for_module) mname
    and new_principal_mts = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_mt_for_module) mname
    and new_mli_mts = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_mt_for_module) mname
    and new_needed_libs = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.needed_libs_for_module) mname
    and new_direct_fathers = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.direct_fathers_for_module) mname
    and new_ancestors = Associative_list.remove_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.ancestors_for_module) mname
    and new_needed_dirs = Associative_list.remove_key (cs.Coma_state_t.needed_dirs_for_module) mname  
    and new_products_up_to_date = Associative_list.remove_key  cs.Coma_state_t.product_up_to_date_for_module mname  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module= temporary_cvrtr_alive_to_dead  new_subdirs;
      Coma_state_t.principal_ending_for_module= temporary_cvrtr_alive_to_dead  new_principal_endings;
      Coma_state_t.mli_presence_for_module= temporary_cvrtr_alive_to_dead  new_mli_presences;
      Coma_state_t.principal_mt_for_module= temporary_cvrtr_alive_to_dead  new_principal_mts;
      Coma_state_t.mli_mt_for_module= temporary_cvrtr_alive_to_dead  new_mli_mts;
      Coma_state_t.needed_libs_for_module= temporary_cvrtr_alive_to_dead  new_needed_libs;
      Coma_state_t.direct_fathers_for_module= temporary_cvrtr_alive_to_dead  new_direct_fathers;
      Coma_state_t.ancestors_for_module= temporary_cvrtr_alive_to_dead  new_ancestors;
      Coma_state_t.needed_dirs_for_module= new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;

let push_right_in_each wrapped_cs (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Dfn_endingless.to_module hm
    and subdir=Dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Small_array.push_right cs.Coma_state_t.modules nm 
    and new_subdirs = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.subdir_for_module) @[nm,subdir]
    and new_principal_endings = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.principal_ending_for_module) @[nm,pr_end] 
    and new_mli_presences = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.mli_presence_for_module) @[nm,mlip] 
    and new_principal_mts = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.principal_mt_for_module) @[nm,prmt] 
    and new_mli_mts = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.mli_mt_for_module) @[nm,mlimt] 
    and new_needed_libs = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.needed_libs_for_module) @[nm,libned] 
    and new_direct_fathers = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.direct_fathers_for_module) @[nm,dirfath]
    and new_ancestors = (temporary_cvrtr_dead_to_alive cs   cs.Coma_state_t.ancestors_for_module) @[nm,allanc] 
    and new_needed_dirs = (cs.Coma_state_t.needed_dirs_for_module)@[nm,dirned] 
    and new_products_up_to_date = (cs.Coma_state_t.product_up_to_date_for_module)@[nm,upy]  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module= temporary_cvrtr_alive_to_dead  new_subdirs;
      Coma_state_t.principal_ending_for_module= temporary_cvrtr_alive_to_dead  new_principal_endings;
      Coma_state_t.mli_presence_for_module= temporary_cvrtr_alive_to_dead  new_mli_presences;
      Coma_state_t.principal_mt_for_module= temporary_cvrtr_alive_to_dead  new_principal_mts;
      Coma_state_t.mli_mt_for_module= temporary_cvrtr_alive_to_dead  new_mli_mts;
      Coma_state_t.needed_libs_for_module= temporary_cvrtr_alive_to_dead  new_needed_libs;
      Coma_state_t.direct_fathers_for_module= temporary_cvrtr_alive_to_dead  new_direct_fathers;
      Coma_state_t.ancestors_for_module= temporary_cvrtr_alive_to_dead  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;

let set_in_each wrapped_cs idx (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Dfn_endingless.to_module hm
    and subdir=Dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Small_array.set cs.Coma_state_t.modules idx nm 
    and new_subdirs = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.subdir_for_module) (nm,subdir)
    and new_principal_endings = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_ending_for_module) (nm,pr_end) 
    and new_mli_presences = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_presence_for_module) (nm, mlip) 
    and new_principal_mts = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_mt_for_module) (nm,prmt) 
    and new_mli_mts = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_mt_for_module) (nm,mlimt) 
    and new_needed_libs = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.needed_libs_for_module) (nm,libned) 
    and new_direct_fathers = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.direct_fathers_for_module) (nm,dirfath)
    and new_ancestors = Associative_list.change_value_for_key (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.ancestors_for_module) (nm,allanc) 
    and new_needed_dirs = Associative_list.change_value_for_key (cs.Coma_state_t.needed_dirs_for_module) (nm,dirned) 
    and new_products_up_to_date = Associative_list.change_value_for_key  cs.Coma_state_t.product_up_to_date_for_module (nm,upy)  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module= temporary_cvrtr_alive_to_dead  new_subdirs;
      Coma_state_t.principal_ending_for_module= temporary_cvrtr_alive_to_dead  new_principal_endings;
      Coma_state_t.mli_presence_for_module= temporary_cvrtr_alive_to_dead  new_mli_presences;
      Coma_state_t.principal_mt_for_module= temporary_cvrtr_alive_to_dead  new_principal_mts;
      Coma_state_t.mli_mt_for_module= temporary_cvrtr_alive_to_dead  new_mli_mts;
      Coma_state_t.needed_libs_for_module= temporary_cvrtr_alive_to_dead  new_needed_libs;
      Coma_state_t.direct_fathers_for_module= temporary_cvrtr_alive_to_dead  new_direct_fathers;
      Coma_state_t.ancestors_for_module= temporary_cvrtr_alive_to_dead  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;
  

    
let reposition_in_each wrapped_cs idx1 idx2=
    let rep=(fun elt->
    Small_array.reposition_by_putting_snd_immediately_after_fst elt idx1 idx2) 
    and cs=of_t wrapped_cs in
    let mn1 = Small_array.get cs.Coma_state_t.modules idx1 
    and mn2 = Small_array.get cs.Coma_state_t.modules idx2 in 
    let l_rep=(fun l->Associative_list.reposition_by_putting_snd_immediately_after_fst l mn1 mn2 ) in 
    let new_modules = rep cs.Coma_state_t.modules  
    and new_subdirs = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.subdir_for_module) 
    and new_principal_endings = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_ending_for_module) 
    and new_mli_presences = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_presence_for_module) 
    and new_principal_mts = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_mt_for_module) 
    and new_mli_mts = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_mt_for_module) 
    and new_needed_libs = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.direct_fathers_for_module) 
    and new_ancestors = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.ancestors_for_module) 
    and new_needed_dirs = l_rep (cs.Coma_state_t.needed_dirs_for_module)
    and new_products_up_to_date = l_rep cs.Coma_state_t.product_up_to_date_for_module in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module= temporary_cvrtr_alive_to_dead  new_subdirs;
      Coma_state_t.principal_ending_for_module= temporary_cvrtr_alive_to_dead  new_principal_endings;
      Coma_state_t.mli_presence_for_module= temporary_cvrtr_alive_to_dead  new_mli_presences;
      Coma_state_t.principal_mt_for_module= temporary_cvrtr_alive_to_dead  new_principal_mts;
      Coma_state_t.mli_mt_for_module= temporary_cvrtr_alive_to_dead  new_mli_mts;
      Coma_state_t.needed_libs_for_module= temporary_cvrtr_alive_to_dead  new_needed_libs;
      Coma_state_t.direct_fathers_for_module= temporary_cvrtr_alive_to_dead  new_direct_fathers;
      Coma_state_t.ancestors_for_module= temporary_cvrtr_alive_to_dead  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;


let reorder wrapped_cs reordered_list_of_modules =
     let cs=of_t wrapped_cs in 
    let l_rep =(fun l->Associative_list.reorder l reordered_list_of_modules) in    
    let new_modules = Small_array.of_list reordered_list_of_modules
    and new_subdirs = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.subdir_for_module) 
    and new_principal_endings = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_ending_for_module) 
    and new_mli_presences = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_presence_for_module) 
    and new_principal_mts = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_mt_for_module) 
    and new_mli_mts = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_mt_for_module) 
    and new_needed_libs = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.direct_fathers_for_module) 
    and new_ancestors = l_rep (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.ancestors_for_module) 
    and new_needed_dirs = l_rep (cs.Coma_state_t.needed_dirs_for_module) 
    and new_products_up_to_date = l_rep cs.Coma_state_t.product_up_to_date_for_module  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module= temporary_cvrtr_alive_to_dead  new_subdirs;
      Coma_state_t.principal_ending_for_module= temporary_cvrtr_alive_to_dead  new_principal_endings;
      Coma_state_t.mli_presence_for_module= temporary_cvrtr_alive_to_dead  new_mli_presences;
      Coma_state_t.principal_mt_for_module= temporary_cvrtr_alive_to_dead  new_principal_mts;
      Coma_state_t.mli_mt_for_module= temporary_cvrtr_alive_to_dead  new_mli_mts;
      Coma_state_t.needed_libs_for_module= temporary_cvrtr_alive_to_dead  new_needed_libs;
      Coma_state_t.direct_fathers_for_module= temporary_cvrtr_alive_to_dead  new_direct_fathers;
      Coma_state_t.ancestors_for_module= temporary_cvrtr_alive_to_dead  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;  

(* For debugging purposes *)

let sizes wrapped_cs =
    let cs=of_t wrapped_cs in
    [ 
      ["modules",Small_array.size(cs.Coma_state_t.modules)];
      ["subdirs",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.subdir_for_module)];
      ["pr_endings",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_ending_for_module)];
      ["mlis",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_presence_for_module)];
      ["mod_times",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_mt_for_module)];
      ["mli_mod_times",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_mt_for_module)];
      ["needed_libs",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.needed_libs_for_module)];
      ["fathers",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.direct_fathers_for_module)];
      ["ancestors",List.length(temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.ancestors_for_module)];
      ["needed_dirs",List.length(cs.Coma_state_t.needed_dirs_for_module)];
      ["datechecks",List.length(cs.Coma_state_t.product_up_to_date_for_module)];
  ];;


let push_after_in_each wrapped_cs idx (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Dfn_endingless.to_module hm
    and subdir=Dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let pivot= Small_array.get cs.Coma_state_t.modules idx in
    let new_modules = Small_array.push_immediately_after_idx cs.Coma_state_t.modules nm idx  
    and new_subdirs = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.subdir_for_module) (nm,subdir) pivot 
    and new_principal_endings = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_ending_for_module) (nm,pr_end) pivot 
    and new_mli_presences = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_presence_for_module) (nm,mlip) pivot 
    and new_principal_mts = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.principal_mt_for_module) (nm,prmt) pivot 
    and new_mli_mts = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.mli_mt_for_module) (nm,mlimt) pivot 
    and new_needed_libs = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.needed_libs_for_module) (nm,libned) pivot 
    and new_direct_fathers = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.direct_fathers_for_module) (nm,dirfath) pivot 
    and new_ancestors = Associative_list.push_immediately_after (temporary_cvrtr_dead_to_alive cs cs.Coma_state_t.ancestors_for_module) (nm,allanc) pivot 
    and new_needed_dirs = Associative_list.push_immediately_after (cs.Coma_state_t.needed_dirs_for_module) (nm,dirned) pivot
    and new_products_up_to_date = Associative_list.push_immediately_after cs.Coma_state_t.product_up_to_date_for_module (nm,upy) pivot  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = temporary_cvrtr_alive_to_dead  new_subdirs;
      Coma_state_t.principal_ending_for_module = temporary_cvrtr_alive_to_dead  new_principal_endings;
      Coma_state_t.mli_presence_for_module = temporary_cvrtr_alive_to_dead  new_mli_presences;
      Coma_state_t.principal_mt_for_module = temporary_cvrtr_alive_to_dead  new_principal_mts;
      Coma_state_t.mli_mt_for_module = temporary_cvrtr_alive_to_dead  new_mli_mts;
      Coma_state_t.needed_libs_for_module = temporary_cvrtr_alive_to_dead  new_needed_libs;
      Coma_state_t.direct_fathers_for_module = temporary_cvrtr_alive_to_dead  new_direct_fathers;
      Coma_state_t.ancestors_for_module = temporary_cvrtr_alive_to_dead  new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;
    



module Private = struct 

let salt = "Coma_"^"state_field.";;

let root_label                          = salt ^ "root";;
let dir_for_backup_label                = salt ^ "dir_for_backup";;
let push_after_backup_label             = salt ^ "push_after_backup";;
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

let cr_of_pair f l= Concrete_object_field.of_pair_list  Dfa_module.to_concrete_object f l;;
let cr_to_pair f crobj= Concrete_object_field.to_pair_list  Dfa_module.of_concrete_object f crobj;;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   let jub =(fun f x y->Small_array.of_concrete_object x y) in 
   {
      Coma_state_t.root = Dfa_root.of_concrete_object(g root_label);
      dir_for_backup = Dfa_root.of_concrete_object(g dir_for_backup_label);
      push_after_backup = Concrete_object_field.to_bool (g push_after_backup_label);
      modules = Small_array.of_concrete_object Dfa_module.of_concrete_object (g modules_label);
      subdir_for_module = jub cr_to_pair Dfa_subdirectory.of_concrete_object (g subdir_for_module_label);
      principal_ending_for_module = jub cr_to_pair Dfa_ending.of_concrete_object (g principal_ending_for_module_label);
      mli_presence_for_module = jub cr_to_pair Concrete_object_field.to_bool (g mli_presence_for_module_label);
      principal_mt_for_module = jub cr_to_pair Concrete_object_field.unwrap_string (g principal_mt_for_module_label);
      mli_mt_for_module = jub cr_to_pair Concrete_object_field.unwrap_string (g mli_mt_for_module_label);
      needed_libs_for_module = jub cr_to_pair (Concrete_object_field.to_list Ocaml_library.of_concrete_object) (g needed_libs_for_module_label);
      direct_fathers_for_module = jub cr_to_pair (Concrete_object_field.to_list Dfa_module.of_concrete_object) (g direct_fathers_for_module_label);
      ancestors_for_module = jub cr_to_pair (Concrete_object_field.to_list Dfa_module.of_concrete_object) (g ancestors_for_module_label); 
      needed_dirs_for_module = cr_to_pair (Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object) (g needed_dirs_for_module_label);
      product_up_to_date_for_module = cr_to_pair Concrete_object_field.to_bool (g product_up_to_date_for_module_label);
      directories = (Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object)  (g directories_label);
      printer_equipped_types = Concrete_object_field.to_pair_list 
                                      Dfn_endingless.of_concrete_object
                                      Concrete_object_field.to_bool (g printer_equipped_types_label);
   };; 

let to_concrete_object cs=
   let jub =(fun f x y->Small_array.to_concrete_object x y) in 
   let items= 
   [
    root_label, Dfa_root.to_concrete_object cs.Coma_state_t.root;
    dir_for_backup_label, Dfa_root.to_concrete_object cs.Coma_state_t.dir_for_backup;
    push_after_backup_label, Concrete_object_field.of_bool cs.Coma_state_t.push_after_backup;
    modules_label, Small_array.to_concrete_object Dfa_module.to_concrete_object cs.Coma_state_t.modules;
    subdir_for_module_label, jub cr_of_pair Dfa_subdirectory.to_concrete_object cs.Coma_state_t.subdir_for_module;
    principal_ending_for_module_label, jub cr_of_pair Dfa_ending.to_concrete_object cs.Coma_state_t.principal_ending_for_module;
    mli_presence_for_module_label, jub cr_of_pair Concrete_object_field.of_bool cs.Coma_state_t.mli_presence_for_module;  
    principal_mt_for_module_label, jub cr_of_pair (fun s->Concrete_object_t.String s) cs.Coma_state_t.principal_mt_for_module;
    mli_mt_for_module_label, jub cr_of_pair (fun s->Concrete_object_t.String s) cs.Coma_state_t.mli_mt_for_module;
    needed_libs_for_module_label, jub cr_of_pair (Concrete_object_field.of_list Ocaml_library.to_concrete_object) cs.Coma_state_t.needed_libs_for_module; 
    direct_fathers_for_module_label, jub cr_of_pair (Concrete_object_field.of_list Dfa_module.to_concrete_object) cs.Coma_state_t.direct_fathers_for_module;   
    ancestors_for_module_label, jub cr_of_pair (Concrete_object_field.of_list Dfa_module.to_concrete_object) cs.Coma_state_t.ancestors_for_module;   
    needed_dirs_for_module_label, cr_of_pair (Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object)  (cs.Coma_state_t.needed_dirs_for_module);  
    product_up_to_date_for_module_label, cr_of_pair Concrete_object_field.of_bool cs.Coma_state_t.product_up_to_date_for_module; 
    directories_label,  (Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object) cs.Coma_state_t.directories; 
    printer_equipped_types_label,  Concrete_object_field.of_pair_list 
                                      Dfn_endingless.to_concrete_object
                                      Concrete_object_field.of_bool cs.Coma_state_t.printer_equipped_types;    
   ]  in
   Concrete_object_t.Record items;;


end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


