
(* 

#use"Makefile_makers/coma_state_field.ml";;

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


let root cs=(of_t cs).Coma_state_t.root;;
let backup_dir cs=(of_t cs).Coma_state_t.dir_for_backup;;
let push_after_backup cs=(of_t cs).Coma_state_t.push_after_backup;;   

let module_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.modules k ;;
let subdir_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.subdir_for_module k ;;
let principal_ending_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.principal_ending_for_module k ;;
let mli_presence_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.mli_presence_for_module k ;;
let principal_mt_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.principal_mt_for_module k ;;
let mli_mt_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.mli_mt_for_module k ;;
let needed_libs_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.needed_libs_for_module k ;;
let direct_fathers_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.direct_fathers_for_module k;;
let ancestors_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.ancestors_for_module k ;; 
let needed_dirs_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.needed_dirs_for_module k ;;
let product_up_to_date_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.product_up_to_date_for_module k ;;
let directories cs=(of_t cs).Coma_state_t.directories;;
let preq_types cs=(of_t cs).Coma_state_t.printer_equipped_types;;


let modules cs= (of_t cs).Coma_state_t.modules;;
let subdirs cs= (of_t cs).Coma_state_t.subdir_for_module;;
let needed_dirs cs= (of_t cs).Coma_state_t.needed_dirs_for_module;;

(* Setters  *)


let set_module_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.modules=
                                  (Small_array.set ccs.Coma_state_t.modules k v) });;

let set_subdir_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.subdir_for_module=
                                  (Small_array.set ccs.Coma_state_t.subdir_for_module k v) });;


let set_principal_ending_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.principal_ending_for_module=
                                  (Small_array.set ccs.Coma_state_t.principal_ending_for_module k v) });;


let set_mli_presence_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.mli_presence_for_module=
                                  (Small_array.set ccs.Coma_state_t.mli_presence_for_module k v) });;


let set_principal_mt_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.principal_mt_for_module=
                                  (Small_array.set ccs.Coma_state_t.principal_mt_for_module k v) });;


let set_mli_mt_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.mli_mt_for_module=
                                  (Small_array.set ccs.Coma_state_t.mli_mt_for_module k v) });;


let set_needed_libs_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.needed_libs_for_module=
                                  (Small_array.set ccs.Coma_state_t.needed_libs_for_module k v) });;


let set_direct_fathers_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.direct_fathers_for_module=
                                  (Small_array.set ccs.Coma_state_t.direct_fathers_for_module k v) });;


let set_ancestors_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.ancestors_for_module=
                                  (Small_array.set ccs.Coma_state_t.ancestors_for_module k v) });;


let set_needed_dirs_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.needed_dirs_for_module=
                                  (Small_array.set ccs.Coma_state_t.needed_dirs_for_module k v) });;


let set_product_up_to_date_at_idx cs k v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.product_up_to_date_for_module=
                                  (Small_array.set ccs.Coma_state_t.product_up_to_date_for_module k v) });;


let set_directories cs v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.directories=v});;


let set_preq_types cs v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.printer_equipped_types=v});;


let set_subdirs cs  v =     let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.subdir_for_module=v});;
let set_needed_dirs cs  v = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.needed_dirs_for_module=v});;


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
     needed_dirs_for_module = Small_array.of_list [];
     product_up_to_date_for_module = Small_array.of_list [];
     directories =[];
     printer_equipped_types =[];
});;


let remove_in_each_at_index wrapped_cs idx=
    let cs=of_t wrapped_cs in
    let new_modules = Small_array.remove_item_at_index cs.Coma_state_t.modules idx 
    and new_subdirs = Small_array.remove_item_at_index cs.Coma_state_t.subdir_for_module idx 
    and new_principal_endings = Small_array.remove_item_at_index cs.Coma_state_t.principal_ending_for_module idx 
    and new_mli_presences = Small_array.remove_item_at_index cs.Coma_state_t.mli_presence_for_module idx 
    and new_principal_mts = Small_array.remove_item_at_index cs.Coma_state_t.principal_mt_for_module idx 
    and new_mli_mts = Small_array.remove_item_at_index cs.Coma_state_t.mli_mt_for_module idx 
    and new_needed_libs = Small_array.remove_item_at_index cs.Coma_state_t.needed_libs_for_module idx 
    and new_direct_fathers = Small_array.remove_item_at_index cs.Coma_state_t.direct_fathers_for_module idx 
    and new_ancestors = Small_array.remove_item_at_index cs.Coma_state_t.ancestors_for_module idx 
    and new_needed_dirs = Small_array.remove_item_at_index cs.Coma_state_t.needed_dirs_for_module idx 
    and new_products_up_to_date = Small_array.remove_item_at_index cs.Coma_state_t.product_up_to_date_for_module idx  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;

let push_right_in_each wrapped_cs (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Small_array.push_right cs.Coma_state_t.modules nm 
    and new_subdirs = Small_array.push_right cs.Coma_state_t.subdir_for_module subdir
    and new_principal_endings = Small_array.push_right cs.Coma_state_t.principal_ending_for_module pr_end 
    and new_mli_presences = Small_array.push_right cs.Coma_state_t.mli_presence_for_module mlip 
    and new_principal_mts = Small_array.push_right cs.Coma_state_t.principal_mt_for_module prmt 
    and new_mli_mts = Small_array.push_right cs.Coma_state_t.mli_mt_for_module mlimt 
    and new_needed_libs = Small_array.push_right cs.Coma_state_t.needed_libs_for_module libned 
    and new_direct_fathers = Small_array.push_right cs.Coma_state_t.direct_fathers_for_module dirfath
    and new_ancestors = Small_array.push_right cs.Coma_state_t.ancestors_for_module allanc 
    and new_needed_dirs = Small_array.push_right cs.Coma_state_t.needed_dirs_for_module dirned 
    and new_products_up_to_date = Small_array.push_right cs.Coma_state_t.product_up_to_date_for_module upy  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;

let set_in_each wrapped_cs idx (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Small_array.set cs.Coma_state_t.modules idx nm 
    and new_subdirs = Small_array.set cs.Coma_state_t.subdir_for_module idx subdir
    and new_principal_endings = Small_array.set cs.Coma_state_t.principal_ending_for_module idx pr_end 
    and new_mli_presences = Small_array.set cs.Coma_state_t.mli_presence_for_module idx  mlip 
    and new_principal_mts = Small_array.set cs.Coma_state_t.principal_mt_for_module idx prmt 
    and new_mli_mts = Small_array.set cs.Coma_state_t.mli_mt_for_module idx mlimt 
    and new_needed_libs = Small_array.set cs.Coma_state_t.needed_libs_for_module idx libned 
    and new_direct_fathers = Small_array.set cs.Coma_state_t.direct_fathers_for_module idx dirfath
    and new_ancestors = Small_array.set cs.Coma_state_t.ancestors_for_module idx allanc 
    and new_needed_dirs = Small_array.set cs.Coma_state_t.needed_dirs_for_module idx dirned 
    and new_products_up_to_date = Small_array.set cs.Coma_state_t.product_up_to_date_for_module idx upy  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;
  
let push_after_in_each wrapped_cs idx (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Small_array.push_immediately_after_idx cs.Coma_state_t.modules nm idx  
    and new_subdirs = Small_array.push_immediately_after_idx cs.Coma_state_t.subdir_for_module subdir idx 
    and new_principal_endings = Small_array.push_immediately_after_idx cs.Coma_state_t.principal_ending_for_module pr_end idx 
    and new_mli_presences = Small_array.push_immediately_after_idx cs.Coma_state_t.mli_presence_for_module mlip idx
    and new_principal_mts = Small_array.push_immediately_after_idx cs.Coma_state_t.principal_mt_for_module prmt idx
    and new_mli_mts = Small_array.push_immediately_after_idx cs.Coma_state_t.mli_mt_for_module mlimt idx
    and new_needed_libs = Small_array.push_immediately_after_idx cs.Coma_state_t.needed_libs_for_module libned idx
    and new_direct_fathers = Small_array.push_immediately_after_idx cs.Coma_state_t.direct_fathers_for_module dirfath idx
    and new_ancestors = Small_array.push_immediately_after_idx cs.Coma_state_t.ancestors_for_module allanc idx
    and new_needed_dirs = Small_array.push_immediately_after_idx cs.Coma_state_t.needed_dirs_for_module dirned idx
    and new_products_up_to_date = Small_array.push_immediately_after_idx cs.Coma_state_t.product_up_to_date_for_module upy idx  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;
    
let reposition_in_each wrapped_cs idx1 idx2=
    let rep=(fun elt->
    Small_array.reposition_by_putting_snd_immediately_after_fst elt idx1 idx2) 
    and cs=of_t wrapped_cs in
    let new_modules = rep cs.Coma_state_t.modules  
    and new_subdirs = rep cs.Coma_state_t.subdir_for_module 
    and new_principal_endings = rep cs.Coma_state_t.principal_ending_for_module 
    and new_mli_presences = rep cs.Coma_state_t.mli_presence_for_module 
    and new_principal_mts = rep cs.Coma_state_t.principal_mt_for_module 
    and new_mli_mts = rep cs.Coma_state_t.mli_mt_for_module 
    and new_needed_libs = rep cs.Coma_state_t.needed_libs_for_module 
    and new_direct_fathers = rep cs.Coma_state_t.direct_fathers_for_module 
    and new_ancestors = rep cs.Coma_state_t.ancestors_for_module 
    and new_needed_dirs = rep cs.Coma_state_t.needed_dirs_for_module 
    and new_products_up_to_date = rep cs.Coma_state_t.product_up_to_date_for_module in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;


let reorder wrapped_cs reordered_list_of_modules =
     let cs=of_t wrapped_cs in
     let old_modules=Small_array.to_list (cs.Coma_state_t.modules) in 
     let indices = Image.image (
        fun elt-> Listennou.find_index elt old_modules
      ) reordered_list_of_modules in 
     let n=List.length old_modules in  
     let rep=(
        fun field->
          let temp1=Ennig.doyle (fun k->
             Small_array.get field (List.nth indices (k-1))
          ) 1 n in 
          Small_array.of_list temp1
      ) in 
    let new_modules = Small_array.of_list reordered_list_of_modules
    and new_subdirs = rep cs.Coma_state_t.subdir_for_module 
    and new_principal_endings = rep cs.Coma_state_t.principal_ending_for_module 
    and new_mli_presences = rep cs.Coma_state_t.mli_presence_for_module 
    and new_principal_mts = rep cs.Coma_state_t.principal_mt_for_module 
    and new_mli_mts = rep cs.Coma_state_t.mli_mt_for_module 
    and new_needed_libs = rep cs.Coma_state_t.needed_libs_for_module 
    and new_direct_fathers = rep cs.Coma_state_t.direct_fathers_for_module 
    and new_ancestors = rep cs.Coma_state_t.ancestors_for_module 
    and new_needed_dirs = rep cs.Coma_state_t.needed_dirs_for_module 
    and new_products_up_to_date = rep cs.Coma_state_t.product_up_to_date_for_module in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;  

(* For debugging purposes *)

let sizes wrapped_cs =
    let cs=of_t wrapped_cs in
    [ 
      ["modules",Small_array.size(cs.Coma_state_t.modules)];
      ["subdirs",Small_array.size(cs.Coma_state_t.subdir_for_module)];
      ["pr_endings",Small_array.size(cs.Coma_state_t.principal_ending_for_module)];
      ["mlis",Small_array.size(cs.Coma_state_t.mli_presence_for_module)];
      ["mod_times",Small_array.size(cs.Coma_state_t.principal_mt_for_module)];
      ["mli_mod_times",Small_array.size(cs.Coma_state_t.mli_mt_for_module)];
      ["needed_libs",Small_array.size(cs.Coma_state_t.needed_libs_for_module)];
      ["fathers",Small_array.size(cs.Coma_state_t.direct_fathers_for_module)];
      ["ancestors",Small_array.size(cs.Coma_state_t.ancestors_for_module)];
      ["needed_dirs",Small_array.size(cs.Coma_state_t.needed_dirs_for_module)];
      ["datechecks",Small_array.size(cs.Coma_state_t.product_up_to_date_for_module)];
  ];;


let push_after_in_each wrapped_cs idx (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Small_array.push_immediately_after_idx cs.Coma_state_t.modules nm idx  
    and new_subdirs = Small_array.push_immediately_after_idx cs.Coma_state_t.subdir_for_module subdir idx 
    and new_principal_endings = Small_array.push_immediately_after_idx cs.Coma_state_t.principal_ending_for_module pr_end idx 
    and new_mli_presences = Small_array.push_immediately_after_idx cs.Coma_state_t.mli_presence_for_module mlip idx
    and new_principal_mts = Small_array.push_immediately_after_idx cs.Coma_state_t.principal_mt_for_module prmt idx
    and new_mli_mts = Small_array.push_immediately_after_idx cs.Coma_state_t.mli_mt_for_module mlimt idx
    and new_needed_libs = Small_array.push_immediately_after_idx cs.Coma_state_t.needed_libs_for_module libned idx
    and new_direct_fathers = Small_array.push_immediately_after_idx cs.Coma_state_t.direct_fathers_for_module dirfath idx
    and new_ancestors = Small_array.push_immediately_after_idx cs.Coma_state_t.ancestors_for_module allanc idx
    and new_needed_dirs = Small_array.push_immediately_after_idx cs.Coma_state_t.needed_dirs_for_module dirned idx
    and new_products_up_to_date = Small_array.push_immediately_after_idx cs.Coma_state_t.product_up_to_date_for_module upy idx  in 
to_t({ cs with 
      Coma_state_t.modules = new_modules;
      Coma_state_t.subdir_for_module = new_subdirs;
      Coma_state_t.principal_ending_for_module = new_principal_endings;
      Coma_state_t.mli_presence_for_module = new_mli_presences;
      Coma_state_t.principal_mt_for_module = new_principal_mts;
      Coma_state_t.mli_mt_for_module = new_mli_mts;
      Coma_state_t.needed_libs_for_module = new_needed_libs;
      Coma_state_t.direct_fathers_for_module = new_direct_fathers;
      Coma_state_t.ancestors_for_module = new_ancestors;
      Coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Coma_state_t.product_up_to_date_for_module = new_products_up_to_date;
});;


let outer_separator=Industrial_separator.modulesystem_data1;; 
let inner_separator=Industrial_separator.modulesystem_data2;; 
      
let archive wrapped_cs=
        let cs=of_t wrapped_cs in
        let (Root_directory_t.R t1)=cs.Coma_state_t.root 
        and (Root_directory_t.R t2)=cs.Coma_state_t.dir_for_backup  
        and  t3=cs.Coma_state_t.push_after_backup
        and  t4=(modules wrapped_cs)
        and  t5=cs.Coma_state_t.subdir_for_module
        and  t6=cs.Coma_state_t.principal_ending_for_module 
        and  t7=cs.Coma_state_t.mli_presence_for_module 
        and  t8=cs.Coma_state_t.principal_mt_for_module 
        and  t9=cs.Coma_state_t.mli_mt_for_module 
        and t10=cs.Coma_state_t.needed_libs_for_module 
        and t11=cs.Coma_state_t.direct_fathers_for_module 
        and t12=cs.Coma_state_t.ancestors_for_module 
        and t13=cs.Coma_state_t.needed_dirs_for_module
        and t14=cs.Coma_state_t.product_up_to_date_for_module
        and t15=cs.Coma_state_t.directories 
        and t16=cs.Coma_state_t.printer_equipped_types in
        let list_arch=(fun old_arch a_list->
        String.concat inner_separator 
           (Image.image old_arch a_list)
        ) in 
        let arrlist_arch=(fun old_arch->
        Small_array.archive (list_arch old_arch)
        ) 
        and id=(fun s->s) in
        String.concat outer_separator
        [
          t1 ;
          t2 ;
          string_of_bool t3;
         Small_array.archive (fun (Naked_module_t.N s)->s) t4;
         Small_array.archive (fun (Subdirectory_t.SD s)->s) t5;
         Small_array.archive Ocaml_ending.to_string t6;
         Small_array.archive string_of_bool t7;
         Small_array.archive id t8;
         Small_array.archive id t9;
         arrlist_arch Ocaml_library.to_string t10;
         arrlist_arch (fun (Naked_module_t.N s)->s) t11;
         arrlist_arch (fun (Naked_module_t.N s)->s) t12;
         arrlist_arch (fun (Subdirectory_t.SD s)->s) t13;
         Small_array.archive string_of_bool t14;
         list_arch (fun w->Subdirectory.without_trailing_slash w) t15;
         list_arch Half_dressed_module.archive_pair t16;
        ];;
      
        
      
      
           
let unarchive s=
          let temp1=Str.split_delim (Str.regexp_string outer_separator) s in
          let  list_unarch=(fun old_unarch s->
            let ttemp2=Str.split_delim
            (Str.regexp_string inner_separator) (s) in
            Image.image old_unarch ttemp2)
          and part=(fun j->List.nth temp1 (j-1)) in
          let arrlist_unarch=(fun old_unarch->
            Small_array.unarchive (list_unarch old_unarch)) 
          and id=(fun s->s) in
          to_t(
         {
          Coma_state_t.root = Root_directory_t.R(part 1);
          dir_for_backup = Root_directory_t.R(part 2);
          push_after_backup = bool_of_string(part 3);
          modules = Small_array.unarchive (fun s->Naked_module_t.N s) (part 4);
          subdir_for_module = Small_array.unarchive (fun s->Subdirectory_t.SD s) (part 5);
          principal_ending_for_module = Small_array.unarchive Ocaml_ending.of_string (part 6) ;
          mli_presence_for_module = Small_array.unarchive bool_of_string (part 7) ;
          principal_mt_for_module = Small_array.unarchive id (part 8) ;
          mli_mt_for_module = Small_array.unarchive id (part 9) ;
          needed_libs_for_module = arrlist_unarch Ocaml_library.of_string (part 10) ;
          direct_fathers_for_module = arrlist_unarch (fun s->Naked_module_t.N s) (part 11);
          ancestors_for_module = arrlist_unarch (fun s->Naked_module_t.N s) (part 12) ; 
          needed_dirs_for_module = arrlist_unarch (fun s->Subdirectory_t.SD s) (part 13);
          product_up_to_date_for_module = Small_array.unarchive bool_of_string (part 14) ;
          directories = list_unarch (fun v->Subdirectory.of_string(v)) (part 15);
          printer_equipped_types = list_unarch Half_dressed_module.unarchive_pair (part 16);
       });; 
      
       
       
      
           