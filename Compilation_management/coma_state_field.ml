
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

let set_push_after_backup cs bowl = let ccs=of_t cs in 
                            to_t({ccs with Coma_state_t.push_after_backup=bowl });;

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

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Coma_state_t.root = Root_directory.of_concrete_object(g root_label);
      dir_for_backup = Root_directory.of_concrete_object(g dir_for_backup_label);
      push_after_backup = Concrete_object_field.to_bool (g push_after_backup_label);
      modules = Small_array.of_concrete_object Naked_module.of_concrete_object (g modules_label);
      subdir_for_module = Small_array.of_concrete_object Subdirectory.of_concrete_object (g subdir_for_module_label);
      principal_ending_for_module = Small_array.of_concrete_object Ocaml_ending.of_concrete_object (g principal_ending_for_module_label);
      mli_presence_for_module = Small_array.of_concrete_object Concrete_object_field.to_bool (g mli_presence_for_module_label);
      principal_mt_for_module = Small_array.of_concrete_object Concrete_object_field.unwrap_string (g principal_mt_for_module_label);
      mli_mt_for_module = Small_array.of_concrete_object Concrete_object_field.unwrap_string (g mli_mt_for_module_label);
      needed_libs_for_module = Small_array.of_concrete_object Ocaml_library.list_of_concrete_object (g needed_libs_for_module_label);
      direct_fathers_for_module = Small_array.of_concrete_object Naked_module.list_of_concrete_object (g direct_fathers_for_module_label);
      ancestors_for_module = Small_array.of_concrete_object Naked_module.list_of_concrete_object (g ancestors_for_module_label); 
      needed_dirs_for_module = Small_array.of_concrete_object Subdirectory.list_of_concrete_object (g needed_dirs_for_module_label);
      product_up_to_date_for_module = Small_array.of_concrete_object Concrete_object_field.to_bool (g product_up_to_date_for_module_label);
      directories = Subdirectory.list_of_concrete_object  (g directories_label);
      printer_equipped_types = Half_dressed_module.list_of_pairs_of_concrete_object (g printer_equipped_types_label);
   };; 


let to_concrete_object cs=
   let items= 
   [
    root_label, Root_directory.to_concrete_object cs.Coma_state_t.root;
    dir_for_backup_label, Root_directory.to_concrete_object cs.Coma_state_t.dir_for_backup;
    push_after_backup_label, Concrete_object_field.of_bool cs.Coma_state_t.push_after_backup;
    modules_label, Small_array.to_concrete_object Naked_module.to_concrete_object cs.Coma_state_t.modules;
    subdir_for_module_label, Small_array.to_concrete_object Subdirectory.to_concrete_object cs.Coma_state_t.subdir_for_module;
    principal_ending_for_module_label, Small_array.to_concrete_object Ocaml_ending.to_concrete_object cs.Coma_state_t.principal_ending_for_module;
    mli_presence_for_module_label, Small_array.to_concrete_object Concrete_object_field.of_bool cs.Coma_state_t.mli_presence_for_module;  
    principal_mt_for_module_label, Small_array.to_concrete_object (fun s->Concrete_object_t.String s) cs.Coma_state_t.principal_mt_for_module;
    mli_mt_for_module_label, Small_array.to_concrete_object (fun s->Concrete_object_t.String s) cs.Coma_state_t.mli_mt_for_module;
    needed_libs_for_module_label, Small_array.to_concrete_object Ocaml_library.list_to_concrete_object cs.Coma_state_t.needed_libs_for_module; 
    direct_fathers_for_module_label, Small_array.to_concrete_object Naked_module.list_to_concrete_object cs.Coma_state_t.direct_fathers_for_module;   
    ancestors_for_module_label, Small_array.to_concrete_object Naked_module.list_to_concrete_object cs.Coma_state_t.ancestors_for_module;   
    needed_dirs_for_module_label, Small_array.to_concrete_object Subdirectory.list_to_concrete_object cs.Coma_state_t.needed_dirs_for_module;  
    product_up_to_date_for_module_label, Small_array.to_concrete_object Concrete_object_field.of_bool cs.Coma_state_t.product_up_to_date_for_module; 
    directories_label,  Subdirectory.list_to_concrete_object cs.Coma_state_t.directories; 
    printer_equipped_types_label,  Half_dressed_module.list_of_pairs_to_concrete_object cs.Coma_state_t.printer_equipped_types;    
   ]  in
   Concrete_object_t.Record items;;


end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


