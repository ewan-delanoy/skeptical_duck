(*

#use"lib/Filewatching/fw_modular_infrastructure.ml";;

*)


module Fw_mudolar_infrastructure_t = struct 

  type t = {
    modularized_details :  (Dfa_module_t.t * Fw_module_details_t.t) list ;
    order: (Dfa_module_t.t * (Dfa_module_t.t list * Dfa_module_t.t list)) list;
    needed_dirs :(Dfa_module_t.t * (Dfa_subdirectory_t.t list)) list;
    needed_libs : (Dfa_module_t.t * (Ocaml_library_t.t list)) list;
    all_subdirectories: Dfa_subdirectory_t.t list;
    all_printables : Dfn_middle_t.t list;
    registered_printers : (int * string) list ;
 } ;;
 

end ;;  

module Crobj = struct
  
  let istr_of_concrete_object =
    Crobj_converter_combinator.to_pair 
    Crobj_converter.int_of_concrete_object
    Crobj_converter.string_of_concrete_object ;;

let istr_to_concrete_object =
    Crobj_converter_combinator.of_pair 
    Crobj_converter.int_to_concrete_object
    Crobj_converter.string_to_concrete_object ;;    

let istrl_of_concrete_object =
  Crobj_converter_combinator.to_list istr_of_concrete_object ;;

let istrl_to_concrete_object =
  Crobj_converter_combinator.of_list istr_to_concrete_object ;;  


let salt = "Fw_mudolar_infrastructure_t." ;;
let modularized_details_label = salt ^ "modularized_details" ;;
let order_label               = salt ^ "order" ;;
let needed_dirs_label         = salt ^ "needed_dirs" ;;
let needed_libs_label         = salt ^ "needed_libs" ;;
let all_subdirectories_label  = salt ^ "all_subdirectories" ;;
let all_printables_label      = salt ^ "all_printables" ;;
let registered_printers_label = salt ^ "registered_printers";;





let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj 
  and cl = Crobj_converter_combinator.to_list in 
  let h = (fun converter label ->
   Crobj_converter_combinator.to_pair_list 
   Dfa_module.of_concrete_object converter  
   (g label)
  ) in 
  let hl = (fun elt_converter label ->
   h (Crobj_converter_combinator.to_list elt_converter) label

  )
  and mlist_of_crobj = Crobj_converter_combinator.to_list 
  Dfa_module.of_concrete_object  in 
  let mlpair_of_crobj = Crobj_converter_combinator.to_pair
  mlist_of_crobj mlist_of_crobj in
 {
   Fw_mudolar_infrastructure_t.modularized_details = 
       h Fw_module_details.Crobj.of_concrete_object modularized_details_label;
   order = h mlpair_of_crobj order_label;
   needed_dirs = hl Dfa_subdirectory.of_concrete_object needed_dirs_label;
   needed_libs = hl Ocaml_library.of_concrete_object needed_libs_label;
   all_subdirectories = cl Dfa_subdirectory.of_concrete_object   (g all_subdirectories_label);
   all_printables =  cl Dfn_middle.of_concrete_object  (g all_printables_label);
   registered_printers = istrl_of_concrete_object (g registered_printers_label); 
 } ;;


let to_concrete_object fwd = 
  let cl = Crobj_converter_combinator.of_list in 
  let h = (fun converter pair_list ->
   Crobj_converter_combinator.of_pair_list 
   Dfa_module.to_concrete_object converter  
   pair_list
  ) in 
  let hl = (fun elt_converter complicated_list ->
   h (Crobj_converter_combinator.of_list elt_converter) complicated_list
  )
  and mlist_to_crobj = Crobj_converter_combinator.of_list 
  Dfa_module.to_concrete_object  in 
  let mlpair_to_crobj = Crobj_converter_combinator.of_pair
  mlist_to_crobj mlist_to_crobj in
  let items=
 [
   modularized_details_label, h Fw_module_details.Crobj.to_concrete_object   (fwd.Fw_mudolar_infrastructure_t.modularized_details);
   order_label,  h mlpair_to_crobj   (fwd.Fw_mudolar_infrastructure_t.order);
   needed_dirs_label,  hl Dfa_subdirectory.to_concrete_object   (fwd.Fw_mudolar_infrastructure_t.needed_dirs);
   needed_libs_label,  hl Ocaml_library.to_concrete_object   (fwd.Fw_mudolar_infrastructure_t.needed_libs);
   all_subdirectories_label, cl Dfa_subdirectory.to_concrete_object   (fwd.Fw_mudolar_infrastructure_t.all_subdirectories);
   all_printables_label, cl Dfn_middle.to_concrete_object   (fwd.Fw_mudolar_infrastructure_t.all_printables);
   registered_printers_label, istrl_to_concrete_object (fwd.Fw_mudolar_infrastructure_t.registered_printers);
 ] in 
Concrete_object_t.Record items;;

end ;;  


module Private = struct 

  let starter = 
  {
    Fw_mudolar_infrastructure_t.modularized_details = [] ;
    order = [];
    needed_dirs = [];
    needed_libs = [];
    all_subdirectories = [];
    all_printables = [];
    registered_printers = [];
 } ;;
 
  let compute_closest_order old_order new_details= 
    let modules_in_old_order = Image.image fst old_order in 
    let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
      new_details modules_in_old_order in 
    Fw_determine_order.main  details_in_old_order ;;

  let compute_new_details_in_stable_case new_fw_dets old_details data_on_changes= 
    let tempf = (
        fun old_pair -> 
          let (mn,_) = old_pair in 
          let temp1 = List.filter (fun (rl,_)->
             (Dfn_rootless.to_module rl)= mn
            ) data_on_changes in
          if temp1 <> []
          then 
               (mn, Fw_module_details.recompute_details_for_module (Fwc_with_file_details.file_details new_fw_dets)
                    mn temp1)
          else old_pair 
      ) in 
    Image.image tempf old_details ;; 

   let module_from_printer_path printer_path = 
     let i = String.index printer_path '.' in 
     Dfa_module.of_line (Cull_string.beginning i printer_path) ;; 

   (*
   
    module_from_printer_path "Peggy.Lee.Is.Great" ;;

   *)

  let replace_module_in_printer_path (old_module,new_module) old_printer_path = 
    let i = String.index old_printer_path '.' in 
    let current_module = Dfa_module.of_line (Cull_string.beginning i old_printer_path) in 
    if current_module = old_module 
    then (Dfa_module.capitalized_form new_module) ^ (Cull_string.cobeginning i old_printer_path)  
    else old_printer_path ;;   

  (*
  
  replace_module_in_printer_path 
   (Dfa_module.of_line "peggy",Dfa_module.of_line "debbie")
   "Peggy.Lee.Is.Great" ;;

  replace_module_in_printer_path 
   (Dfa_module.of_line "amy",Dfa_module.of_line "debbie")
   "Peggy.Lee.Is.Great" ;;

  *)  

 module Setter = struct 

  let modularized_details deps_ref new_details = 
    (deps_ref:={(!deps_ref) with 
    Fw_mudolar_infrastructure_t.modularized_details = new_details} );;
  let order deps_ref new_order = 
    (deps_ref:={(!deps_ref) with 
    Fw_mudolar_infrastructure_t.order = new_order} );;

  let needed_dirs deps_ref new_dirs = 
    (deps_ref:={(!deps_ref) with 
    Fw_mudolar_infrastructure_t.needed_dirs = new_dirs} );; 

  let needed_libs deps_ref new_libs = 
    (deps_ref:={(!deps_ref) with 
    Fw_mudolar_infrastructure_t.needed_libs = new_libs} );;


  let all_subdirectories deps_ref new_subdirectories = 
    (deps_ref:={(!deps_ref) with 
    Fw_mudolar_infrastructure_t.all_subdirectories = new_subdirectories} );;

  let all_printables deps_ref new_printables = 
    (deps_ref:={(!deps_ref) with 
    Fw_mudolar_infrastructure_t.all_printables = new_printables} );;

  let registered_printers deps_ref new_printers = 
      (deps_ref:={(!deps_ref) with 
      Fw_mudolar_infrastructure_t.registered_printers = new_printers} );;  
 
  end ;; 


  module Default = struct 


let modularized_details fw_dets deps_ref= 
  let u_files=Fwc_with_file_details.usual_compilable_files fw_dets 
  and small_details = Fwc_with_file_details.file_details fw_dets in 
  let new_details=Fw_module_details.modularize_from_compilable_files_and_small_details 
     u_files small_details in 
  Setter.modularized_details deps_ref new_details;;

let order deps_ref= 
  let old_deps=(!deps_ref) in  
  let new_order=Fw_determine_order.main old_deps.Fw_mudolar_infrastructure_t.modularized_details in 
  Setter.order deps_ref new_order ;;


  let needed_dirs deps_ref= 
    let old_deps=(!deps_ref) in  
    let details = old_deps.Fw_mudolar_infrastructure_t.modularized_details in 
    let subdir_at_module = (fun mn->
      Fw_module_details.subdirectory(List.assoc mn details)
    ) in 
    let new_needed_dirs = Image.image (
      fun (mn,(_,ancestors)) ->
      let temp1 = Image.image subdir_at_module (mn::ancestors) in 
      (mn,Ordered.sort Total_ordering.standard temp1)
    ) (old_deps.Fw_mudolar_infrastructure_t.order) in 
    Setter.needed_dirs deps_ref new_needed_dirs;;

  let needed_libs deps_ref= 
    let old_deps=(!deps_ref) in  
    let details = old_deps.Fw_mudolar_infrastructure_t.modularized_details in 
    let needed_libs_at_module = (fun mn->
      Fw_module_details.used_libraries(List.assoc mn details)
    ) in 
    let new_needed_libs = Image.image (
     fun (mn,(_,ancestors)) ->
      let temp1 = List.flatten(Image.image needed_libs_at_module (mn::ancestors)) in 
      (mn,Ordered.sort Total_ordering.standard temp1)
    ) (old_deps.Fw_mudolar_infrastructure_t.order)  in 
    Setter.needed_libs deps_ref new_needed_libs;;
  let all_subdirectories deps_ref= 
    let old_deps=(!deps_ref) in  
    let details = old_deps.Fw_mudolar_infrastructure_t.modularized_details in 
    let new_subdirectories = Ordered.sort Total_ordering.standard (Image.image (
      fun (_,details_on_mn) ->
      Fw_module_details.subdirectory(details_on_mn)
    ) details)  in 
    Setter.all_subdirectories deps_ref new_subdirectories;;

  let all_printables deps_ref= 
    let old_deps=(!deps_ref) in  
    let details = old_deps.Fw_mudolar_infrastructure_t.modularized_details in 
    let mods_without_subdirs = List.filter_map (
      fun (mn,details) ->
      if Fw_module_details.has_printer details
      then Some mn
      else None
   ) details in 
   let new_printables = Image.image (
    fun mn ->
      let local_details = List.assoc mn details in 
      let subdir = Fw_module_details.subdirectory local_details in 
      Dfn_join.subdirectory_to_module subdir mn
 ) mods_without_subdirs in 
 Setter.all_printables deps_ref new_printables;;

 let registered_printers deps_ref= 
  let old_deps=(!deps_ref) in  
  let details = old_deps.Fw_mudolar_infrastructure_t.modularized_details in 
  let new_printers = List.flatten(Image.image (
  fun (_mn,details) -> Fw_module_details.registered_printers details
  ) details) in 
  Setter.registered_printers deps_ref new_printers;;

 let all fw_dets deps_ref= (
  modularized_details fw_dets deps_ref;
  order deps_ref;
  needed_dirs deps_ref;
  needed_libs deps_ref;
  all_subdirectories deps_ref;
  all_printables deps_ref;
  registered_printers deps_ref;
 ) ;; 

 let generic deps_ref
 new_details new_order
   new_needed_dirs new_needed_libs new_subdirectories 
   new_printables new_printers
  = (
  Setter.modularized_details deps_ref new_details;
  Setter.order deps_ref new_order;
  Setter.needed_dirs deps_ref new_needed_dirs;
  Setter.needed_libs deps_ref new_needed_libs;
  Setter.all_subdirectories deps_ref new_subdirectories;
  Setter.all_printables deps_ref new_printables;
  Setter.registered_printers deps_ref new_printers;
 ) ;; 
  

    end ;;
    
 

 module ReactOnReference = struct 
  
 let forget_modules deps_ref mods_to_be_erased= 
  let tester = (fun mn->not(List.mem mn mods_to_be_erased)) in 
  let new_details = List.filter 
  (fun (mn,_)->not(List.mem mn mods_to_be_erased))
  ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details)
  and new_order = List.filter_map (fun (mn,(before,after))->
    if not(tester mn) then None else 
    Some(mn,(before,List.filter tester after))  
    ) 
    ((!deps_ref).Fw_mudolar_infrastructure_t.order) 
  and new_printables = List.filter (fun middle->
    not(List.mem (Dfn_middle.to_module middle) mods_to_be_erased)) 
    ((!deps_ref).Fw_mudolar_infrastructure_t.all_printables) 
  and new_printers = List.filter (fun (_idx,printer_path)->
      not(List.mem (module_from_printer_path printer_path) mods_to_be_erased)) 
  ((!deps_ref).Fw_mudolar_infrastructure_t.registered_printers) in      
  (
    Setter.modularized_details deps_ref new_details;
    Setter.order deps_ref new_order;
    Default.needed_dirs deps_ref;
    Default.needed_libs deps_ref;
    Default.all_subdirectories deps_ref;
    Setter.all_printables deps_ref new_printables;
    Setter.registered_printers deps_ref new_printers;
  );; 

 let inspect_and_update extra deps_ref = 
  let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) 
  and ((_,_),changed_u_files,_) = extra in 
 let tempf = (
   fun old_pair ->
    let (mn,_) = old_pair in 
    let temp1 = List.filter (fun (rl,_)->
       (Dfn_rootless.to_module rl)= mn
      ) changed_u_files in 
 if temp1 <> []
 then (mn, Fw_module_details.compute_details_from_acolytes_list_for_one_module temp1)
    else old_pair
 ) in 
  let  new_details = Image.image tempf old_details in 
  let old_order = ((!deps_ref).Fw_mudolar_infrastructure_t.order) in 
  let new_order = compute_closest_order old_order new_details in 
  (
  Setter.modularized_details deps_ref new_details;
  Setter.order deps_ref new_order;
  Default.needed_dirs deps_ref;
  Default.needed_libs deps_ref;
  Default.all_subdirectories deps_ref;
  Default.all_printables deps_ref;
  Default.registered_printers deps_ref;
 ) ;; 

 let of_configuration = Default.all ;;
 
 let of_configuration_and_list = Default.all ;;
   
   
 let overwrite_file_if_it_exists extra new_fw_dets deps_ref =
 let old_details =  ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details)  in 
 let new_details = ( match extra with 
      None -> old_details 
      |Some(change) ->
        compute_new_details_in_stable_case new_fw_dets old_details [change]
        ) in 
 let old_order = ((!deps_ref).Fw_mudolar_infrastructure_t.order) in 
 let new_order = compute_closest_order old_order new_details in 
  (
  Setter.modularized_details deps_ref new_details;
  Setter.order deps_ref new_order;
  Default.needed_dirs deps_ref;
  Default.needed_libs deps_ref;
  Default.all_subdirectories deps_ref;
  Default.all_printables deps_ref;
  Default.registered_printers deps_ref;
 ) ;; 
  
 let plunge_fw_configuration deps_ref = (deps_ref:=starter) ;;
 let register_rootless_paths extra new_fw_dets deps_ref= 
   let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) in 
   let (_,novelties) = extra in 
   let old_mods = Image.image fst old_details in 
   let (overlapping,nonoverlapping) = List.partition (
      fun (rl,_) -> List.mem (Dfn_rootless.to_module rl) old_mods 
   ) novelties in 
   let tempf1 = (
     fun old_pair -> 
       let (mn,_) = old_pair in 
       let temp1 = List.filter_map (fun (rl,details2)->
          if (Dfn_rootless.to_module rl)= mn
          then Some(rl,Some(rl,details2))
          else None 
         ) overlapping in
       if temp1 <> []
       then (mn, Fw_module_details.recompute_details_for_module (Fwc_with_file_details.file_details new_fw_dets) mn temp1)
       else old_pair 
   ) in 
   let new_details = (Image.image tempf1 old_details)@
   (Fw_module_details.compute_details_from_acolytes_list_for_several_modules nonoverlapping) in 
   
 let old_order =  ((!deps_ref).Fw_mudolar_infrastructure_t.order) in 
 let novel_details = List_again.long_tail (List.length old_order) new_details in
 let novel_modules_in_order = Image.image fst (Fw_determine_order.main novel_details) in 
 let novel_details_in_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
     novel_details novel_modules_in_order in 
 let new_order = Fw_determine_order.compute_coatoms_and_ancestors_in_small_extension
      old_order novel_details_in_order in 
  let old_subdirectories = ((!deps_ref).Fw_mudolar_infrastructure_t.all_subdirectories) in 
 let (_,novelties) = extra in 
 let possibly_new = Ordered.sort Total_ordering.standard 
   (Image.image (fun (rl,_)->Dfn_rootless.to_subdirectory rl  ) novelties) in 
 let new_subdirectories = Ordered.merge Total_ordering.standard possibly_new old_subdirectories in 
  (
  Setter.modularized_details deps_ref new_details;
  Setter.order deps_ref new_order;
  Default.needed_dirs deps_ref;
  Default.needed_libs deps_ref;
  Setter.all_subdirectories deps_ref new_subdirectories;
  Default.all_printables deps_ref;
  Default.registered_printers deps_ref;
 ) ;;  


 let relocate_module_to extra new_fw_dets deps_ref= 
  let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) in 
  let new_details = compute_new_details_in_stable_case new_fw_dets old_details (fst extra) in 
  (
  Setter.modularized_details deps_ref new_details;
  (* field order does not need to be changed *)
  Default.needed_dirs deps_ref;
  (* field needed_libs does not need to be changed *)
  (* field all_subdirectories does not need to be changed *)
  (* field all_printables does not need to be changed *)
  (* field registered_printers does not need to be changed *)
 ) ;; 
   
 
   
 let remove_files extra new_fw_dets deps_ref
  = 
  let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) in 
  let new_details = compute_new_details_in_stable_case new_fw_dets old_details extra in 
  ( 
  Setter.modularized_details deps_ref new_details;
  Default.order deps_ref;
  Default.needed_dirs deps_ref;
  Default.needed_libs deps_ref;
  Default.all_subdirectories deps_ref;
  Default.all_printables deps_ref;
  Default.registered_printers deps_ref;
 ) ;; 


 let rename_module_on_filename_level_and_in_files extra new_fw_dets deps_ref triple
  = 
  let (old_mname,new_mname,_) = triple in
  let rap = (fun mn->if mn = old_mname then new_mname else mn) in 
  let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) in 
  let tempf = (
   fun old_pair -> 
     let (pre_mn,_) = old_pair in 
     let temp1 = List.filter (fun (rl,_)->
        (Dfn_rootless.to_module rl)= pre_mn
       ) (fst extra) in
     if temp1 <> []
     then let mn = rap pre_mn in 
          (mn, Fw_module_details.recompute_details_for_module (Fwc_with_file_details.file_details new_fw_dets) mn temp1)
     else old_pair 
 ) in 
 let new_details = Image.image tempf old_details in 
  let old_order = ((!deps_ref).Fw_mudolar_infrastructure_t.order) in 
  let new_order = Image.image (fun (mn2,(coat_mn2,ancestors_mn2)) ->
    (rap mn2,(Image.image rap coat_mn2,Image.image rap ancestors_mn2))
  ) old_order in 
  let old_needed_dirs = ((!deps_ref).Fw_mudolar_infrastructure_t.needed_dirs) in 
  let new_needed_dirs = Image.image (fun (mn2,dirs) -> (rap mn2,dirs)) old_needed_dirs in 
  let old_needed_libs = ((!deps_ref).Fw_mudolar_infrastructure_t.needed_libs) in 
  let new_needed_libs = Image.image (fun (mn2,libs) -> (rap mn2,libs)) old_needed_libs in 
  let old_printables =  ((!deps_ref).Fw_mudolar_infrastructure_t.all_printables) in 
  let rep = Dfn_middle.rename_module (old_mname,new_mname) in 
  let new_printables = Image.image rep old_printables in 
  let rup = (fun (idx,path)->
    (idx,replace_module_in_printer_path (old_mname,new_mname) path)) in
  let old_printers =  ((!deps_ref).Fw_mudolar_infrastructure_t.registered_printers) in 
  let new_printers = Image.image rup old_printers in 
  (
  Setter.modularized_details deps_ref new_details;
  Setter.order deps_ref new_order;
  Setter.needed_dirs deps_ref new_needed_dirs;
  Setter.needed_libs deps_ref new_needed_libs;
  (* field all_subdirectories does not need to be changed *)
  Setter.all_printables deps_ref new_printables;
  Setter.registered_printers deps_ref new_printers;
 ) ;; 


   
let rename_subdirectory_as extra new_fw_dets deps_ref sdir_pair = 
  let (old_sdir,new_sdir) = sdir_pair in 
  let s_new_sdir = Dfa_subdirectory.without_trailing_slash new_sdir in 
  let rep = Dfn_middle.rename_endsubdirectory (old_sdir,s_new_sdir) in 
  let rap = (fun sdir ->
    match Dfa_subdirectory.soak sdir_pair sdir with 
    None -> sdir 
    |Some new_sdir -> new_sdir   
  ) in 
  let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) in 
  let new_details = compute_new_details_in_stable_case new_fw_dets old_details (fst extra) in 
  let old_needed_dirs =  ((!deps_ref).Fw_mudolar_infrastructure_t.needed_dirs) in  
  let new_needed_dirs = Image.image (fun (mn,sdirs)->(mn,Image.image rap sdirs) ) old_needed_dirs in 
  let old_subdirectories =  ((!deps_ref).Fw_mudolar_infrastructure_t.all_subdirectories) in  
  let new_subdirectories = Image.image rap old_subdirectories in 
  let old_printables =  ((!deps_ref).Fw_mudolar_infrastructure_t.all_printables) in  
  let new_printables = Image.image rep old_printables in 
  ( 
  Setter.modularized_details deps_ref new_details;
  (* field order does not need to be changed *)
  Setter.needed_dirs deps_ref new_needed_dirs;
  (* field needed_libs does not need to be changed *)
  Setter.all_subdirectories deps_ref new_subdirectories;
  Setter.all_printables deps_ref new_printables;
  (* field registered_printers does not need to be changed *)
 ) ;; 

 let template fw_dets deps_ref
 new_details new_order
   new_needed_dirs new_needed_libs new_subdirectories new_printables new_printers
  = (
  Default.modularized_details fw_dets deps_ref;  
  Setter.modularized_details deps_ref new_details;
  Default.order deps_ref;
  Setter.order deps_ref new_order;
  Default.needed_dirs deps_ref;
  Setter.needed_dirs deps_ref new_needed_dirs;
  Default.needed_libs deps_ref;
  Setter.needed_libs deps_ref new_needed_libs;
  Default.all_subdirectories deps_ref;
  Setter.all_subdirectories deps_ref new_subdirectories;
  Default.all_printables deps_ref;
  Setter.all_printables deps_ref new_printables;
  Default.registered_printers deps_ref;
  Setter.registered_printers deps_ref new_printers;
 ) ;; 

 let replace_string extra new_fw_dets deps_ref
   = 
 let old_details = ((!deps_ref).Fw_mudolar_infrastructure_t.modularized_details) in 
 let new_details = compute_new_details_in_stable_case new_fw_dets old_details (fst extra) in 
 let old_order = ((!deps_ref).Fw_mudolar_infrastructure_t.order) in 
 let new_order = compute_closest_order old_order new_details in 
 ( 
 Setter.modularized_details deps_ref new_details;
 Setter.order deps_ref new_order;
 Default.needed_dirs deps_ref;
 Default.needed_libs deps_ref;
 Default.all_subdirectories deps_ref;
 Default.all_printables deps_ref;
 Default.registered_printers deps_ref;
) ;; 

  let replace_value = replace_string ;;

 end ;;  


end;;  

module ReactOnReference = Private.ReactOnReference ;;

let starter = Private.starter;;

 