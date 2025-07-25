(*

#use"lib/Filewatching/Fw_classes/fwc_with_file_details.ml";;

*)

type t = Fwg_with_file_details.t ;;

module Inherited = struct 

   module Ancestry = Fwc_with_archives.Inherited ;;

   module Parent = Fwc_with_archives;;
   let parent = Fwg_with_file_details.parent ;;


   let archived_files fw = Parent.archived_files (parent fw) ;;
   let check_that_no_change_has_occurred fw = Parent.check_that_no_change_has_occurred(parent fw)  ;;  

   let ignored_files fw = Ancestry.ignored_files (parent fw) ;;
   let ignored_subdirectories fw = Ancestry.ignored_subdirectories (parent fw) ;;

   let  latest_changes fw = Parent.latest_changes(parent fw)  ;;  

   let  noncompilable_files fw = Parent.noncompilable_files(parent fw)  ;;  

   let root fw = Ancestry.root (parent fw) ;;


   let test_equality fw1 fw2 = 
      (
        Ancestry.test_equality (parent fw1) (parent fw2)
      )
      @
      (
        List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld)
        [
          "file_details",(
            (Fwg_with_file_details.file_details fw1)=
          (Fwg_with_file_details.file_details fw2))
        ]
      ) ;;

   let test_for_admissibility fw = Ancestry.test_for_admissibility (parent fw) ;;
  
   let to_fw_configuration fw = Ancestry.to_fw_configuration (parent fw) ;;
    
   let to_fw_with_archives fw = parent fw ;;
   let  usual_compilable_files fw = Fwc_with_archives.usual_compilable_files(parent fw)  ;; 


end ;;   


module Crobj = struct 
   let salt = "Fwc_with_small_details." ;;
   let label_for_parent = salt ^ "parent" ;;
   let label_for_file_details  = salt ^ "file_details" ;;
       
       
   let of_concrete_object ccrt_obj = 
     let g=Concrete_object.get_record ccrt_obj in 
     Fwg_with_file_details.make 
     (Fwc_with_archives.Crobj.of_concrete_object (g label_for_parent))
     (Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Fw_file_details.of_concrete_object (g label_for_file_details))
     ;;
       
   let to_concrete_object fw = 
     let items =  
     [
          label_for_parent, Fwc_with_archives.Crobj.to_concrete_object ( Fwg_with_file_details.parent fw ) ;
          label_for_file_details, 
          Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Fw_file_details.to_concrete_object
           (Fwg_with_file_details.file_details fw ) ;
     ] in 
     Concrete_object_t.Record items ;;
       
       
end;; 


module Private = struct

  



   (* Start of level 2 *)
   let parent fw = Fwg_with_file_details.parent fw ;;
   (* End of level 2 *)
 



   (* Start of level 1 *)
 
   
   let constructor mother =
      Fwg_with_file_details.make 
       mother  (Fwc_with_archives.compute_all_small_details mother) ;;
   let root fw     = Inherited.root fw ;;  
   let file_details fw = Fwg_with_file_details.file_details fw ;;  
   
   (* End of level 1 *)
   

 
   let forget_modules fw mod_names =
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,removed_files) = Fwc_with_archives.forget_modules old_parent mod_names in
      (
         Fwg_with_file_details.make 
         new_parent 
         (List.filter (
            fun (rl,_)->not(List.mem (Dfn_rootless.to_module rl) mod_names)
          ) old_details),removed_files) ;;  
   
   let inspect_and_update fw  =
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,changed_files,(a_files,u_files)) = Fwc_with_archives.inspect_and_update old_parent in
      let changed_details_ref = ref [] in 
      let new_small_details = Image.image (
         fun old_pair->
          let rl = fst old_pair in
          if List.mem rl changed_files 
          then let new_pair = (rl,Fwc_with_archives.compute_small_details_on_one_file new_parent rl) in 
               let _ = (changed_details_ref:=new_pair::(!changed_details_ref) ) in 
               new_pair 
          else old_pair  
       ) old_details in 
      (Fwg_with_file_details.make 
      new_parent 
      new_small_details,
      ((a_files,u_files),!changed_details_ref,changed_files));;
   
   let of_configuration config =   
       let mother = Fwc_with_archives.of_configuration config in 
       constructor mother ;;
   
   let of_configuration_and_list (config,files)=   
      let mother = Fwc_with_archives.of_configuration_and_list config files  in 
      constructor mother ;;
   
   let overwrite_file_if_it_exists fw (rootless,new_content) = 
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,change_made) = 
         Fwc_with_archives.overwrite_file_if_it_exists 
           old_parent rootless new_content in 
      let accu = ref None in      
      let new_fw = (
      if change_made 
      then 
         let new_small_details = Image.image (
            fun old_pair->
            let rl = fst old_pair in
            if rl  = rootless 
            then let new_pair = (rl,Fwc_with_archives.compute_small_details_on_one_file new_parent rl) in 
                 let _= (accu:=Some(rl,Some(new_pair))) in 
                 new_pair
            else old_pair  
         ) old_details in 
         Fwg_with_file_details.make 
         new_parent 
          new_small_details
      else fw ) in (new_fw,!accu);;           
   
   
   let register_rootless_paths fw rootless_paths= 
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let new_parent = Fwc_with_archives.register_rootless_paths 
           old_parent rootless_paths in 
      let new_details =    (Image.image (fun rl->
         (rl,Fwc_with_archives.compute_small_details_on_one_file new_parent rl)) 
         rootless_paths) in 
      ( Fwg_with_file_details.make 
      new_parent 
      (old_details @ new_details),
      (Fwc_with_archives.partition_for_singles new_parent rootless_paths,new_details) ) ;;    
   
   let relocate_module_to fw (mod_name,new_subdir)=
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,replacements) = Fwc_with_archives.relocate_module_to 
           old_parent mod_name new_subdir in 
      let accu = ref [] in      
      let new_small_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if (Dfn_rootless.to_module rl) = mod_name 
         then let new_rl = Dfn_rootless.relocate_to rl new_subdir in 
              let new_pair = (new_rl,snd old_pair) in 
              let _ = (accu := (rl,Some new_pair) :: (!accu)) in
              new_pair
         else old_pair        
      ) old_details in 
      let new_fw = Fwg_with_file_details.make 
      new_parent 
      new_small_details  in 
      (new_fw,(!accu,replacements));;  
   

      let remove_files fw removed_rootless_paths=   
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let new_parent = Fwc_with_archives.remove_files 
         old_parent removed_rootless_paths in 
      ( Fwg_with_file_details.make 
      new_parent 
       (List.filter (
            fun (rl,_)->not(List.mem rl removed_rootless_paths)
         ) old_details),
         Image.image (fun rl->(rl,None)) removed_rootless_paths) ;;
   
   
      let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten) =
         let old_parent = parent fw 
         and old_details = file_details fw  in 
         let (new_parent,file_renamings,changed_u_files,changed_a_files) = 
             Fwc_with_archives.rename_module_on_filename_level_and_in_files 
           old_parent (old_module,new_module,files_to_be_rewritten) in 
         let optional_new_rl = (fun rl->
            match List.assoc_opt rl file_renamings with 
               Some(new_rl) -> Some(new_rl) 
               | None ->
                  if List.mem rl changed_u_files
                  then Some rl 
                  else None
         ) in 
         let accu = ref [] in 
         let new_details = Image.image (
            fun old_pair->
              let rl = fst old_pair in 
              match optional_new_rl rl with 
               Some(new_rl) -> 
              let new_pair = (new_rl,Fwc_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
             | None -> old_pair  
          ) old_details in    
         (Fwg_with_file_details.make 
         new_parent 
         new_details,
         (List.rev(!accu),(file_renamings,changed_u_files@changed_a_files))) ;;  
   
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=   
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,original_reps) = Fwc_with_archives.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
      let accu = ref [] in
      let new_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         match Dfn_rootless.soak (old_subdir,new_subdir) rl with 
         (Some new_rl) -> 
            let new_pair = (new_rl,Fwc_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
                let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                new_pair 
         | None -> old_pair        
      ) old_details in 
      (Fwg_with_file_details.make 
      new_parent 
      new_details,(List.rev(!accu),original_reps)) ;;   
   
   let replace_string fw (replacee,replacer)=
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,(changed_a_files,changed_u_files)) = Fwc_with_archives.replace_string old_parent (replacee,replacer) in
      let accu = ref [] in 
      let new_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_u_files
         then let new_pair = (rl,Fwc_with_archives.compute_small_details_on_one_file new_parent rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
         else old_pair  
      ) old_details in
      (Fwg_with_file_details.make 
      new_parent 
      new_details,(List.rev(!accu),changed_a_files@changed_u_files));;   
   
   let replace_value fw ((preceding_files,path),(replacee,pre_replacer)) =
      let old_parent = parent fw 
      and old_details = file_details fw  in 
      let (new_parent,(all_changes,changed_files)) = 
           Fwc_with_archives.replace_value 
            old_parent (preceding_files,path) (replacee,pre_replacer) in
      let accu = ref [] in 
      let new_details = Image.image (
         fun old_pair->
            let rl = fst old_pair in
            if List.mem rl changed_files
            then let new_pair = (rl,Fwc_with_archives.compute_small_details_on_one_file new_parent rl) in 
                    let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                    new_pair 
            else old_pair  
         ) old_details in      
      (Fwg_with_file_details.make 
      new_parent 
      new_details,(List.rev(!accu),all_changes));;     
   
      let check_that_no_change_has_occurred fw =
         Fwc_with_archives.check_that_no_change_has_occurred (parent fw) ;; 
   
      let latest_changes fw = Fwc_with_archives.latest_changes (parent fw) ;;   
         
   end ;;
   
   
   
   let forget_modules = Private.forget_modules ;;
   let inspect_and_update = Private.inspect_and_update;;
   let of_configuration = Private.of_configuration ;;
   let of_configuration_and_list = Private.of_configuration_and_list ;;
   let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
   let plunge_fw_configuration config= Fwg_with_file_details.make (Fwc_with_archives.plunge_fw_configuration config) [] ;; 
   let register_rootless_paths = Private.register_rootless_paths;;
   let relocate_module_to = Private.relocate_module_to;;
   let remove_files = Private.remove_files;;
   let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;
   let rename_subdirectory_as = Private.rename_subdirectory_as;;
   let replace_string = Private.replace_string;;
   let replace_value = Private.replace_value;;
   let file_details = Private.file_details ;;
   let usual_compilable_files fw = Fwc_with_archives.usual_compilable_files (Private.parent fw) ;;
    