(*

#use"Filewatching/fw_with_small_details.ml";;

*)


module Private = struct

   (* Start of level 2 *)
   let parent fw = fw.Fw_with_small_details_t.parent ;;
   (* End of level 2 *)
 
   (* Start of level 1 *)
 
   let configuration fw = Fw_with_archives.configuration (parent fw) ;;
   let constructor mother =
     {
        Fw_with_small_details_t.parent = mother ;
        small_details_in_files = Fw_with_archives.compute_all_small_details mother;
     } ;;  
   let root fw     = Fw_with_archives.root (parent fw) ;;  
   let small_details_in_files fw = fw.Fw_with_small_details_t.small_details_in_files ;;  
   
   (* End of level 1 *)
   
   (* Start of crobj level *)
 
   let cr_of_pair_list f l= Crobj_converter_combinator.of_pair_list  Dfn_rootless.to_concrete_object f l;;
   let cr_to_pair_list f crobj= Crobj_converter_combinator.to_pair_list  Dfn_rootless.of_concrete_object f crobj;;
      
   let salt = "Fw_"^"with_small_details_t.";;
   
   let parent_label                  = salt ^ "parent";;
   let small_details_in_files_label  = salt ^ "small_details_in_files";;
   
   
   let of_concrete_object ccrt_obj = 
      let g=Concrete_object.get_record ccrt_obj in
      {
         Fw_with_small_details_t.parent = Fw_with_archives.of_concrete_object(g parent_label);
         small_details_in_files = cr_to_pair_list Fw_file_small_details.of_concrete_object (g small_details_in_files_label);
 
      };; 
   
 
   let to_concrete_object fw=
      let items= 
      [
        parent_label, Fw_with_archives.to_concrete_object fw.Fw_with_small_details_t.parent;
        small_details_in_files_label, cr_of_pair_list  Fw_file_small_details.to_concrete_object (fw.Fw_with_small_details_t.small_details_in_files);
      
      ]  in
      Concrete_object_t.Record items;;
   
 
   (* End of crobj level *)
 
   let forget_modules fw mod_names =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,removed_files) = Fw_with_archives.forget_modules old_parent mod_names in
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = List.filter (
           fun (rl,_)->not(List.mem (Dfn_rootless.to_module rl) mod_names)
         ) old_details;
      },removed_files) ;;  
   
   let inspect_and_update fw  =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,changed_files,(a_files,u_files)) = Fw_with_archives.inspect_and_update old_parent in
      let changed_details_ref = ref [] in 
      let new_small_details = Image.image (
         fun old_pair->
          let rl = fst old_pair in
          if List.mem rl changed_files 
          then let new_pair = (rl,Fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
               let _ = (changed_details_ref:=new_pair::(!changed_details_ref) ) in 
               new_pair 
          else old_pair  
       ) old_details in 
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = new_small_details;
      },
      ((a_files,u_files),!changed_details_ref,changed_files));;
   
   let of_configuration config =   
       let mother = Fw_with_archives.of_configuration config in 
       constructor mother ;;
   
   let of_configuration_and_list (config,files)=   
      let mother = Fw_with_archives.of_configuration_and_list config files  in 
      constructor mother ;;
   
   let overwrite_file_if_it_exists fw (rootless,new_content) = 
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,change_made) = 
         Fw_with_archives.overwrite_file_if_it_exists 
           old_parent rootless new_content in 
      let accu = ref None in      
      let new_fw = (
      if change_made 
      then 
         let new_small_details = Image.image (
            fun old_pair->
            let rl = fst old_pair in
            if rl  = rootless 
            then let new_pair = (rl,Fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
                 let _= (accu:=Some(rl,Some(new_pair))) in 
                 new_pair
            else old_pair  
         ) old_details in 
         {
            Fw_with_small_details_t.parent = new_parent ;
            small_details_in_files = new_small_details;
         }
      else fw ) in (new_fw,!accu);;           
   
   
   let register_rootless_paths fw rootless_paths= 
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let new_parent = Fw_with_archives.register_rootless_paths 
           old_parent rootless_paths in 
      let new_details =    (Image.image (fun rl->
         (rl,Fw_with_archives.compute_small_details_on_one_file new_parent rl)) 
         rootless_paths) in 
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = old_details @ new_details ;
      },
      (Fw_with_archives.partition_for_singles new_parent rootless_paths,new_details) ) ;;    
   
   let relocate_module_to fw (mod_name,new_subdir)=
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,replacements) = Fw_with_archives.relocate_module_to 
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
      let new_fw = {
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = new_small_details;
      } in 
      (new_fw,(!accu,replacements));;  
   
   let remove_files fw removed_rootless_paths=   
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let new_parent = Fw_with_archives.remove_files 
         old_parent removed_rootless_paths in 
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = List.filter (
            fun (rl,_)->not(List.mem rl removed_rootless_paths)
         ) old_details;
      },Image.image (fun rl->(rl,None)) removed_rootless_paths) ;;
   
   
      let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten) =
         let old_parent = parent fw 
         and old_details = small_details_in_files fw  in 
         let (new_parent,file_renamings,changed_u_files,changed_a_files) = 
             Fw_with_archives.rename_module_on_filename_level_and_in_files 
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
              let new_pair = (new_rl,Fw_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
             | None -> old_pair  
          ) old_details in    
         ({
            Fw_with_small_details_t.parent = new_parent ;
            small_details_in_files = new_details;
          },(List.rev(!accu),(file_renamings,changed_u_files@changed_a_files))) ;;  
   
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=   
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,original_reps) = Fw_with_archives.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
      let accu = ref [] in
      let new_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         match Dfn_rootless.soak (old_subdir,new_subdir) rl with 
         (Some new_rl) -> 
            let new_pair = (new_rl,Fw_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
                let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                new_pair 
         | None -> old_pair        
      ) old_details in 
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = new_details ;
      },(List.rev(!accu),original_reps)) ;;   
   
   let replace_string fw (replacee,replacer)=
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,(changed_a_files,changed_u_files)) = Fw_with_archives.replace_string old_parent (replacee,replacer) in
      let accu = ref [] in 
      let new_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_u_files
         then let new_pair = (rl,Fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
         else old_pair  
      ) old_details in
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = new_details;
      },(List.rev(!accu),changed_a_files@changed_u_files));;   
   
   let replace_value fw ((preceding_files,path),(replacee,pre_replacer)) =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,(all_changes,changed_files)) = 
           Fw_with_archives.replace_value 
            old_parent (preceding_files,path) (replacee,pre_replacer) in
      let accu = ref [] in 
      let new_details = Image.image (
         fun old_pair->
            let rl = fst old_pair in
            if List.mem rl changed_files
            then let new_pair = (rl,Fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
                    let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                    new_pair 
            else old_pair  
         ) old_details in      
      ({
         Fw_with_small_details_t.parent = new_parent ;
         small_details_in_files = new_details;
      },(List.rev(!accu),all_changes));;     
   
      let check_that_no_change_has_occurred fw =
         Fw_with_archives.check_that_no_change_has_occurred (parent fw) ;; 
   
      let latest_changes fw = Fw_with_archives.latest_changes (parent fw) ;;   
         
   end ;;
   
   let check_that_no_change_has_occurred = Private.check_that_no_change_has_occurred;;
   let configuration = Private.configuration ;;
   let empty_one config= { Fw_with_small_details_t.parent = Fw_with_archives.empty_one config;small_details_in_files = [];} ;; 
   let forget_modules = Private.forget_modules ;;
   let inspect_and_update = Private.inspect_and_update;;
   let latest_changes = Private.latest_changes ;;
   let noncompilable_files fw = Fw_with_archives.noncompilable_files (Private.parent fw) ;;
   let of_concrete_object = Private.of_concrete_object ;;
   let of_configuration = Private.of_configuration ;;
   let of_configuration_and_list = Private.of_configuration_and_list ;;
   let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
   let register_rootless_paths = Private.register_rootless_paths;;
   let relocate_module_to = Private.relocate_module_to;;
   let remove_files = Private.remove_files;;
   let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;
   let rename_subdirectory_as = Private.rename_subdirectory_as;;
   let replace_string = Private.replace_string;;
   let replace_value = Private.replace_value;;
   let root = Private.root ;;
   let small_details_in_files = Private.small_details_in_files ;;
   let to_concrete_object = Private.to_concrete_object ;;
   let usual_compilable_files fw = Fw_with_archives.usual_compilable_files (Private.parent fw) ;;
   