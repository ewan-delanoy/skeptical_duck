(*

#use"lib/Filewatching/gw_with_small_details.ml";;

*)

(* Beginning of POR(Polymorphic Ocaml Record)-related code *)

module Background = struct 

let fw_space = Por_space_example.filewatching ;; 

let small_details_in_files_field =
   {Por_field_t.field_name = "small_details_in_files";
   field_type = "(Dfn_rootless_t.t * Fw_file_small_details_t.t) list";
   var_name = "small_details"; default_value = "[]";
   crobj_converters =
    Some
     ("Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Fw_file_small_details.of_concrete_object",
      "Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Fw_file_small_details.to_concrete_object");
   dependency_data = None;} ;; 

let gw_with_small_details_extension =
   {Por_subclass_t.subclass_name = "gw_with_small_details";
   subclass_fields = [small_details_in_files_field];
   parent = Some "gw_with_archives"; 
   extensions_leading_here = [];
   has_restriction = false; 
   has_constructor = false} ;;

Por_space.add_extension fw_space "gw_with_archives" gw_with_small_details_extension;;


end ;;    

(* End of POR(Polymorphic Ocaml Record)-related code *)


module Private = struct

   (* Start of level 2 *)
   let parent fw = Gw_poly.parent fw ;;
   (* End of level 2 *)
 
   (* Start of level 1 *)
 
   
   let constructor mother =
     Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
       mother  
       ~small_details_in_files:(Gw_with_archives.compute_all_small_details mother) ;;
   let root fw     = Gw_poly.root fw ;;  
   let small_details_in_files fw = Gw_poly.small_details_in_files fw ;;  
   
   (* End of level 1 *)
   

 
   let forget_modules fw mod_names =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,removed_files) = Gw_with_archives.forget_modules old_parent mod_names in
      (
         Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
         new_parent 
         ~small_details_in_files:(List.filter (
            fun (rl,_)->not(List.mem (Dfn_rootless.to_module rl) mod_names)
          ) old_details),removed_files) ;;  
   
   let inspect_and_update fw  =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,changed_files,(a_files,u_files)) = Gw_with_archives.inspect_and_update old_parent in
      let changed_details_ref = ref [] in 
      let new_small_details = Image.image (
         fun old_pair->
          let rl = fst old_pair in
          if List.mem rl changed_files 
          then let new_pair = (rl,Gw_with_archives.compute_small_details_on_one_file new_parent rl) in 
               let _ = (changed_details_ref:=new_pair::(!changed_details_ref) ) in 
               new_pair 
          else old_pair  
       ) old_details in 
      (Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files:new_small_details,
      ((a_files,u_files),!changed_details_ref,changed_files));;
   
   let of_configuration config =   
       let mother = Gw_with_archives.of_configuration config in 
       constructor mother ;;
   
   let of_configuration_and_list (config,files)=   
      let mother = Gw_with_archives.of_configuration_and_list config files  in 
      constructor mother ;;
   
   let overwrite_file_if_it_exists fw (rootless,new_content) = 
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,change_made) = 
         Gw_with_archives.overwrite_file_if_it_exists 
           old_parent rootless new_content in 
      let accu = ref None in      
      let new_fw = (
      if change_made 
      then 
         let new_small_details = Image.image (
            fun old_pair->
            let rl = fst old_pair in
            if rl  = rootless 
            then let new_pair = (rl,Gw_with_archives.compute_small_details_on_one_file new_parent rl) in 
                 let _= (accu:=Some(rl,Some(new_pair))) in 
                 new_pair
            else old_pair  
         ) old_details in 
         Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
         new_parent 
          ~small_details_in_files:new_small_details
      else fw ) in (new_fw,!accu);;           
   
   
   let register_rootless_paths fw rootless_paths= 
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let new_parent = Gw_with_archives.register_rootless_paths 
           old_parent rootless_paths in 
      let new_details =    (Image.image (fun rl->
         (rl,Gw_with_archives.compute_small_details_on_one_file new_parent rl)) 
         rootless_paths) in 
      ( Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files:(old_details @ new_details),
      (Gw_with_archives.partition_for_singles new_parent rootless_paths,new_details) ) ;;    
   
   let relocate_module_to fw (mod_name,new_subdir)=
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,replacements) = Gw_with_archives.relocate_module_to 
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
      let new_fw = Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files:new_small_details  in 
      (new_fw,(!accu,replacements));;  
   
   let remove_files fw removed_rootless_paths=   
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let new_parent = Gw_with_archives.remove_files 
         old_parent removed_rootless_paths in 
      ( Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files: (List.filter (
            fun (rl,_)->not(List.mem rl removed_rootless_paths)
         ) old_details),
         Image.image (fun rl->(rl,None)) removed_rootless_paths) ;;
   
   
      let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten) =
         let old_parent = parent fw 
         and old_details = small_details_in_files fw  in 
         let (new_parent,file_renamings,changed_u_files,changed_a_files) = 
             Gw_with_archives.rename_module_on_filename_level_and_in_files 
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
              let new_pair = (new_rl,Gw_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
             | None -> old_pair  
          ) old_details in    
         (Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
         new_parent 
         ~small_details_in_files:new_details,
         (List.rev(!accu),(file_renamings,changed_u_files@changed_a_files))) ;;  
   
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=   
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,original_reps) = Gw_with_archives.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
      let accu = ref [] in
      let new_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         match Dfn_rootless.soak (old_subdir,new_subdir) rl with 
         (Some new_rl) -> 
            let new_pair = (new_rl,Gw_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
                let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                new_pair 
         | None -> old_pair        
      ) old_details in 
      (Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files:new_details,(List.rev(!accu),original_reps)) ;;   
   
   let replace_string fw (replacee,replacer)=
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,(changed_a_files,changed_u_files)) = Gw_with_archives.replace_string old_parent (replacee,replacer) in
      let accu = ref [] in 
      let new_details = Image.image (
         fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_u_files
         then let new_pair = (rl,Gw_with_archives.compute_small_details_on_one_file new_parent rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
         else old_pair  
      ) old_details in
      (Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files:new_details,(List.rev(!accu),changed_a_files@changed_u_files));;   
   
   let replace_value fw ((preceding_files,path),(replacee,pre_replacer)) =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,(all_changes,changed_files)) = 
           Gw_with_archives.replace_value 
            old_parent (preceding_files,path) (replacee,pre_replacer) in
      let accu = ref [] in 
      let new_details = Image.image (
         fun old_pair->
            let rl = fst old_pair in
            if List.mem rl changed_files
            then let new_pair = (rl,Gw_with_archives.compute_small_details_on_one_file new_parent rl) in 
                    let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                    new_pair 
            else old_pair  
         ) old_details in      
      (Gw_poly.extend_gw_with_archives_to_gw_with_small_details 
      new_parent 
      ~small_details_in_files:new_details,(List.rev(!accu),all_changes));;     
   
      let check_that_no_change_has_occurred fw =
         Gw_with_archives.check_that_no_change_has_occurred (parent fw) ;; 
   
      let latest_changes fw = Gw_with_archives.latest_changes (parent fw) ;;   
         
   end ;;
   
   
   
   let forget_modules = Private.forget_modules ;;
   let inspect_and_update = Private.inspect_and_update;;
   let of_configuration = Private.of_configuration ;;
   let of_configuration_and_list = Private.of_configuration_and_list ;;
   let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
   let plunge_gw_configuration config= Gw_poly.extend_gw_with_archives_to_gw_with_small_details (Gw_with_archives.plunge_fw_configuration config) ~small_details_in_files:[] ;; 
   let register_rootless_paths = Private.register_rootless_paths;;
   let relocate_module_to = Private.relocate_module_to;;
   let remove_files = Private.remove_files;;
   let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;
   let rename_subdirectory_as = Private.rename_subdirectory_as;;
   let replace_string = Private.replace_string;;
   let replace_value = Private.replace_value;;
   let small_details_in_files = Private.small_details_in_files ;;
   let usual_compilable_files fw = Gw_with_archives.usual_compilable_files (Private.parent fw) ;;
