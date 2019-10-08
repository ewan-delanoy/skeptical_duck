(*

#use"Filewatching/fw_wrapper.ml";;

*)

exception Register_rootless_path_exn of string;;

module Private = struct

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
     let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     mtime file;;


let recompute_all_info fw path =
     let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     (path,mtime file,Io.read_whole_file(Absolute_path.of_string file));;

let remove_watched_files fw rootless_paths =
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.watched_files = List.filter (fun (path,_,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.watched_files)  
   };;

let remove_special_watched_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.special_watched_files = List.filter (fun (path,_,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.special_watched_files)  
   };;


let forget_module fw mod_name =
   let the_files = Option.filter_and_unpack (
      fun (path,_,_)-> 
        if (Dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Fw_wrapper_t.watched_files in 
   remove_watched_files fw the_files;;

let forget fw x=
      if String.contains x '.'
      then remove_watched_files fw [Dfn_rootless.of_line x]
      else forget_module fw (Dfa_module.of_line(x));;


let register_rootless_path fw rootless_path= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
   let s_full_path = s_root^(Dfn_rootless.to_line rootless_path)  in 
   if not(Sys.file_exists s_full_path)
   then raise(Register_rootless_path_exn(s_full_path))
   else
    {
      fw with 
      Fw_wrapper_t.watched_files =  
        (fw.Fw_wrapper_t.watched_files)@[recompute_all_info fw rootless_path]  
    };;

let register_special_rootless_path fw rootless_path= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
   let s_full_path = s_root^(Dfn_rootless.to_line rootless_path)  in 
   if not(Sys.file_exists s_full_path)
   then raise(Register_rootless_path_exn(s_full_path))
   else
    {
      fw with 
      Fw_wrapper_t.special_watched_files =  
        (fw.Fw_wrapper_t.special_watched_files)@[recompute_all_info fw rootless_path]  
    };;



let relocate_watched_files_to fw rootless_paths new_subdir=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
    and s_subdir = Dfa_subdirectory.connectable_to_subpath new_subdir in 
    let displacements_to_be_made = Image.image (
      fun path->" mv "^s_root^(Dfn_rootless.to_line path)^" "^
      s_root^s_subdir 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc displacements_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.watched_files = Image.image (fun triple->
         let (path,_,content)=triple in 
         if(List.mem path rootless_paths) 
         then let new_path = Dfn_rootless.relocate_to path new_subdir in 
              (new_path,recompute_mtime fw new_path,content)
         else triple
      ) (fw.Fw_wrapper_t.watched_files)  
   };;

let relocate_module_to fw mod_name new_subdir=
   let the_files = Option.filter_and_unpack (
      fun (path,_,_)-> 
        if (Dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Fw_wrapper_t.watched_files in 
   relocate_watched_files_to fw the_files new_subdir;;

let helper_during_string_replacement fw (old_string,new_string) accu old_list=
    let new_list =Image.image (
       fun triple->
         let (old_path,_,old_content)=triple in 
         if not(Supstring.contains old_content old_string)
         then triple 
         else 
            let ap = Dfn_full.to_absolute_path (Dfn_join.root_to (Fw_wrapper_field.root fw) old_path) in 
            let _=(
             Replace_inside.replace_inside_file (old_string,new_string) ap;
             accu:=old_path::(!accu)
            ) in 
            recompute_all_info fw old_path 
         ) old_list in 
    (new_list,List.rev(!accu));;

let replace_string fw (old_string,new_string)=
    let ref_for_usual_ones=ref[] 
    and ref_for_special_ones=ref[] in 
    let (new_usual_files,changed_usual_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_usual_ones fw.Fw_wrapper_t.watched_files 
    and  (new_special_files,changed_special_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_special_ones fw.Fw_wrapper_t.special_watched_files   in 
    let new_fw ={
       fw with
       Fw_wrapper_t.watched_files         = new_usual_files ;
       Fw_wrapper_t.special_watched_files = new_special_files ;
    }  in 
    (new_fw,(changed_usual_files,changed_special_files));;         
       
let rename_value_inside_module fw (old_name,new_name) preceding_files rootless_path=
   let full_path = Dfn_join.root_to (Fw_wrapper_field.root fw) rootless_path in 
   let absolute_path=Dfn_full.to_absolute_path  full_path in 
   let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name absolute_path in 
   let new_watched_files =Image.image (
       fun triple->
         let (path,_,_)=triple in 
         if path = rootless_path 
         then recompute_all_info fw path
         else triple 
   ) (fw.Fw_wrapper_t.watched_files) in 
   {
      fw with 
       Fw_wrapper_t.watched_files = new_watched_files
   };;  


let helper1_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root triple=
   let (rootless_path,_,_)=triple in 
   if Dfn_rootless.to_subdirectory rootless_path = old_subdir 
   then triple
   else let new_rootless_path=Dfn_rootless.rename_subdirectory_as (old_subdir,new_subdir) rootless_path in
        let cmd=" mv "^s_root^(Dfn_rootless.to_line rootless_path)^" "^(Dfn_rootless.to_line new_rootless_path) in 
        let _=Unix_command.hardcore_uc cmd in 
        recompute_all_info fw new_rootless_path;;

let helper2_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root l_triples =
     Image.image (helper1_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root) l_triples;;

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw)  in 
   {
      fw with
      Fw_wrapper_t.watched_files = helper2_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root (fw.Fw_wrapper_t.watched_files)  ;
      Fw_wrapper_t.special_watched_files = helper2_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root (fw.Fw_wrapper_t.special_watched_files)  ;
   };;   

let helper1_during_inspection fw accu triple=
   let (rootless_path,old_mtime,_)=triple in 
   let new_mtime = recompute_mtime fw rootless_path in 
   if new_mtime <> old_mtime
   then let _=(accu:=rootless_path::(!accu)) in 
        recompute_all_info fw rootless_path
   else triple;;

let helper2_during_inspection fw accu l_triples =
   let new_l_triples = Image.image (helper1_during_inspection fw accu) l_triples in 
   (new_l_triples,List.rev(!accu));;

let inspect_and_update fw = 
    let ref_for_usual_ones=ref[] 
    and ref_for_special_ones=ref[] in 
    let (new_usual_files,changed_usual_files)=
        helper2_during_inspection fw ref_for_usual_ones fw.Fw_wrapper_t.watched_files 
    and  (new_special_files,changed_special_files)=
        helper2_during_inspection fw ref_for_special_ones fw.Fw_wrapper_t.special_watched_files   in 
    let new_fw ={
       fw with
       Fw_wrapper_t.watched_files         = new_usual_files ;
       Fw_wrapper_t.special_watched_files = new_special_files ;
    }  in 
    (new_fw,(changed_usual_files,changed_special_files));;         

let triple_of_concrete_object crobj=   
    let (arg1,arg2,arg3,_,_,_,_)=Concrete_object_field.unwrap_bounded_uple crobj 
    and us=Concrete_object_field.unwrap_string in
   (Dfn_rootless.of_concrete_object arg1,us arg2,us arg3);;     

let triple_to_concrete_object (rootless_path,mtime,content)=   
   Concrete_object_t.Uple
   [
    Dfn_rootless.to_concrete_object rootless_path;
    Concrete_object_t.String(mtime);
    Concrete_object_t.String(content);
   ];;     

let triplelist_of_concrete_object crobj = Concrete_object_field.to_list triple_of_concrete_object crobj;;
let triplelist_to_concrete_object l = Concrete_object_field.of_list triple_to_concrete_object l;;

 
let salt = "Fw_"^"wrapper_t.";;

let configuration_label         = salt ^ "configuration";;
let watched_files_label         = salt ^ "watched_files";;
let special_watched_files_label = salt ^ "special_watched_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_wrapper_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
      watched_files = triplelist_of_concrete_object(g watched_files_label);
      special_watched_files = triplelist_of_concrete_object(g special_watched_files_label);
   };; 

let to_concrete_object wr=
   let items= 
   [
    configuration_label, Fw_configuration.to_concrete_object wr.Fw_wrapper_t.configuration;
    watched_files_label, triplelist_to_concrete_object wr.Fw_wrapper_t.watched_files;
    special_watched_files_label, triplelist_to_concrete_object wr.Fw_wrapper_t.special_watched_files;
   ]  in
   Concrete_object_t.Record items;;

end;;

let of_concrete_object = Private.of_concrete_object;;

let empty_one config= {
   Fw_wrapper_t.configuration = config;
   watched_files = [];
   special_watched_files = [];
};; 

let forget = Private.forget;;

let register_rootless_path = Private.register_rootless_path;;

let relocate_module_to = Private.relocate_module_to;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let to_concrete_object = Private.to_concrete_object;;

