(*

#use"Filewatching/fw_wrapper.ml";;


*)

exception Register_rootless_path_exn of string;;

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

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
     let m_time =mtime s_full_path  
     and content =Io.read_whole_file (Absolute_path.of_string s_full_path) in
    {
      fw with 
      Fw_wrapper_t.watched_files =  
        (fw.Fw_wrapper_t.watched_files)@[rootless_path,m_time,content]  
    };;

let register_special_rootless_path fw rootless_path= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
   let s_full_path = s_root^(Dfn_rootless.to_line rootless_path)  in 
   if not(Sys.file_exists s_full_path)
   then raise(Register_rootless_path_exn(s_full_path))
   else
     let m_time =mtime s_full_path  
     and content =Io.read_whole_file (Absolute_path.of_string s_full_path) in
    {
      fw with 
      Fw_wrapper_t.special_watched_files =  
        (fw.Fw_wrapper_t.special_watched_files)@[rootless_path,m_time,content]  
    };;

(*

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
         let (path,_,_)=triple in 
         if(List.mem path rootless_paths) 
         then let the_module=Dfb.to_module path in 
              let new_path = s_root^s_subdir^
         else 
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

*)   