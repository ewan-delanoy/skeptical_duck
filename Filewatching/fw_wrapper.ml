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

