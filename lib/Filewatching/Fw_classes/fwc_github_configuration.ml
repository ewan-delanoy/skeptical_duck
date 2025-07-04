(* 

#use"lib/Filewatching/Fw_classes/fwc_github_configuration.ml";;

*)

module Field = struct 

  let set_gitpush_after_backup fw new_gab = 
    Fwg_github_configuration.make 
     ~v_root:(Fwg_github_configuration.root fw)
     ~v_dir_for_backup:(Fwg_github_configuration.dir_for_backup fw)
     ~v_gitpush_after_backup:new_gab
     ~v_github_url:(Fwg_github_configuration.github_url fw)
     ~v_encoding_protected_files:(Fwg_github_configuration.encoding_protected_files fw) ;;

end ;;  


module Private = struct 


module Crobj = struct 
let salt = "Fwc_github_configuration_t." ;;
let label_for_root                               = salt ^ "root" ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;


let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj in 
  Fwg_github_configuration.make 
   ~v_root:(Dfa_root.of_concrete_object (g label_for_root))
   ~v_dir_for_backup:(Dfa_root.of_concrete_object (g label_for_dir_for_backup))
   ~v_gitpush_after_backup:(Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup))
   ~v_github_url:(Crobj_converter.string_of_concrete_object (g label_for_github_url))
   ~v_encoding_protected_files:(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files)) ;;
  ;;

let to_concrete_object fw = 
 let items =  
 [
   label_for_root, Dfa_root.to_concrete_object ( Fwg_github_configuration.root fw ) ;
   label_for_dir_for_backup, Dfa_root.to_concrete_object ( Fwg_github_configuration.dir_for_backup fw ) ;
   label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object ( Fwg_github_configuration.encoding_protected_files fw ) ;
   label_for_github_url, Crobj_converter.string_to_concrete_object ( Fwg_github_configuration.github_url fw ) ;
   label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object ( Fwg_github_configuration.gitpush_after_backup fw ) ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 


let commands_for_backup fw diff=
  if Dircopy_diff.is_empty diff
  then ([],[])
  else 
  let source_dir = Fwg_github_configuration.root fw 
  and destination_dir = Fwg_github_configuration.dir_for_backup fw in 
  let s_destination=Dfa_root.connectable_to_subpath destination_dir in
  let created_ones=Image.image Dfn_rootless.to_line (Dircopy_diff.recently_created diff) in
  let temp2=List.filter_map
  (fun fn->
    if String.contains fn '/'
    then let dn=Cull_string.before_rightmost fn '/' in
         Some("mkdir -p "^s_destination^dn)
    else None 
   ) created_ones in
  let temp3=Ordered.sort Total_ordering.silex_for_strings temp2 in
  let s_source=Dfa_root.connectable_to_subpath source_dir in
  let temp4=Image.image(
     fun fn->
     "cp "^s_source^fn^" "^s_destination^(Cull_string.before_rightmost fn '/')
  ) created_ones in
  let changed_ones=Image.image Dfn_rootless.to_line (Dircopy_diff.recently_changed diff) in
  let temp5=Image.image(
     fun fn->
     "cp "^s_source^fn^" "^s_destination^fn
  ) changed_ones in
  let temp6=Image.image(
     fun fn->
     "git -C "^s_destination^" add "^fn
  ) (created_ones@changed_ones) in
  let temp7=Image.image(
     fun rl->
     let fn = Dfn_rootless.to_line rl in 
     "git -C "^s_destination^" rm "^fn
  ) (Dircopy_diff.recently_deleted diff) in
  let temp8= Image.image (
    fun (replacer,replacee) ->
      let s_replacer = Dfn_rootless.to_line  replacer 
      and s_backup_dir = Dfa_root.connectable_to_subpath destination_dir in 
      let s_full_path = s_backup_dir^(Dfn_rootless.to_line replacee) in 
      Unix_command.prefix_for_replacing_patterns^s_replacer^" "^s_full_path
  ) (Fwg_github_configuration.encoding_protected_files fw) in 
  (temp3@temp4@temp5@temp8,temp6@temp7);;

let backup_with_message fw  diff msg=
 let destination_dir = Fwg_github_configuration.dir_for_backup fw in 
 let (nongit_cmds,git_cmds)=commands_for_backup fw diff in
 let s_destination=Dfa_root.connectable_to_subpath destination_dir in
 let _=Image.image Unix_command.uc nongit_cmds in
 let _=(
 if Fwg_github_configuration.gitpush_after_backup fw
 then let cwd=Sys.getcwd() in
      Image.image Unix_command.uc
      (
      [Unix_command.cd s_destination]@   
      git_cmds@   
      [
        "git -C "^s_destination^" commit -m \""^msg^"\"";
        "git -C "^s_destination^" push"
      ]@
      [Unix_command.cd cwd]
      ) 
 else let cwd=Sys.getcwd() in
      Image.image Unix_command.uc
      (
      [Unix_command.cd s_destination]@   
      git_cmds@   
      [
        "git -C "^s_destination^" commit -m \""^msg^"\""
      ]@
      [Unix_command.cd cwd]
      ) 
 ) in
 ();;

let backup fw diff opt_msg=
 if Dircopy_diff.is_empty diff
 then (print_string "No recent changes to commit ...";flush stdout) 
 else 
 let msg=(
  match opt_msg with
   None->Dircopy_diff.explain diff
  |Some(msg0)->msg0) in
 backup_with_message fw diff msg;;

end ;;


let backup = Private.backup ;;
let of_concrete_object = Private.Crobj.of_concrete_object ;;

let to_concrete_object = Private.Crobj.to_concrete_object ;;