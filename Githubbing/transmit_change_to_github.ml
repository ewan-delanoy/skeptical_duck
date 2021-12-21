(* 

#use"Githubbing/transmit_change_to_github.ml";;

*)

module Private = struct

let commands_for_backup config diff=
   if Dircopy_diff.is_empty diff
   then ([],[])
   else 
   let source_dir = config.Github_configuration_t.root 
   and destination_dir = config.Github_configuration_t.dir_for_backup in 
   let s_destination=Dfa_root.connectable_to_subpath destination_dir in
   let created_ones=Image.image Dfn_rootless.to_line (Dircopy_diff.recently_created diff) in
   let temp2=Option.filter_and_unpack
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
   ) config.Github_configuration_t.encoding_protected_files in 
   (temp3@temp4@temp5@temp8,temp6@temp7);;

let backup_with_message config  diff msg=
  let destination_dir = config.Github_configuration_t.dir_for_backup in 
  let (nongit_cmds,git_cmds)=commands_for_backup config diff in
  let s_destination=Dfa_root.connectable_to_subpath destination_dir in
  let _=Image.image Unix_command.uc nongit_cmds in
  let _=(
  if config.Github_configuration_t.gitpush_after_backup
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

let backup config diff opt_msg=
  if Dircopy_diff.is_empty diff
  then (print_string "No recent changes to commit ...";flush stdout) 
  else 
  let msg=(
   match opt_msg with
    None->Dircopy_diff.explain diff
   |Some(msg0)->msg0) in
  backup_with_message config diff msg;;
  
end ;; 

let backup config diff opt_msg=
  Private.backup config diff opt_msg;;

  
  