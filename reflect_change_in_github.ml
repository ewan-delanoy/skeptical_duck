
(* 

#use"reflect_change_in_github.ml";;

*)

let commands_for_backup (source_dir,destination_dir) diff=
   if Dircopy_diff.is_empty diff
   then ([],[])
   else 
   let s_destination=Dfa_root.connectable_to_subpath destination_dir in
   let created_ones=Dircopy_diff.recently_created diff in
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
   let changed_ones=Dircopy_diff.recently_changed diff in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp6=Image.image(
      fun fn->
      "git add "^fn
   ) (created_ones@changed_ones) in
   let temp7=Image.image(
      fun fn->
      "git rm "^fn
   ) (Dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5,temp6@temp7);;

let backup_with_message (source_dir,destination_dir,p_after_b) diff msg=
  let (nongit_cmds,git_cmds)=commands_for_backup (source_dir,destination_dir)  diff in
  let s_destination=Dfa_root.connectable_to_subpath destination_dir in
  let _=Image.image Unix_command.uc nongit_cmds in
  let _=(
  if p_after_b
  then let cwd=Sys.getcwd() in
       Image.image Unix_command.uc
       (
       [Unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ]@
       [Unix_command.cd cwd]
       ) 
  else let cwd=Sys.getcwd() in
       Image.image Unix_command.uc
       (
       [Unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\""
       ]@
       [Unix_command.cd cwd]
       ) 
  ) in
  ();;

let backup (source_dir,destination_dir,p_after_b) diff opt=
  let msg=(
   match opt with
    None->Dircopy_diff.explain diff
   |Some(msg0)->msg0) in
  backup_with_message (source_dir,destination_dir,p_after_b) diff msg;;
  

