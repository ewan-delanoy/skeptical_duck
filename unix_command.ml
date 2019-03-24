(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


module Private = struct

let prefix_for_changing_directories = "cd ";;

end;;

let command cmd=
   let cd_prefix = Private.prefix_for_changing_directories in 
   if Supstring.begins_with cmd cd_prefix 
   then let  _=Sys.chdir(Cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else Sys.command cmd;;

let cd dirname = (Private.prefix_for_changing_directories)^dirname;;

exception Command_failed of string;;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;


let hardcore_uc s=
   let i=command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(if (!remember_commands_mode) then accu:=s::(!accu)) in 
        i;;

let hardcore_verbose_uc s=
   let _=(print_string ("Executing "^s^"\n\n");flush stdout) in
   hardcore_uc s;;

let mild_uc s=
   let i=command s in
   let _=(
   if i<>0
   then (print_string ("Failed during "^s^"\n");flush stdout)
   else (if (!remember_commands_mode) then accu:=s::(!accu))
   ) in
   i;;

let hardcore_mode=ref(false);;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else mild_uc s;;

let rec conditional_multiple_uc commands=match commands with
  []->true
  |cmd1::other_commands ->
    if (uc cmd1)=0
    then conditional_multiple_uc other_commands 
    else false;;
           