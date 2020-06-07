(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;

module Private = struct

let prefix_for_changing_directories = "cd ";;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;
let hardcore_mode=ref(false);;

let command cmd=
   let cd_prefix =prefix_for_changing_directories in 
   if Supstring.begins_with cmd cd_prefix 
   then let  _=Sys.chdir(Cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else Sys.command cmd;;


let mild_uc s=
   let i=command s in
   let _=(
   if i<>0
   then (print_string ("Failed during "^s^"\n");flush stdout)
   else (if (!remember_commands_mode) 
               then accu:=s::(!accu))
   ) in
   i;;

let hardcore_uc s=
   let i=command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(if (!remember_commands_mode) 
               then accu:=s::(!accu)) in 
        i;;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else mild_uc s;;



end;;

let cd dirname = (Private.prefix_for_changing_directories)^dirname;;

let rec conditional_multiple_uc commands=match commands with
  []->true
  |cmd1::other_commands ->
    if (Private.uc cmd1)=0
    then conditional_multiple_uc other_commands 
    else false;;
           

let hardcore_uc = Private.hardcore_uc ;;

let mv full_path new_location =
   let destination_equals_source=(
     if not(Supstring.begins_with full_path new_location) then false else 
     let naked_name=Cull_string.two_sided_cutting (new_location,"") full_path in 
     not(String.contains naked_name '/') 
   ) in 
   if destination_equals_source 
   then None 
   else Some("mv "^full_path^" "^new_location);;



let uc = Private.uc;;



           