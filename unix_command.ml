(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;
exception Command_failed_just_now ;;

module Private = struct

let prefix_for_changing_directories = "cd ";;
let prefix_for_replacing_patterns   = "rp ";;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;
let hardcore_mode=ref(false);;

let command cmd=
   let cd_prefix =prefix_for_changing_directories 
   and rp_prefix =prefix_for_replacing_patterns in 
   if Supstring.begins_with cmd cd_prefix 
   then let  _=Sys.chdir(Cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else 
   if Supstring.begins_with cmd rp_prefix 
   then let  _=Sys.chdir(Cull_string.cobeginning (String.length rp_prefix) cmd) in 0
   else 
   Sys.command cmd;;


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

let debug_individual_uc (j,s) =
    let msg = "Trying command number "^(string_of_int j)^" : "^s^"\n" in 
    let _=(print_string msg;flush stdout) in
    let i = command s in 
    if i<>0 
    then raise(Command_failed_just_now)
    else (print_string "Successful.\n";flush stdout);;

let rec helper_for_debug_multiple_uc (j,l)=
   match l with 
   [] -> () 
   |cmd :: other_cmds ->
     let _ = debug_individual_uc (j,cmd) in 
     helper_for_debug_multiple_uc (j+1,other_cmds) ;; 


end;;

let cd dirname = (Private.prefix_for_changing_directories)^dirname;;

let rec conditional_multiple_uc commands=match commands with
  []->true
  |cmd1::other_commands ->
    if (Private.uc cmd1)=0
    then conditional_multiple_uc other_commands 
    else false;;
           
let debug_multiple_uc l = Private.helper_for_debug_multiple_uc (1,l);;           

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



           