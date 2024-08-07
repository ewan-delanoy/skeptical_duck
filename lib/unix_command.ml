(*

Wrapper on the Sys dot command function.

#use"lib/unix_command.ml";;

*)


exception Command_failed of string;;
exception Command_failed_just_now ;;

module Private = struct

let prefix_for_changing_directories         = "cd ";;
let prefix_for_replacing_patterns           = "rp ";;
let prefix_for_reverse_replacing_patterns   = "rvp ";;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;
let hardcore_mode=ref(false);;

let command cmd=
   let cd_prefix =prefix_for_changing_directories 
   and rp_prefix =prefix_for_replacing_patterns 
   and rvp_prefix =prefix_for_reverse_replacing_patterns in 
   if String.starts_with ~prefix:cd_prefix cmd 
   then let  _=Sys.chdir(Cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else 
   if String.starts_with ~prefix:rp_prefix cmd 
   then let  _= Compact_replacer.execute(Cull_string.cobeginning (String.length rp_prefix) cmd) in 0 
   else 
   if String.starts_with ~prefix:rvp_prefix cmd 
   then let  _= Compact_replacer.reverse_execute(Cull_string.cobeginning (String.length rvp_prefix) cmd) in 0 
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
     if not(String.starts_with ~prefix:new_location full_path) then false else 
     let naked_name=Cull_string.two_sided_cutting (new_location,"") full_path in 
     not(String.contains naked_name '/') 
   ) in 
   if destination_equals_source 
   then None 
   else Some("mv "^full_path^" "^new_location);;

let prefix_for_replacing_patterns           = Private.prefix_for_replacing_patterns;;
let prefix_for_reverse_replacing_patterns   = Private.prefix_for_reverse_replacing_patterns;;


let uc = Private.uc;;



           