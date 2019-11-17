(* 

#use"Hex_analysis/hex_shell.ml";;

*)

let launch () = 
  let _= Hex_analysis.restart None  in 
  let _=(Hex_shell_internals.game_is_on:=true;
         print_string "Game started ...\n";
         Hex_shell_command.send_report()) in 
   while (!(Hex_shell_internals.game_is_on)) 
   do   
      let cmd = Hex_shell_command.of_string (read_line()) in 
      Hex_shell_command.execute cmd
   done;;

