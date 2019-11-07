(* 

#use"Hex_analysis/hex_shell.ml";;

*)

let launch () = 
  let _=(
       Hex_persistent.initialize_all_data_if_necessary ();
       Hex_shell_internals.game_is_on:=true
   ) in 
   while (!(Hex_shell_internals.game_is_on)) 
   do   
      let cmd = Hex_shell_command.of_string (read_line()) in 
      Hex_shell_command.execute cmd
   done;;

   