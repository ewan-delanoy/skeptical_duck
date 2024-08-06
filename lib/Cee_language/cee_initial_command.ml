(*

#use"lib/Cee_language/cee_initial_command.ml";;

*)


let make raw_command = 
   let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " raw_command 1) in 
   let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " raw_command (i1+4)) in
   let short_filename = Cull_string.interval raw_command (i1+4) (i2-1) in 
   {
     Cee_initial_command_t.short_path = Cull_string.coending 2 short_filename ;
     ending = Cull_string.ending 2 short_filename ;
     core_of_command =Cull_string.beginning (i1-1) raw_command ;
   }  ;;

