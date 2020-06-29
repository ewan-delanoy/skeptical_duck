(*

#use "Text_editing/Text_lengthening/sabbrex_stringify_on_standard.ml";;

*)


let apply line= 
   let cmd = Parse_sabbrex_command.parse line in 
   let (Unexpected_change_after_update_t.Ucau changes) = Sabbrex_apply_on_standard.apply cmd in 
   let temp1=Image.imagination (fun (x,old_y,new_y)-> x^" : "^old_y^" ->"^new_y ) changes in 
   let answer=(
      if changes=[] then "" else 
      "\n\n"^(String.concat "\n" temp1)^"\n\n"
   ) in 
   answer;;


