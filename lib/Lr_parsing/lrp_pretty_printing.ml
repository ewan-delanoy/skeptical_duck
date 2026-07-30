(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_pretty_printing.ml";;

*)

open Lrp_types ;;

module Private = struct 

let on_index names_for_states idx0=
 snd(List.find (fun ((idx,_),_)->idx=idx0) names_for_states) ;;   

let on_action names_for_states = function 
  (Shift j)->  on_index names_for_states j 
  |Reduce(Prod(a,b)) -> a^" -> "^(String.concat "" b) 
  |Accept -> "Accept" ;;

let on_action_transition names_for_states (symb,act) = 
   symb^"\226\165\184 \226\157\170 "^(on_action names_for_states act)^" \226\157\171";;

let on_action_transitions names_for_states l = 
    String.concat "   " (Image.image (on_action_transition names_for_states) l);;   

let on_action_indexed_pair names_for_states (idx,transitions) = 
   (on_index names_for_states idx)^" : "^(on_action_transitions names_for_states transitions);;

let on_action_data names_for_states l =
   "Action table :\n\n"^
   (String.concat "\n" (Image.image (on_action_indexed_pair names_for_states) l))
  ;;  

let on_goto_transition names_for_states (symb,state) = 
   symb^"\226\165\184 \226\157\170 "^(on_index names_for_states state)^" \226\157\171";;

let on_goto_transitions names_for_states l = 
    String.concat "   " (Image.image (on_goto_transition names_for_states) l);;   

let on_goto_indexed_pair names_for_states (idx,transitions) = 
   (on_index names_for_states idx)^" : "^(on_goto_transitions names_for_states transitions);;

let on_goto_data names_for_states l =
   let effective_l = List.filter (fun (_idx,transitions)->transitions<>[]) l in 
   "Goto table :\n\n"^
   (String.concat "\n" (Image.image (on_goto_indexed_pair names_for_states) effective_l))
  ;;  

let on_table names_for_states tbl =
   (on_action_data names_for_states tbl.action_data)^
   "\n\n\n"^
   (on_goto_data names_for_states tbl.goto_data)^
   "\n\n\n" ;; 
 
let on_action_conflict names_for_states ((idx,mover),acts) =
    (on_index names_for_states idx)^" \226\157\159 "^mover^" \226\165\184 "^
    (String.concat " \194\166 "(Image.image (on_action names_for_states) acts));;

let on_action_conflicts names_for_states conflicts =
   if conflicts = [] then "" else 
   "Action conflicts : \n\n"^
    (String.concat "\n"(Image.image (on_action_conflict names_for_states) conflicts));;    

let on_goto_conflict names_for_states ((idx,mover),destinations) =
    (on_index names_for_states idx)^" \226\157\159 "^mover^" \226\165\184 "^
    (String.concat " \194\166 "(Image.image (on_index names_for_states) destinations));;

let on_goto_conflicts names_for_states conflicts =
   if conflicts = [] then "" else 
   "Goto conflicts : \n\n"^
    (String.concat "\n"(Image.image (on_goto_conflict names_for_states) conflicts));;    

let on_both_conflicts names_for_states (action_conflicts,goto_conflicts) =
   let between = (if (action_conflicts<>[])&&(goto_conflicts<>[]) then "\n\n\n" else "") in  
   (on_action_conflicts names_for_states action_conflicts)^between^
   (on_goto_conflicts names_for_states goto_conflicts)^"\n\n" ;;
      

end ;;   

let on_action = Private.on_action ;;

let on_both_conflicts = Private.on_both_conflicts ;;

let on_index = Private.on_index ;;
let on_table = Private.on_table ;;