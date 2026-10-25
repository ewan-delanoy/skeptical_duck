(*

#use"lib/Job_Hunting/executable_jh.ml";;


*)



module Private = struct 

let write_days k = 
   if k=0 then "today" else 
   if k=1 then "in 1 day" else    
   "in "^(string_of_int k)^" days" ;;
   
let all_data () =   
   let ((nonisolated_ones,nbr_of_isolated),selector) = Job_hunting.usual () 
   and pairs = Job_hunting.available_later () in 
   let lines = Image.image (fun (company_name,nbr_of_days)->company_name^" ( "^(write_days nbr_of_days)^" )") pairs in 
  let msg_for_available_ones = "The following "^(string_of_int(List.length nonisolated_ones))^" companies are currently available : \n"^
   (String.concat ", " nonisolated_ones)^"\n\n"^
   (string_of_int(nbr_of_isolated))^" isolated companies are also available."
  and  msg_for_releasable_ones = 
  "The following companies will be released soon : \n"^
  (String.concat "\n" lines)
  and react_to_input=(fun s->
    let c=String.get s 0 in 
    let selected = selector c in 
    if selected = []
    then "\nThere are no company names starting with this letter."
    else "\nThe following companies are currently unavailable : \n\n"^
  (String.concat ", " selected)^"\n\n") in 
(msg_for_available_ones^"\n\n"^msg_for_releasable_ones,react_to_input);;


let main _args = 
   let (initial_ann,react_to_input) = all_data () in 
   let _ = Basic.announce initial_ann in 
   while true do
      Basic.announce (react_to_input(read_line()))  
   done;;

end ;;

let main = Private.main ;;


