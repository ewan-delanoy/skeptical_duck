(*

#use"lib/Job_Hunting/executable_jh.ml";;


*)



module Private = struct 

let write_days k = 
   if k=0 then "today" else 
   if k=1 then "in 1 day" else    
   "in "^(string_of_int k)^" days" ;;
   
let watcher = ref true ;;

let all_data () =   
   let msg_for_today = Job_hunting.data_for_today () in 
   let available_now = Job_hunting.available_now_ordered_by_company_name () in 
   let (available_and_not_isolated,available_and_isolated) = List.partition (
      fun (_company_name,(_date,has_multiple_applications,_remaining_before_release))-> 
          has_multiple_applications
   ) available_now in 
   let names_for_nonisolated = Image.image (
       fun (company_name,(_date,_has_multiple_applications,_remaining_before_release))-> company_name
   ) available_and_not_isolated in
   let avl_company_names = Image.image (
       fun (company_name,(_date,_has_multiple_applications,_remaining_before_release))-> company_name
   ) (Job_hunting.available_later_ordered_by_company_name()) in 
   let iav_company_names = Image.image (
       fun (company_name,(_date,_has_multiple_applications,_remaining_before_release))-> company_name
   ) available_and_isolated in 
   let selector_unavailable = (fun c->List.filter(fun company_name->String.get company_name 0=c) avl_company_names) in
   let selector_available_isolated = (fun c->List.filter(fun company_name->String.get company_name 0=c) iav_company_names) in 
   let available_later = List.filter (
     fun (_date,(_company_name,has_multiple_applications,_remaining_before_release))->has_multiple_applications
   ) (Job_hunting.available_later_ordered_by_date()) in 
   let ten_earliest = List_again.long_head 10 available_later in 
   let lines = Image.image (fun 
      (_date,(company_name,_has_multiple_applications,remaining_before_release))->
      company_name^" ( "^(write_days remaining_before_release)^" )") ten_earliest in 
  let msg_for_available_ones = "The following "^(string_of_int(List.length names_for_nonisolated))^" companies are currently available : \n\n"^
   (String.concat ", " names_for_nonisolated)^"\n\n"^
   "In addition to those non-isolated companies, "^(string_of_int(List.length(available_and_isolated)))^" isolated companies are also available."
  and  msg_for_more_detailed_info = 
  "The following companies will be released soon : \n\n"^
  (String.concat "\n" lines)
  and react_to_input=(fun s->
    let c=String.get s 0 in 
    if c='q' then let _=(watcher:=false) in "" else
    let selected1 = selector_unavailable c in 
    let msg_unavailable = (
    if selected1 = []
    then "\nThere are no unavailable company names starting with this letter."
    else "\nThe following companies are currently unavailable : \n\n"^
  (String.concat ", " selected1)^"\n\n") in 
    let selected2 = selector_available_isolated c in 
    let msg_available_isolated = (
    if selected2 = []
    then "\nThere are no isolated available company names starting with this letter."
    else "\nThe following companies are currently available and isolated : \n\n"^
    (String.concat ", " selected2)^"\n\n") in 
    msg_unavailable^msg_available_isolated
  ) in 
(msg_for_today^"\n\n"^msg_for_available_ones^"\n\n"^msg_for_more_detailed_info,react_to_input);;


let main _args = 
   let (initial_ann,react_to_input) = all_data () in 
   let _ = Basic.display_message initial_ann in 
   while !watcher do
      Basic.display_message (react_to_input(read_line()))  
   done;;

end ;;

let main = Private.main ;;


