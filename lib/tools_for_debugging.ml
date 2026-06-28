(*

#use"lib/tools_for_debugging.ml";;

*)

let detect_exception f x = try (fun _->false)(f x) with _->true ;;

let extract_from_fold_left f starter pushers=
   let rec tempf = (fun (count_of_pushers_already_applied,current_starter,current_pushers)->
    match current_pushers with 
    [] -> failwith("There is nothing to debug.")
    |pusher :: others ->
     if detect_exception (f current_starter) pusher
     then (count_of_pushers_already_applied,pusher)
     else tempf(count_of_pushers_already_applied+1,f current_starter pusher,others)
   ) in 
   tempf (0,starter,pushers);;

let extract_from_iteration f v0=
   let rec tempf = (fun (j,v)->
     if detect_exception f v 
     then (j,v)
     else tempf(j+1,f v)
   ) in 
   tempf (0,v0);;

let extract_from_list f l =
   let temp1 = Int_range.index_everything l in 
   let tempf= (fun (_j,elt)->detect_exception f elt) in 
   List.find tempf temp1 ;; 