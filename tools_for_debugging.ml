(*

#use"tools_for_debugging.ml";;

*)

let extract_from_list f l =
   let temp1 = Ennig.index_everything l in 
   let tempf= (fun (j,elt)->try (fun _->false)(f elt) with _->true) in 
   Listennou.force_find tempf temp1 ;; 