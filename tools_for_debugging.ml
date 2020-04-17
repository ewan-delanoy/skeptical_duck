(*

#use"tools_for_debugging.ml";;

*)

let detect_exception f x = try (fun _->false)(f x) with _->true ;;

let extract_from_iteration f v0=
   let rec tempf = (fun (j,v)->
     if detect_exception f v 
     then (j,v)
     else tempf(j+1,f v)
   ) in 
   tempf (0,v0);;

let extract_from_list f l =
   let temp1 = Ennig.index_everything l in 
   let tempf= (fun (j,elt)->detect_exception f elt) in 
   Listennou.force_find tempf temp1 ;; 