(*

#use"Text_editing/remove_blank_lines_after_percents.ml";;

*)

module Private = struct 

  let remove_first_blankets text =
    let indexed_lines = Lines_in_string.core text in 
    let m = List.length indexed_lines in 
    let nth_line = (fun k->List.assoc k indexed_lines) in 
    let bad_linedices = List.filter (fun j->
        (Supstring.begins_with (nth_line (j-1)) "%") && ((nth_line j)="")
      ) (Ennig.ennig 2 m) in 
   let good_lines = Option.filter_and_unpack (fun (linedex,line)->
     if List.mem linedex bad_linedices then None else Some line
    ) indexed_lines  in 
   String.concat "\n" good_lines ;;
  
  let remove_all_blankets text =
    let rec tempf = (fun (previous,next)->
       if next = previous then previous else tempf (next,remove_first_blankets next)
      ) in 
    tempf(text,remove_first_blankets text);;  

end ;;

let in_string = Private.remove_all_blankets ;;

let in_file fn=
    let old_text=Io.read_whole_file fn in
    let new_text=in_string old_text  in
    Io.overwrite_with fn new_text;; 

