(*

#use"Text_editing/remove_blank_lines_after_percents.ml";;

*)

module Private = struct 
 
  let is_a_lowercase_letter c=
     let i = int_of_char c in 
      (97<=i)&&(i<=122);;

  let is_an_uppercase_letter c=
      let i = int_of_char c in 
       (65<=i)&&(i<=90);;    

  let is_a_letter c = (is_a_lowercase_letter c) || (is_an_uppercase_letter c) ;; 
  let is_not_a_letter c = not(is_a_letter c);;
  let has_no_letter line= List.for_all is_not_a_letter (Strung.explode line);;


  let remove_first_blankets text =
    let indexed_lines = Lines_in_string.core text in 
    let m = List.length indexed_lines in 
    let nth_line = (fun k->List.assoc k indexed_lines) in 
    let line_is_empty = (fun j->(nth_line j)="")
    and line_starts_with_percent = (fun j->Supstring.begins_with (nth_line j) "%") 
    and line_is_boink = (fun j->has_no_letter(nth_line j)) in 
    let is_bad1 = (fun j-> if j>=m then false else (line_starts_with_percent(j+1)) && (line_is_empty j))
    and is_bad2 =  (fun j->if j<=1 then false else (line_starts_with_percent(j-1)) && (line_is_empty j)) 
    and is_bad3 =  (fun j->if j<=1 then false else (line_starts_with_percent(j-1)) && (line_is_boink j)) in 
    let bad_linedices = List.filter (fun j->
        (is_bad1 j)||(is_bad2 j)||(is_bad3 j)
      ) (Ennig.ennig 1 m) in 
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

