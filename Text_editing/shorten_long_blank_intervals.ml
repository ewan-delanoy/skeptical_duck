(*

#use"Text_editing/shorten_long_blank_intervals.ml";;

*)

module Private = struct 

let in_string text =
  let temp1 = Str.full_split (Str.regexp "[ \t\n\r]+") text in 
  let temp2 = Image.image (
     function 
      |Str.Text(t) -> t 
      |Str.Delim(d) ->
        let m = String.length d in 
        let num_of_linebreaks = 
          List.length(
            List.filter (fun k->String.get d k='\n') 
              (Ennig.ennig 0 (m-1))
          ) in 
        if num_of_linebreaks > 4
        then "\n\n"
        else d    
  ) temp1 in 
  String.concat "" temp2 ;;

end ;;


let in_file file = 
  let old_text = Io.read_whole_file file in
  let new_text = Private.in_string old_text  in
  Io.overwrite_with file new_text ;;  

let in_string = Private.in_string ;;  

(* in_string "1\n2\n3\n\t\n\r\n\r\n4\n5\n6\n7" ;; *)  

