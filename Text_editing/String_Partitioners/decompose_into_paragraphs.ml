(*

#use"Text_editing/String_Partitioners/decompose_into_paragraphs.ml";;

The convention is that a sequence of whitespaces introduces 
a new paragraph iff it contains at least two line breaks.

*)

module Private = struct

let contains_at_least_two_linebreaks s =
    List.length(Substring.occurrences_of_in "\n" s) >= 2;;
    
let rec iterator (dead,dying,to_be_treated) = 
    match to_be_treated with 
          [] -> let dead2=(
                   if dying=""
                   then dead
                  else  (dying,"") :: dead
                ) in 
                List.rev (dead2)
        | (txt,delim) :: others ->
        if contains_at_least_two_linebreaks delim 
        then iterator ((dying^txt,delim)::dead,"",others)
        else iterator (dead,dying^txt^delim,others);;

let from_whitespaces_to_paragraph_breaks l= 
    iterator ([],"",l) ;;
   
let main s = from_whitespaces_to_paragraph_breaks 
   (Whitespace_separated.ws s);;

(*

main "abc\ndef gh \n \n ij \r klm \n\n nop q\nr\ns";;

*)   

      end ;;

let dec =  Private.main ;;     
