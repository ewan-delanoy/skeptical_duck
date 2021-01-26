(*

#use"Text_editing/partition_string.ml";;

The wrt_whistepace function is basically just the Str.full_split map, with 
a little bit of standardization (if the string starts with
whitespace we artificially insert a dummy empty text at the beginning,
and if the string ends with non-whitespace we artificially a dummy
empty whitespace sequence at the end).

In wrt_paragraphs, the convention is that a sequence of whitespaces introduces 
a new paragraph iff it contains at least two line breaks.

*)

exception Unwrap_text_exn of string;;
exception Unwrap_delim_exn of string;;


module Private = struct 

  module Whitespace = struct

    let has_text_at_beginning l= match l with 
        [] -> false
        |elt::others ->
            (match elt with  
              Str.Delim(_) -> false
              |Str.Text(_) -> true
            );;
    
    let force_text_at_beginning l= 
        if has_text_at_beginning l 
        then l
        else (Str.Text ""):: l;;   
    
    let has_delim_at_end l= match List.rev l with 
    [] -> false
    |elt::others ->
        (match elt with  
          Str.Delim(_) -> true
          |Str.Text(_) -> false
        );;
    
    let force_delim_at_end l= 
        if has_delim_at_end l 
        then l
        else List.rev((Str.Delim "")::(List.rev l));;       
    
    let standardize_beginning_and_end l=
       force_text_at_beginning (force_delim_at_end l);;
    
    let unwrap_text = function 
     Str.Delim(d) -> raise(Unwrap_text_exn(d))
    |Str.Text(txt) -> txt ;;
    
    let unwrap_delim = function 
     Str.Delim(d) -> d
    |Str.Text(txt) -> raise(Unwrap_delim_exn(txt));;
     
    
    let main s=
      let temp1 = Str.full_split (Str.regexp "[ \r\t\n]+") s in 
      let temp2 = standardize_beginning_and_end temp1 in 
      let n = (List.length temp2)/2 
      and elt = (fun j->List.nth temp2 (j-1)) in 
      Ennig.doyle (fun k->(unwrap_text(elt (2*k-1)),unwrap_delim(elt (2*k)))) 1 n;;
    
    (*
    
    main "abc";;
    main "abc\n\tdef";;
    main "abc\n\tdef ghij";;
    main "abc\n\tdef ghij\r";;
    main "\rabc";;
    main "\tabc\n\tdef";;
    main "\nabc\n\tdef ghij";;
    main "\rabc\n\tdef ghij\r";;
    
    *)
    
    end ;; 


    module Paragraphs = struct

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
         (Whitespace.main s);;
      
      (*
      
      main "abc\ndef gh \n \n ij \r klm \n\n nop q\nr\ns";;
      
      *)   
      
      end ;;
      

end ;;  

let wrt_paragraphs = Private.Paragraphs.main ;;
let wrt_whitespace = Private.Whitespace.main ;;