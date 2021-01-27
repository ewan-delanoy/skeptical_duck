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

exception Overlapping_occurrences of string ;;
exception Unwrap_text_exn of string;;
exception Unwrap_delim_exn of string;;


module Private = struct 
  
  module Constant = struct 
    
    let check_for_overlapping_occurrences pattern txt indices=
      let p = String.length pattern in 
      let temp1 = Listennou.universal_delta_list indices in 
      match Option.seek (fun (i,j)->j-i<p) temp1 with 
      None -> ()
      |Some(i0,j0) -> raise(Overlapping_occurrences(Cull_string.interval txt i0 (j0+p-1)));;

    let main pattern txt =
        let indices = Substring.occurrences_of_in pattern txt in 
        if indices = [] then None else 
        let _= check_for_overlapping_occurrences pattern txt indices in 
        let p = String.length pattern and n = String.length txt 
        and imax=List.hd(List.rev indices) in 
        let temp1 = Listennou.universal_delta_list ((1-p)::indices) in 
        let itv =(fun (i,j)->Cull_string.interval txt (i+p) (j-1)) in 
        Some(Image.image itv temp1,itv (imax,n+1)) ;;

    (*    
      main "abc" "12abc3456abcabc7abc8abc9012345abc";;    
      main "abc" "12abc3456abcabc7abc8abc9012345abc6";;
      main "abc" "abc12abc3456abcabc7abc8abc9012345abc";;    
      main "abc" "abc12abc3456abcabc7abc8abc9012345abc6";;
      main "abcab" "123abcababcab456";; 
      main "abcab" "123abcabcab456";;    
    *)
  end ;;   

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

let wrt_constant = Private.Constant.main ;;
let wrt_paragraphs = Private.Paragraphs.main ;;
let wrt_whitespace = Private.Whitespace.main ;;