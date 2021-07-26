(*

#use"Text_editing/detect_printer_declaration_in_text.ml";;

*)

module Private = struct

let let_keyword = "let" ;;

let po_keyword  = "print_out" ;;

let equal_sign = "=" ;;

let blanks = [' ';'\n';'\r';'\t'];;

let after_blanks text =
  let n=String.length text in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get text (j-1)) blanks
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;

let detect_printer_declaration_at_index text idx =
  if not (Substring.is_a_substring_located_at let_keyword text idx )
  then None 
  else 
  match after_blanks text (idx+(String.length let_keyword)) with 
   None -> None 
   |Some(idx2) ->
  if not (Substring.is_a_substring_located_at po_keyword text idx2 )
  then None 
  else  
  match after_blanks text (idx+(String.length let_keyword)) with 
   None -> None 
  |Some(idx3) ->
  if not (Substring.is_a_substring_located_at equal_sign text idx3 )
  then None 
  else Some(idx2,idx3) ;;
  
let rec detect_printer_declaration_from_index text idx = 
   if idx>(String.length text)
   then None 
   else
   match  detect_printer_declaration_at_index text idx with 
   Some(idx2,idx3) -> Some(idx,idx2,idx3)
   |None -> detect_printer_declaration_from_index text (idx+1) ;;    

end ;;   

let detect text = Private.detect_printer_declaration_from_index text 0;;

