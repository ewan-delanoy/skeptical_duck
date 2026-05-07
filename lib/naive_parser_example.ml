(*

#use"lib/naive_parser_example.ml";;

*)

module Private = struct 

let condition_on_character f = Naive_parser_t.NP(fun text idx ->
    if idx > (String.length text)
    then None 
    else let c=String.get text (idx-1) in 
         if f c 
         then Some(c,idx+1)
         else None   
    ) ;;

let uppercase_character = condition_on_character (
  fun c->
    let i=int_of_char c in 
    (65<=i)&&(i<=90)
) ;;



end ;;  

let digit =
   Naive_parser_t.NP(
    fun text idx ->
      if idx > (String.length text)
      then None 
      else let c = String.get text (idx-1) in 
           let i = int_of_char c in 
           if (48<=i) &&(i<=57)
           then Some(c,idx+1)
           else None
   ) ;;

let fixed_string substr =
   Naive_parser_t.NP(
    fun text idx ->
      if Substring.is_a_substring_located_at substr text idx 
      then Some((),idx+(String.length substr)) 
      else None 
   ) ;;

let uppercase_character = Private.uppercase_character ;;   